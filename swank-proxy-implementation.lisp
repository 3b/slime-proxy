(in-package #:swank)

;;import symbols from swank-proxy that we use extensively
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(swank-proxy:proxy-eval
            swank-proxy:proxy-create-channel
            swank-proxy:proxy-channel
            swank-proxy:proxy-listener-channel
            swank-proxy:channel-target
            swank-proxy:start-swank-proxy-server
            swank-proxy:*swank-proxy-port*
            swank-proxy:start-websockets-proxy-server
            swank-proxy:proxy-send-to-client)
          :swank))

(defclass proxy-channel (channel)
  ;; THREAD slot is a kludge
  ((thread :initarg :thread :initform (current-thread) :accessor channel-thread)
   (target :initarg :target :initform nil :accessor channel-target
           :documentation "The target for messages delivered through this
           proxy channel. "))
  (:documentation "Subclass of the main slime channel class used for
  slime-proxy."))

(defclass proxy-listener-channel (proxy-channel)
  ())

(defgeneric proxy-eval (op proxy-target continuation &rest args)
  (:documentation "The beautiful generic function at the heart of
slime-proxy.  Used to evaluate a particular operation OP targetting
a particulary thing, proxy-target.

Unless this function returns :async, then continuation will be
evaluated by PROXY-EVAL-FOR-EMACS."))

(defmethod proxy-eval ((op t) (proxy-target t) cont &rest args)
  ;; by default, simply call the continuation and return :async
  (format t "unknown proxy-eval command ~s or proxy target ~s~%" (cons op args) proxy-target)
  #++(when cont
       (funcall cont nil nil))
  :async
  #+nil
  :pass
  (format nil "unknown proxy-eval command ~s or proxy target ~s~%" op proxy-target))

(defmacro define-proxy-fun (name target (&rest args) &body body)
  "Defines a method for proxy-eval with NAME and TARGET as eql
specializers for the OP and PROXY-TARGET arguments, respectively.  The
body of the method is BODY, with the verbatim symbols op, target, and
continuation bound appropriately, and the ARGS passed in used as the
lambda-list to destructure whatever remaining parameters are passed to
proxy-eval. "
  (let ((rest (gensym)))
    `(defmethod proxy-eval ((op (eql ',name)) (target (eql ',target)) continuation &rest ,rest)
       (destructuring-bind (,args) ,rest
         ,@body))))

(defgeneric proxy-eval-form (form target continuation)
  (:documentation ""))

(defmethod  proxy-eval-form (form target continuation)
  (funcall 'proxy-eval (car form) target continuation
           (mapcar 'eval (cdr form))))

(defun proxy-eval-for-emacs (form channel thread buffer-package id)
  "Binds *BUFFER-PACKAGE* to BUFFER-PACKAGE and proxy-evaluates FORM.
Return the result to the continuation ID.  Errors are trapped and
invoke our debugger.

Analagous to EVAL-FOR-EMACS, but instead of using EVAL to evaluate
form, uses PROXY-EVAL-FORM"
  (declare (optimize (debug 3)))
  (let* ((b (guess-buffer-package buffer-package))
         (brt (guess-buffer-readtable buffer-package))
         (pc (cons id *pending-continuations*))
         (conn *emacs-connection*))
    ;; fixme: make sure that we are binding the proper
    ;; specials. these specials were determined by guess-and-check
    (macrolet ((with-dynamic-bindings-for-proxy-eval (ignored &body body)
                 (declare (ignore ignored))
                 `(let ((*buffer-package* b)
                        (*buffer-readtable* brt)
                        (*pending-continuations* pc))
                    (check-type *buffer-package* package)
                    (check-type *buffer-readtable* readtable)
                    (with-connection (conn)
                      ,@body))))
      (flet ((cont (ok result)
               ;; fixme what about the not-okay case?
               (with-dynamic-bindings-for-proxy-eval ()
                 (run-hook *pre-reply-hook*)
                                        ;(format t "About to send to emacs: ~A~%" result)
                 (send-to-emacs `(:return ,thread
                                          ,(if ok
                                               `(:ok ,result)
                                               `(:abort ,(if (stringp result)
                                                             result
                                                             "proxy: non-string error")))
                                          ,id)))))
        (let (ok result)
          (unwind-protect
               (with-dynamic-bindings-for-proxy-eval ()
                 ;; APPLY would be cleaner than EVAL.
                 ;; (setq result (apply (car form) (cdr form)))
                 (setq result
                       (with-slime-interrupts (proxy-eval-form form (channel-target channel)
                                                               #'cont)))
                 (setq ok t)
                 (when (eql result :pass)
                   (setf result
                         (eval-for-emacs  form *buffer-package* id))))

            (when (not (eq result :async))
              (cont ok result))))))))

(defvar *proxy-cmd*
  "Used for debugging purposes.")

;;; All slime-proxy events are sent through the :proxy method, bundled
;;; with a particular command and its arguments
(define-channel-method :proxy ((channel proxy-channel) args)
  (setf *proxy-cmd* (list channel args))
  #+nil(format t "proxy ~s~%" (list channel args))
  (case (car args)
    (:emacs-rex
     (destructuring-bind (form package thread id &rest r) (cdr args)
       (declare (ignore r))
       ;;(format t "form ~s~% package ~s~% id ~S~%" form package id)
       (proxy-eval-for-emacs form channel thread package id)
       #++(let ((swank-backend::*proxy-interfaces* (make-hash-table)))
            (eval-for-emacs form package id))))))


;; SPAWN-PROXY-THREAD and CREATE-PROXY-LISTENER set up the swank-proxy
;; thread that listens in on SWANK events and
(defgeneric proxy-create-channel (target &key remote)
  (:documentation "Returns an instance of a proxy-channel connected to
the given remote instance.

fixme: this function has a very tentative interface"))

(defmethod proxy-create-channel (target &key remote)
  (make-instance 'proxy-listener-channel
                 :target target))

(defslimefun create-proxy-listener (remote target)
  ;; fixme: move most of this into proxy-create-channel
  (when (not (use-threads-p))
    (error "SLIME-PROXY requires a multi-threaded lisp."))

  (let* ((pkg *package*)
         (conn *emacs-connection*))
    (multiple-value-bind (thread channel)
        (start-swank-proxy-server remote target conn :kill-existing nil)
      (with-slots (id) channel
        (list id
              (thread-id thread)
              (package-name pkg)
              (package-string-for-prompt pkg))))))



(defvar *swank-proxy-thread* nil
  "Thread executing the swank proxy event-loop.")

(defvar *swank-proxy-channel* nil
  "Channel used to communicate between slime and swank.")

(defvar *connection-channels* (make-hash-table)
  "Store a list of channels we should clean up in
 *connection-closed-hook* when a connection goes away")

(defun kill-connection-channels (connection)
  (let ((channels (gethash connection *connection-channels*)))
    (when channels
      (remhash connection *connection-channels*)
      (loop for channel in channels
           for thread = (channel-thread channel)
           do (when (bordeaux-threads:thread-alive-p thread)
                (bordeaux-threads:destroy-thread thread))
           (when (and (boundp '*swank-proxy-thread*)
                      (eq *swank-proxy-thread* thread))
             ;; should this unbind instead?
             ;; (is it even used anywhere for that matter?)
             (setf *swank-proxy-thread* nil))))))

(push 'kill-connection-channels swank::*connection-closed-hook*)

#++
(maphash (lambda (k v) (kill-connection-channels k)) *connection-channels*)

(defun start-swank-proxy-server (remote-channel target emacs-connection &key kill-existing (port *swank-proxy-port*))
  "Spawns all the necessary threads to connect emacs up to a proxy
backend.  Returns the thread of the swank proxy server "
  (declare (optimize (debug 3)))
  (with-connection (emacs-connection)
    (macrolet ((maybe-kill (special)
                 `(progn
                    (when (and ,special (not (bordeaux-threads:thread-alive-p ,special)))
                      (setf ,special nil))
                    (when (and kill-existing ,special)
                      (bordeaux-threads:destroy-thread ,special)
                      (setf ,special nil))))
               (maybe-setf (special value)
                 `(if ,special
                      ,special
                      (setf ,special ,value))))

      (maybe-kill *swank-proxy-thread*)

      ;; first spawn the websockets threads
      (start-websockets-proxy-server :kill-existing kill-existing :port port)

      ;; now we create a channel unless one already exists
      (let ((channel (maybe-setf *swank-proxy-channel*
                                 (proxy-create-channel (intern (string-upcase target) :keyword)
                                                       :remote remote-channel))))
        (maybe-setf *swank-proxy-thread*
                    (bordeaux-threads:make-thread
                     (lambda ()
                       (unwind-protect
                            (run-swank-proxy-loop channel emacs-connection)
                         (when (eql *swank-proxy-channel* channel)
                           (setf *swank-proxy-channel* nil))))
                     :name "swank-proxy-thread"))
        (setf (channel-thread channel) *swank-proxy-thread*)
        (push channel (gethash emacs-connection *connection-channels* nil))
        (values *swank-proxy-thread* channel)))))

(defun run-swank-proxy-loop  (channel connection)
  "Runs the swak proxy event loop in the current thread indefinitely."
  (tagbody
   start
     (with-top-level-restart (connection (go start))
       (with-connection (connection)
         (loop
            (destructure-case (wait-for-event `(:emacs-channel-send . _))
              ((:emacs-channel-send c (selector &rest args))
               (assert (eq c channel))
               (channel-send channel selector args))))))))


;;; eval-and-grab-output
;;; interactive-eval
;;; compile-string-for-emacs
;;; compile-file-for-emacs
;;; listener-eval
;;; completions

;;; some methods for proxy-eval that aren't expected to need specialized for now

(defmethod proxy-eval ((op (eql 'swank-backend:buffer-first-change)) (target t)
                       continuation &rest r)
  (destructuring-bind ((name)) r
    (buffer-first-change (eval name))))

;;;; slime-proxy REPL
(defun send-proxy-repl-results-to-emacs (values)
  (finish-output)
  (if (null values)
      (send-to-emacs `(:write-string "; No value" :proxy-repl-result))
      (dolist (v values)
        (send-to-emacs `(:write-string ,(cat (prin1-to-string v) #\newline)
                                       :proxy-repl-result)))))