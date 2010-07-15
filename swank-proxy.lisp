(in-package #:swank)

(defvar *proxy-cmd*
  "Used for debugging purposes.")

(defclass proxy-channel (channel)
  ((target :initarg :target :initform nil :accessor channel-target
           :documentation "The target for messages delivered through this
           proxy channel. "))
  (:documentation "Subclass of the main slime channel class used for
  slime-proxy."))

(defclass proxy-listener-channel (proxy-channel listener-channel)
  ())

;; fixme: this should be implemented in swank-proxy-ps, not here.
;; Maybe achieve this by by generic function--the main problem now is
;; achieving the propery dynamic bindings for specials needed by the
;; parenscript proxy target
(defvar *arglist-dispatch-hooks* nil)
(defvar *operator-p-hooks* nil)
(defun ps-operator-p (op )
  #++(let ((*package* (find-package :keyword)))
       (format t "operator-p ~s = ~s~%" op  (ps::parenscript-function-p op)))
  #+parenscript
  (ps::parenscript-function-p op))


(defgeneric proxy-eval (op proxy-target continuation &rest args)
  (:documentation "The beautiful generic function at the heart of
slime-proxy.  Used to evaluate a particular operation OP targetting
a particulary thing, proxy-target.

Unless this function returns :async, then continuation will be
evaluated by PROXY-EVAL-FOR-EMACS."))

(defmethod proxy-eval ((op t) (proxy-target t) cont &rest args)
  ;; by default, simply call the continuation and return :async
  (format t "unknown proxy-eval command ~s or proxy target ~s~%" (cons op args) proxy-target)
  (funcall cont nil nil)
  :async)

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

(defun proxy-eval-form (form target continuation)
  ""
  ;; fixme: this should be implemented in swank-proxy-ps, not here.
  (let ((*arglist-dispatch-hooks* (cons 'ps-arglist-dispatch
                                        *arglist-dispatch-hooks*))
        (*operator-p-hooks* (cons 'ps-operator-p
                                  *operator-p-hooks*)))
    (funcall 'proxy-eval (car form) target continuation
             (mapcar 'eval (cdr form)))))


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
         (conn *emacs-connection*)
         (*buffer-package* b)
         (*buffer-readtable* brt)
         (*pending-continuations* pc))
    (check-type *buffer-package* package)
    (check-type *buffer-readtable* readtable)
    #+nil
    (format t "proxy-eval-for-emacs connection: ~A / ~A~%" conn *emacs-connection*)
    (flet ((cont (ok result)
             ;; fixme: make sure that we are binding the proper specials
             ;; fixme what about the not-okay case?
               (when ok
                 (let ((*buffer-package* b)
                       (*buffer-readtable* brt)
                       (*pending-continuations* pc)
                       (*emacs-connection* conn))
                   #+nil(format t "proxy-eval... connection (continuation): ~A / ~A~%" conn *emacs-connection*)
                   (run-hook *pre-reply-hook*)
                   (send-to-emacs `(:return ,thread
                                            ,(if ok
                                                 `(:ok ,result)
                                                 `(:abort))
                                            ,id))))))
     (let (ok result)
       (unwind-protect
            (let ((*buffer-package* (guess-buffer-package buffer-package))
                  (*buffer-readtable* (guess-buffer-readtable buffer-package))
                  (*pending-continuations* (cons id *pending-continuations*)))
              (check-type *buffer-package* package)
              (check-type *buffer-readtable* readtable)
              ;; APPLY would be cleaner than EVAL. 
              ;; (setq result (apply (car form) (cdr form)))
              (setq result (with-slime-interrupts (proxy-eval-form form (channel-target channel)
                                                                   #'cont)))
              #++(run-hook *pre-reply-hook*)
              (setq ok t))
         (when (not (eq result :async))
           (cont ok result)))))))

;;; All slime-proxy events are sent through the :proxy method, bundled
;;; with a particular command and its arguments
(define-channel-method :proxy ((channel proxy-channel) args)
  (setf *proxy-cmd* (list channel args))
  #+nil(format t "proxy ~s~%" (list channel args))
  (case (car args)
    (:emacs-rex
     (destructuring-bind (form package thread id &rest r) (cdr args)
       (declare (ignore r))
       ;(format t "form ~s~% package ~s~% id ~S~%" form package id)
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
                 :target target
                 :remote remote
                 :env (initial-listener-bindings remote)))

(defslimefun create-proxy-listener (remote target)
  ;; fixme: move most of this into proxy-create-channel
  (let* ((pkg *package*)
         (conn *emacs-connection*)
         (ch (proxy-create-channel (intern (string-upcase target) :keyword) :remote remote)))

    (with-slots (thread id) ch
      (when (use-threads-p)
        (setf thread (spawn-proxy-thread ch conn)))
      (list id
            (thread-id thread)
            (package-name pkg)
            (package-string-for-prompt pkg)))))

(defun spawn-proxy-thread (channel connection)
  (spawn (lambda ()
           (tagbody
            start
              (with-top-level-restart (connection (go start))
                (with-connection (connection)
                  (loop
                     (destructure-case (wait-for-event `(:emacs-channel-send . _))
                       ((:emacs-channel-send c (selector &rest args))
                        (assert (eq c channel))
                        (channel-send channel selector args))))))))
         :name "swank-proxy-thread"))

#++
(progn
  (setf *channels* '())
  (setf *channel-counter* 0)
  (create-proxy-listener))
#++
(create-proxy-listener)

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
