(in-package #:swank)

(defvar *proxy-cmd*
  "Used for debugging purposes.")

(defclass proxy-channel (channel)
  ()
  (:documentation "Subclass of the main slime channel class used for
  slime-proxy."))

;; fixme: this should be implemented in swank-proxy-ps, not here.
;; Maybe achieve this by by generic function--the main problem now
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


(defun proxy-eval-for-emacs (form buffer-package id)
  "Binds *BUFFER-PACKAGE* to BUFFER-PACKAGE and proxy-evaluates FORM.
Return the result to the continuation ID.  Errors are trapped and
invoke our debugger.

Analagous to EVAL-FOR-EMACS, but instead of using EVAL to evaluate
form, uses PROXY-EVAL-FORM"
  (declare (optimize (debug 3)))
  (let* ((b (guess-buffer-package buffer-package))
         (brt (guess-buffer-readtable buffer-package))
         (*buffer-package* b)
         (*buffer-readtable* brt)
         (*pending-continuations* (cons id *pending-continuations*)))
    (check-type *buffer-package* package)
    (check-type *buffer-readtable* readtable)
    (flet ((cont (ok result)
             ;; fixme: make sure that we are binding the proper specials
               (when ok
                 (let ((*buffer-package* b)
                       (*buffer-readtable* brt))
                   (run-hook *pre-reply-hook*)
                   (send-to-emacs `(:return ,(current-thread)
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
              (setq result (with-slime-interrupts (proxy-eval-form form :ps #'cont)))
              #++(run-hook *pre-reply-hook*)
              (setq ok t))
         (when (not (eq result :async))
           (cont ok result)))))))

;;; All slime-proxy events are sent through the :proxy method, bundled
;;; with a particular command and its arguments
(define-channel-method :proxy (c args)
  (setf *proxy-cmd* (list c args))
  (format t "proxy ~s~%" (list c args))
  (case (car args)
    (:emacs-rex
     (destructuring-bind (form package thread id &rest r) (cdr args)
       (declare (ignore r thread))
       ;(format t "form ~s~% package ~s~% id ~S~%" form package id)
       (proxy-eval-for-emacs form package id)
       #++(let ((swank-backend::*proxy-interfaces* (make-hash-table)))
         (eval-for-emacs form package id))))))


;; SPAWN-PROXY-THREAD and CREATE-PROXY-LISTENER set up the swank-proxy
;; thread that listens in on SWANK events and  
(defslimefun create-proxy-listener (remote)
  (let* ((pkg *package*)
         (conn *emacs-connection*)
         (ch (make-instance 'listener-channel
                            :remote remote
                            :env (initial-listener-bindings remote))))

    (with-slots (thread id) ch
      (when (use-threads-p)
        (setf thread (spawn-proxy-thread ch conn)))
      (list id
            (thread-id thread)
            (package-name pkg)
            (package-string-for-prompt pkg)))))

(defun create-proxy-listener ()
  "Analogous to SWANK:CREATE-LISTENER"
  (let ((ch (make-instance 'proxy-channel)))
    (with-slots (thread id) ch
      (when (use-threads-p)
        (setf thread (spawn-proxy-thread ch *emacs-connection*)))
      (list id (thread-id thread)))))


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
