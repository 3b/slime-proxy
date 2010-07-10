(in-package #:swank)
(defvar *proxy-cmd*)
(defclass proxy-channel (channel)
  ()
  )

;; stub out some stuff that otherwise would require a hacked slime
(defvar *arglist-dispatch-hooks* nil)
(defvar *operator-p-hooks* nil)
(defun ps-operator-p (op )
  #++(let ((*package* (find-package :keyword)))
    (format t "operator-p ~s = ~s~%" op  (ps::parenscript-function-p op)))
  (ps::parenscript-function-p op))
(fmakunbound 'proxy-eval)
(defgeneric proxy-eval (op proxy-target continuation &rest args))
(defmethod proxy-eval ((op t) (proxy-target t) cont &rest args)
  (format t "unknown proxy-eval command ~s or proxy target ~s~%" (cons op args) proxy-target)
  (funcall cont nil nil)
  :async)
(defmacro define-proxy-fun (name target (&rest args) &body body)
  (let ((rest (gensym)))
    `(defmethod proxy-eval ((op (eql ',name)) (target (eql ',target)) continuation &rest ,rest)
      (destructuring-bind (,args) ,rest
        ,@body))))

(defun proxy-eval-form (form target continuation)
  (let ((*arglist-dispatch-hooks* (cons 'ps-arglist-dispatch
                                        *arglist-dispatch-hooks*))
        (*operator-p-hooks* (cons 'ps-operator-p
                                  *operator-p-hooks*)))
    (funcall 'proxy-eval (car form) target continuation
             (mapcar 'eval (cdr form)))))



(defun proxy-eval-for-emacs (form buffer-package id)
  (let* ((b (guess-buffer-package buffer-package))
         (*buffer-package* b)
         (*buffer-readtable* (guess-buffer-readtable buffer-package))
         (*pending-continuations* (cons id *pending-continuations*)))
    (check-type *buffer-package* package)
    (check-type *buffer-readtable* readtable)
    (flet ((cont (ok result)
             (when ok
               (let ((*buffer-readtable* b))
                 (run-hook *pre-reply-hook*)))
             (send-to-emacs `(:return ,(current-thread)
                                     ,(if ok
                                          `(:ok ,result)
                                          `(:abort))
                                     ,id))))
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

(define-channel-method :proxy (c args)
  (setf *proxy-cmd* (list c args))
  ;(format t "proxy ~s~%" (list c args))
  (case (car args)
    (:emacs-rex
     (destructuring-bind (form package thread id &rest r) (cdr args)
       (declare (ignore r thread))
       ;(format t "form ~s~% package ~s~% id ~S~%" form package id)
       (proxy-eval-for-emacs form package id)
       #++(let ((swank-backend::*proxy-interfaces* (make-hash-table)))
         (eval-for-emacs form package id))
)
))
  #++(apply #'eval-for-emacs args))



(defun create-proxy-listener ()
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
