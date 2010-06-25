(in-package #:swank)
(defvar *proxy-cmd*)
(defclass proxy-channel (channel)
  ()
  )


(Defun proxy-eval (form cont)
  (with-buffer-syntax ()
    (destructure-case form
     ((swank::operator-arglist name package)
      (ignore-errors
        (let ((sym (parse-symbol name (guess-buffer-package package))))
          #++(format t "lookup symbol ~s ~s -> ~s (~s)~%"
                  name package sym (guess-buffer-package package))
          ;; first check for a ps macro/function
          (cond
            ((ps::parenscript-function-p sym)
             (let ((args (ps::parenscript-arglist sym)))
               (format t "found s function, args=~s~%" args)
               (cond ((eq args :not-available) nil)
                     (t (princ-to-string (cons name args))))))
            ;; then ask a browser
            (t
             (swank-proxy-ws::proxy-send-to-client
              nil
              (ps:ps (let (rr)
                       (ps:try (setf rr
                                     ((ps:@ *function introspect)
                                      (ps:lisp
                                       (read-from-string name)
                                       #++(ps::parenscript-print (ps::compile-expression (read-from-string name)) nil))))
                               (:catch (e)
                                 (setf rr (ps:create arg-names "not available"))))
                       rr))
              ;; if that fails, fall back to looking up in host lisp
              ;; fixme: possibly better to try host first?
              (lambda (response error)
                (let (result)
                  (unwind-protect
                       (progn
                         #++(format t "11got response from client: ~s ~s~%~s~%" response error (if response (gethash "argNames" response)))
                         (cond
                           ((and (not error)
                                 response
                                 (not (equal "not available"
                                             (gethash "argNames" response))))
                            (setf result
                                  (princ-to-string (cons name (gethash "argNames" response))))
                            )
                           (t (setf result
                                    (when sym
                                      (let ((args (ignore-errors (arglist sym))))
                                        (cond ((eq args :not-available) nil)
                                              (t (princ-to-string (cons name args)))))))))
                         )
                    (funcall cont t result)))))
             :async
             )))))
     ((swank:interactive-eval string)
      (let ((p (ps:ps* (read-from-string  string))))
                                        ;(format t "eval ~s~%" string)
                                        ;(format t " -> ~s~%" p)
        (swank-proxy-ws::proxy-send-to-client nil p)
        "(async)"
        )
      )
     ((swank:listener-eval string)
      (let ((p (ps:ps* (read-from-string  string))))
        (swank-proxy-ws::proxy-send-to-client nil p)
        nil
        )
      )
     ((swank:compile-string-for-emacs string buffer position filename policy)
      (declare (ignore buffer position filename policy))
      (let ((p (ps:ps* (read-from-string  string))))
        (swank-proxy-ws::proxy-send-to-client nil p)
        "(async)"
        )
      )
     (t (format t "unknown method ~s~%" form)))))


(defun proxy-eval-for-emacs (form buffer-package id)
  (flet ((cont (ok result)
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
            (setq result (with-slime-interrupts (proxy-eval form #'cont)))
            (run-hook *pre-reply-hook*)
            (setq ok t))
       (when (not (eq result :async))
         (cont ok result))))))

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

(progn
  (setf *channels* '())
  (setf *channel-counter* 0)
  (create-proxy-listener))



;;; eval-and-grab-output
;;; interactive-eval
;;; compile-string-for-emacs
;;; compile-file-for-emacs
;;; listener-eval
;;; completions
