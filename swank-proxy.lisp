(in-package #:swank)
(defvar *proxy-cmd*)
(defclass proxy-channel (channel)
  ()
  )
(defun ps-arglist-dispatch (op args)
  (declare (ignorable args))
  #++(let ((*package* (find-package :keyword)))
    (format t "p-a-d ~s ~s~%" op args)
    )
  (when (ps::parenscript-function-p op)
    ;(format t "p-a-d -> ~s~%" (ps::parenscript-arglist op))
    (with-available-arglist (decoded-arglist) 
        (decode-arglist (ps::parenscript-arglist op))
      #++(enrich-decoded-arglist-with-extra-keywords decoded-arglist
                                                  (cons op args))
      (values decoded-arglist nil nil))))

(defun ps-operator-p (op )
  #++(let ((*package* (find-package :keyword)))
    (format t "operator-p ~s = ~s~%" op  (ps::parenscript-function-p op)))
  (ps::parenscript-function-p op))

(Defun proxy-eval (form cont)
  ;; fixme: more of these should EVAL their args, possibly just swap
  ;; out the operator and call EVAL instead?
  (let ((*arglist-dispatch-hooks* (cons 'ps-arglist-dispatch
                                        *arglist-dispatch-hooks*))
        (*operator-p-hooks* (cons 'ps-operator-p
                                  *operator-p-hooks*)))
    (with-buffer-syntax ()
      ;; todo: convert this to a defgeneric, and methods specializing on
      ;; swank symbol and proxy type (symbol like :ps etc)
      ;; then set a buffer-local to store the proxy type
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
        (declare (ignorable buffer position filename policy))
        (let* ((start (get-internal-real-time))
               (ps::*ps-source-file* filename)
               (ps::*ps-source-position* position )
               (ps::*ps-source-buffer* buffer)
               (p (ps:ps* (read-from-string  string))))
          (swank-proxy-ws::proxy-send-to-client nil p)
          (make-compilation-result nil t (float
                                          (/ (- (get-internal-real-time) start)
                                             internal-time-units-per-second)))
          )
        )
       ((swank:compile-file-for-emacs filename load-p &rest r)
        (declare (ignore r))
        (unless load-p
          (format t "warning: compilation without loading not handled properly yet in swank-proxy/compile-file-for-emacs..."))
        (let ((start (get-internal-real-time))
              (compiled (ps:ps-compile-file filename)))
          (swank-proxy-ws::proxy-send-to-client
           nil
           compiled)
          (make-compilation-result nil t (float
                                          (/ (- (get-internal-real-time) start)
                                             internal-time-units-per-second))))
        )
       ((swank-backend:buffer-first-change name)
        (swank-backend:buffer-first-change (eval name)))
       ((swank:find-definitions-for-emacs name)
        (let ((n (from-string (eval name)) ))
          (format t "looking for xrefs for ~s = ~s (p ~s~%~%" name n *package*)
         (print (gethash n ps::*ps-function-location-toplevel-cache*)))
        #++(print (swank:find-definitions-for-emacs "defun")))
       ((swank:autodoc raw-form &key print-right-margin)
        (swank:autodoc (eval raw-form) :print-right-margin (eval print-right-margin) ))

       (t (format t "unknown method ~s~%" form))))))


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

#++
(progn
  (setf *channels* '())
  (setf *channel-counter* 0)
  (create-proxy-listener))

(create-proxy-listener)

;;; eval-and-grab-output
;;; interactive-eval
;;; compile-string-for-emacs
;;; compile-file-for-emacs
;;; listener-eval
;;; completions



;;;; autodoc hooks
;;fixme: separate this into a separate file, only load with autodoc contrib
(defmethod arglist-dispatch :around (op argument)
  ;(format t "arglist dispatch ~s / ~s~T" op argument)
  (when (next-method-p)
    (call-next-method))
  #++((ps::parenscript-function-p sym)
             (let ((args (ps::parenscript-arglist sym)))
               (format t "found s function, args=~s~%" args)
               (cond ((eq args :not-available) nil)
                     (t (princ-to-string (cons name args))))))
)


(defmethod arglist-dispatch :around (op argument)
 ; (format t "arglist dispatch ~s / ~s~T" op argument)
  (when (next-method-p)
    (call-next-method))
  #++((ps::parenscript-function-p sym)
             (let ((args (ps::parenscript-arglist sym)))
               (format t "found s function, args=~s~%" args)
               (cond ((eq args :not-available) nil)
                     (t (princ-to-string (cons name args))))))
)

(defmethod arglist-dispatch (operator arguments)
  ;(format t "--dispatch--~%")
    (cond
      ((and (symbolp operator) (valid-operator-symbol-p operator))
       (multiple-value-bind (decoded-arglist determining-args)
           (compute-enriched-decoded-arglist operator arguments)
         (with-available-arglist (arglist) decoded-arglist
           ;; replace some formal args by determining actual args
           (setf arglist (delete-given-args arglist determining-args))
           (setf (arglist.provided-args arglist) determining-args)
           arglist)))
      ((find-if (lambda (hook) #++(format t "trying hook ~s~%" hook) (funcall hook operator arguments)) *arglist-dispatch-hooks*))
      (t (return-from arglist-dispatch :not-available)))) 