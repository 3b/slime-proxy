(Defun old-proxy-eval (form cont)
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
                 #++(format t "found ps function, args=~s~%" args)
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
          #++(format t "looking for xrefs for ~s = ~s (p ~s~%~%" name n *package*)
          (gethash n ps::*ps-function-location-toplevel-cache*))
        #++(print (swank:find-definitions-for-emacs "defun")))
       ((swank:autodoc raw-form &key print-right-margin)
        (swank:autodoc (eval raw-form) :print-right-margin (eval print-right-margin) ))

       (t (format t "unknown method ~s~%" form))))))



(in-package #:swank-backend)
(defparameter *proxy-interfaces* nil)
(defparameter *imp-used* nil)
(loop for i in *interface-functions*
   do (setf (get i 'old-implementation)
            (or (get i 'old-implementation)
                (get i 'implementation))
            (get i 'implementation)
            (let ((i i))
              (lambda (&rest args)
                (when *proxy-interfaces*
                  (pushnew (format nil "impl ~s : ~s~%" i args) *imp-used*))
                (let ((f (or (if nil ;*proxy-interfaces*
                                 (gethash i *proxy-interfaces*)
                                 (get i 'old-implementation))
                             (get i 'default))))
                  (if f
                      (apply f args)
                      (error "~s not implemented (proxy=~s" i *proxy-interfaces*)))))))

(loop for i in *interface-functions*
   do (setf (get i 'implementation) (get i 'old-implementation)
            ))

*imp-used*

#++
(FIND-EXTERNAL-FORMAT
 GUESS-EXTERNAL-FORMAT
 BUFFER-FIRST-CHANGE
 RECEIVE-IF
 FRAME-RESTARTABLE-P
 PRINT-FRAME
 MAKE-LOCK
 MAKE-OUTPUT-STREAM
 COMPUTE-BACKTRACE
 CONDITION-EXTRAS
 FORMAT-SLDB-CONDITION
 THREAD-ID
 CURRENT-THREAD
 CALL-WITH-DEBUGGING-ENVIRONMENT
 CALL-WITH-DEBUGGER-HOOK
 SEND
 CALL-WITH-LOCK-HELD
 CALL-WITH-SYNTAX-HOOKS)