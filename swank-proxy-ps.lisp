(in-package #:swank)

(define-proxy-fun swank:interactive-eval :ps (string)
  (let ((p (ps:ps* (read-from-string string))))
    (swank-proxy-ws::proxy-send-to-client nil p continuation)
    :async))

(define-proxy-fun swank::operator-arglist :ps (name package)
  ;; fixme: probably should factor some of this out to common code?
  (ignore-errors
    (let ((sym (parse-symbol name (guess-buffer-package package))))
      ;; first check for a ps macro/function
      (cond
        ((ps::parenscript-function-p sym)
         (let ((args (ps::parenscript-arglist sym)))
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
                                   (read-from-string name))))
                           (:catch (e)
                             (setf rr (ps:create arg-names "not available"))))
                   rr))
          ;; if that fails, fall back to looking up in host lisp
          ;; fixme: possibly better to try host first?
          (lambda (response error)
            (let (result)
              (unwind-protect
                   (progn
                     (cond
                       ((and (not error)
                             response
                             (not (equal "not available"
                                         (gethash "argNames" response))))
                        (setf result
                              (princ-to-string (cons name (gethash "argNames" response)))))
                       (t (setf result
                                (when sym
                                  (let ((args (ignore-errors (arglist sym))))
                                    (cond ((eq args :not-available) nil)
                                          (t (princ-to-string (cons name args))))))))))
                (funcall continuation t result)))))
         :async)))))


(define-proxy-fun swank:listener-eval :ps (string)
  (clear-user-input)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry (proxied) SLIME REPL evaluation rexuest.")
      (track-package
       (lambda ()
         (let* ((eof (cons nil nil))
                (f (read-from-string string nil eof))
                (p (unless (eq f eof) (ps:ps* f))))
           (if (eq f eof)
               (progn
                 (funcall *send-repl-results-function* nil)
                 nil)
               (progn
                 (swank-proxy-ws::proxy-send-to-client
                  nil p
                  (lambda (o r)
                    (if o
                        (progn
                          (funcall *send-repl-results-function* (list r))
                          (funcall continuation t nil))
                        (funcall continuation o r))
                    ))
                 :async))))))))

(define-proxy-fun swank:compile-string-for-emacs :ps (string buffer position filename policy)
  (declare (ignorable buffer position filename policy))
  (let* ((start (get-internal-real-time))
         (ps::*ps-source-file* filename)
         (ps::*ps-source-position* position )
         (ps::*ps-source-buffer* buffer)
         (p (ps:ps* (read-from-string  string))))
    (swank-proxy-ws::proxy-send-to-client nil p)
    (make-compilation-result nil t (float
                                    (/ (- (get-internal-real-time) start)
                                       internal-time-units-per-second)))))

(define-proxy-fun swank:compile-file-for-emacs :ps (filename load-p &rest r)
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

(define-proxy-fun swank:find-definitions-for-emacs :ps (name)
 (let ((n (from-string name)))
   (gethash n ps::*ps-function-location-toplevel-cache*)))

(define-proxy-fun swank:autodoc :ps (raw-form &key print-right-margin)
  (swank:autodoc raw-form :print-right-margin print-right-margin ))
