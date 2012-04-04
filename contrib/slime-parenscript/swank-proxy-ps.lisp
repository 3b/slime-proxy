(in-package #:swank)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (swank-require :swank-arglists)
  (swank-require :swank-c-p-c))

(defvar *arglist-dispatch-hooks* nil)
(defvar *operator-p-hooks* nil)
(defun ps-operator-p (op )
  (ps::parenscript-function-p op))

(defmethod proxy-eval-form (form (target (eql :ps)) continuation)
  (let ((*arglist-dispatch-hooks* (cons 'ps-arglist-dispatch
                                        *arglist-dispatch-hooks*))
        (*operator-p-hooks* (cons 'ps-operator-p
                                  *operator-p-hooks*))
        (swank::*valid-operator-symbol-p-hooks*
         (cons 'ps::parenscript-function-p
               swank::*valid-operator-symbol-p-hooks*))
        (swank-backend::*arglist-hooks*
         (cons 'ps::parenscript-arglist
               swank-backend::*arglist-hooks*)))
    (call-next-method)))

;;; Mostly ordered by how difficult these were to implement

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-proxy-fun swank:simple-completions :ps (prefix package)
  (swank:simple-completions prefix package))

(defvar swank-proxy::*proxy-read-function* nil)
;;#+parenscript-reader
;;(defparameter swank-proxy::*proxy-read-function* #'parenscript.reader::read)

;;; macro expansion
(defun ps-read-from-string (string)
  (with-input-from-string (stream string)
    (let ((ps:*ps-read-function* (or swank-proxy::*proxy-read-function*
                                     ps:*ps-read-function*
                                     #'read)))
      (funcall ps:*ps-read-function* stream))))

(defun apply-ps-macro-expander (expander string)
  (with-retry-restart (:msg "Retry (proxied) SLIME Macroexpansion request.")
    (with-buffer-syntax ()
      (with-bindings *macroexpand-printer-bindings*
        (prin1-to-string (funcall expander (ps-read-from-string string)))))))

(define-proxy-fun swank-macroexpand-1 :ps (string)
  (apply-ps-macro-expander #'ps::ps-macroexpand-1 string))

(define-proxy-fun swank-macroexpand :ps (string)
  (apply-ps-macro-expander #'ps::ps-macroexpand string))

(define-proxy-fun swank-macroexpand-all :ps (string)
  ;; fixme: true macroexpand all
  (apply-ps-macro-expander #'ps::ps-macroexpand string))

(define-proxy-fun swank-compiler-macroexpand-1 :ps (string)
  ;; fixme: true macroexpand all
  (apply-ps-macro-expander #'ps::ps-macroexpand-1 string))

(define-proxy-fun swank-compiler-macroexpand :ps (string)
  ;; fixme: true macroexpand all
  (apply-ps-macro-expander #'ps::ps-macroexpand string))

(define-proxy-fun swank-format-string-expand :ps (string)
  ;; fixme: true macroexpand all
  (apply-ps-macro-expander #'format-string-expand string))

;; overridden by autodoc
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
         (swank-proxy::proxy-send-to-client
          nil
          (ps:ps (let (rr)
                   (ps:try (setf rr
                                 ((ps:@ *function introspect)
                                  (ps:lisp
                                   (let ((*package* (guess-buffer-package package)))
                                     (read-from-string name)))))
                           (:catch (e)
                             (setf rr (ps:create arg-names "not available"))))
                   rr))
          ;; if that fails, fall back to looking up in host lisp
          ;; fixme: possibly better to try host first?
          (lambda (ok response)
            (let (result
                  (error (not ok)))
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

(defun eval-common (string continuation)
  (with-retry-restart (:msg "Retry (proxied) SLIME REPL evaluation request.")
      (proxy-track-package
       (lambda ()
         (let* ((eof (cons nil nil))
                (f (read-from-string string nil eof))
                (p (unless (eq f eof) (ps::ps* f)))
                (*send-repl-results-function* 'send-proxy-repl-results-to-emacs))
           (cond
             ((eq f eof)
              (funcall *send-repl-results-function* nil)
              nil)
             (t
              (swank-proxy::proxy-send-to-client
               nil p
               (lambda (eval-ok? result)
                 ;(break)
                 (let ((*send-repl-results-function* 'send-proxy-repl-results-to-emacs))
                   (cond
                     (eval-ok?
                      (funcall *send-repl-results-function* (list result))
                      (funcall continuation t nil))

                     (t
                      (funcall *send-repl-results-function* nil)
                      (funcall continuation eval-ok? result))))))

              :async)))))))

(define-proxy-fun swank::listener-eval :ps (string)
  (declare (optimize (debug 3)))
  (clear-user-input)
  (with-buffer-syntax ()
    (eval-common string continuation)))

(define-proxy-fun swank:re-evaluate-defvar :ps (string)
  (declare (optimize (debug 3)))
  (clear-user-input)
  (with-buffer-syntax ()
    (eval-common string continuation)))

(define-proxy-fun swank:find-definitions-for-emacs :ps (name)
  (declare (optimize (debug 3)))
  (let ((result
         (ignore-errors
           (with-buffer-syntax ()
             (let* ((name-form (ps-read-from-string name))
                    (source-loc (car (gethash name-form ps::*function-location-toplevel-cache*))))
               (cond
                 ((assoc :buffer source-loc)
                  (princ (swank-backend::make-location
                    (assoc :buffer source-loc)
                    (assoc :offset source-loc)
                    (assoc :snippet source-loc))))
                 ((assoc :file source-loc)
                  ;; fixme: don't repeat all these ASSOC etc
                  (let* ((f (assoc :file source-loc))
                         (pos (or (if (cadr (assoc :position source-loc))
                                      (1- (cadr (assoc :position source-loc))))
                                  (when (assoc :form-path source-loc)
                                    (swank-backend::source-file-position
                                     (translate-logical-pathname
                                      (cadr f))
                                     (cadr (assoc :modified source-loc))
                                     (cadr (assoc :form-path source-loc))
                                     ))))
                         (snippet (or (cadr (assoc :snippet source-loc))
                                      (and pos
                                           (swank-backend::source-hint-snippet
                                            (translate-logical-pathname
                                             (cadr f))
                                            (cadr (assoc :modified source-loc))
                                            pos)))))
                    (swank-backend::make-location
                     (if (pathnamep (cadr f))
                         `(:file ,(namestring (cadr f)))
                         f)
                     `(:position ,(1+ (or pos 0)))
                     `(:snippet ,snippet)))
                  )))))))
    #++(format t "Would return ~S~%" result)
    (list (list name result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SWANK-C-P-C
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-proxy-fun swank::completions-for-character :ps (prefix)
  (swank::completions-for-character prefix))

;; fixme kludge
(defparameter *ps-repl-package* nil
  "Used to track the package we are in in the proxy
  environment/buffer.  Prevents the proxy buffer's *package* and the
  CL repl's *package* from interacting strangely. ")

(defun proxy-track-package (fun)
  "Calls FN and lets it change *package*, transmitting the result to
emacs with :proxy-new-package."
  (let ((p *package*))
    (unwind-protect (funcall fun)
      (unless (eq *package* p)
        ;; :proxy-event needs patched slime, so disabled for now
        (send-to-emacs (list #++ :proxy-event
                             :new-package (package-name *package*)
                             (package-string-for-prompt *package*)))))))

(define-proxy-fun swank:compile-string-for-emacs :ps (string buffer position filename policy)
  (declare (ignorable buffer position filename policy))
  (let* ((start (get-internal-real-time))
         ;; protect package changes in ps repl from affecting cl repl
         ;(*package* *buffer-package*)
         (ps::*ps-source-file* filename)
         (ps::*ps-source-position* position )
         (ps::*ps-source-buffer* buffer))
    (with-buffer-syntax ()
      (let ((p (ps:ps* (read-from-string  string))))
        (swank-proxy::proxy-send-to-client nil p)
        (make-compilation-result
         :notes nil :successp t
         :duration (float (/ (- (get-internal-real-time) start)
                             internal-time-units-per-second)))))))
      

(define-proxy-fun swank:compile-file-for-emacs :ps (filename load-p &rest r)
    (declare (ignore r))
  (unless load-p
    (format t "warning: compilation without loading not handled properly yet in swank-proxy/compile-file-for-emacs..."))
  (let ((start (get-internal-real-time))
        (compiled (ps:ps-compile-file filename)))
    (swank-proxy::proxy-send-to-client
     nil
     compiled)
    (make-compilation-result
     :notes nil :successp t
     :duration (float (/ (- (get-internal-real-time) start)
                         internal-time-units-per-second)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Swank arglists (autodoc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-ps-autodoc-env (&body body)
  `(let ((swank::*valid-operator-symbol-p-hooks*
          (cons 'ps::parenscript-function-p
                swank::*valid-operator-symbol-p-hooks*))
         (swank-backend::*arglist-hooks*
          (cons 'ps::parenscript-arglist
                swank-backend::*arglist-hooks*)))
        ,@body))

(define-proxy-fun swank::autodoc :ps (raw-form &rest args &key print-right-margin)
  "Return a string representing the arglist for the deepest subform in
RAW-FORM that does have an arglist. The highlighted parameter is
wrapped in ===> X <===."
  (with-ps-autodoc-env
    (apply 'swank::autodoc raw-form args)))

(define-proxy-fun :ps swank::complete-form (raw-form)
  "Read FORM-STRING in the current buffer package, then complete it
  by adding a template for the missing arguments."
  (with-ps-autodoc-env
    (swank::complete-form raw-form)))

(define-proxy-fun swank::completions-for-keyword :ps (keyword-string raw-form)
  "Return a list of possible completions for KEYWORD-STRING relative
to the context provided by RAW-FORM."
  (with-ps-autodoc-env
    (swank::completions-for-keyword keyword-string raw-form)))

(define-proxy-fun swank::completions :ps (string default-package-name)
  (with-ps-autodoc-env
    (swank::completions string default-package-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Not classified
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-proxy-fun swank:interactive-eval :ps (string)
  #+nil
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLIME interactive evaluation request.")
      (let ((p (ps:ps* (read-from-string string))))
        (swank-proxy::proxy-send-to-client nil p continuation)
        :async)))
  (let ((p (ps:ps* (let ((*package* *buffer-package*))
                     (read-from-string string))))
        ;; store some specials that format-values-for-echo-area expects
        ;; to be bound, so we can restore them in the continuation
        (buffer-package *buffer-package*)
        (buffer-readtable *buffer-readtable*))
    (swank-proxy::proxy-send-to-client
     nil p
     (lambda (ok result)
       (funcall continuation
                ok (let ((*buffer-package* buffer-package)
                         (*buffer-readtable* buffer-readtable))
                     (format-values-for-echo-area (list result))))))
    :async)
  )



