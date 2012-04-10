(cl:in-package #:cl-user)
(cl:defpackage #:web-socket-js
  (:export #:*web-socket
           #:__swf-location
           #:__debug
           #:+web_socket_swf_location+
           #:+web_socket_debug+))
(cl:defpackage #:slimy
  (:use :cl :ps))

#++
(ps:obfuscate-package '#:slimy)
#++
(ps:unobfuscate-package '#:slimy)
#++
(ps:obfuscate-package '#:slimy
  (let ((code-pt-counter #x100)
        (symbol-map (make-hash-table)))
    (lambda (symbol)
      (or (gethash symbol symbol-map)
          (setf (gethash symbol symbol-map)
                (make-symbol (string (code-char (incf code-pt-counter)))))))))

;;; this stuff should probably be somewhere else...
(in-package #:slimy)
;; wip hack for making functions in objects nicer to deal with
;; across slime-proxy:
;; store the function body as a lambda in the specified object
;; and define a macro so the function/arglist/etc show up in ps,
;; which expands to a call to the function in the object/
(defpsmacro defun-wrapped ((&rest scoped-name) lambda-list &body body)
  (let ((w (gensym)))
    `(progn
       (defmacro ,(car (last scoped-name)) (&whole ,w ,@lambda-list)
         (declare (ignore ,@(remove-if (lambda (a)
                                         (member a '(&key &rest &optional
                                                     &allow-other-keys)))
                                       lambda-list)))
         `(funcall (ps:@ ,@',scoped-name) ,@(cdr ,w)))
       (setf (@ ,@scoped-name)
             (lambda ,lambda-list
               ,@body))
       )))
;; same thing for variables, member in object + symbol-macro to access it
(defpsmacro defvar-wrapped ((&rest scoped-name) value)
  `(progn
     (define-symbol-macro ,(car (last scoped-name))
         (ps:@ ,@scoped-name))
       (setf (@ ,@scoped-name) ,value)))
