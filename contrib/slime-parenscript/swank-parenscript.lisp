;; this file is loaded up in Common Lisp via slime-parenscript-init
;; we simply asdf-load the slime-parenscript system
(asdf:operate 'asdf:load-op :swank-parenscript)
;; and tell swank it loaded
(provide :swank-parenscript)