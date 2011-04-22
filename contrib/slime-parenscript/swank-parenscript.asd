(cl:in-package :cl-user)

(defpackage :swank-parenscript-system
  (:use #:cl #:asdf))

(in-package :swank-parenscript-system)

(asdf:defsystem :swank-parenscript
  :depends-on (:parenscript :slime-proxy)
  :serial t
  :components ((:file "swank-proxy-ps")))
  
