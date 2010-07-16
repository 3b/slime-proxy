(cl:in-package :cl-user)

(defpackage :slime-parenscript-system
  (:use #:cl #:asdf))

(in-package :slime-parenscript-system)

(asdf:defsystem :slime-parenscript
  :depends-on (:parenscript :slime-proxy)
  :serial t
  :components ((:file "swank-proxy-ps")))
  
