(cl:in-package :cl-user)

(defpackage :slime-proxy-system
  (:use #:cl #:asdf))

(in-package :slime-proxy-system)

(asdf:defsystem :slime-proxy
  :depends-on (:clws :parenscript :yason :anaphora :bordeaux-threads :alexandria)
  :serial t
  :components ((:file "package")
               (:file "wsproxy")
               (:file "swank-proxy-implementation")))
  
