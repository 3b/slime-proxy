(defsystem :slime-proxy
  :depends-on (:clws :parenscript :yason)
  :serial t
  :components ((:file "wsproxy")
               (:file "swank-proxy")))
