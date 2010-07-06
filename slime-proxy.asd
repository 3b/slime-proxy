(defsystem :slime-proxy
  :depends-on (:parenscript :yason :clws)
  :serial t
  :components ((:file "wsproxy")
               (:file "swank-proxy")))
