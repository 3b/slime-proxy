(defsystem :slime-proxy-ps
  :depends-on (:slime-proxy)
  :serial t
  :components ((:file "swank-proxy-ps")))
