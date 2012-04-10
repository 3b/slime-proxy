(defsystem :slimy.ui
  :defsystem-depends-on (:3b-ps-asdf)
  :depends-on (:parenscript)
  :serial t
  :components ((:file "package" :pathname "contrib/slimy/package")
               (:3b-ps-file "slimy-loader" :pathname "contrib/slimy/slimy-loader.ps")
               (:3b-ps-file "slimy-ui" :pathname "contrib/slimy/slimy-ui.ps")
               (:3b-ps-file "slimy" :pathname "contrib/slimy/slimy.ps")))