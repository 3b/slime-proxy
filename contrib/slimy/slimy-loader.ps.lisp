;;; -*- Mode: LISP; slime-proxy-proxy-connection: t -*-
(in-package :slimy)

;; <script type="text/javascript" src="/static/web-socket-js/swfobject.js"></script>
;; <script type="text/javascript" src="/static/web-socket-js/web_socket.js"></script>
;; <script type="text/javascript" src="/js/Function.introspect.js"></script>
;; <script type="text/javascript" src="/js/slimy-ui.js"></script>
;; <script type="text/javascript" src="/js/slimy.js"></script>
#++
((lambda ()
   (flet ((load (a)
            (chain $ (ajax (create :url a
                                   data-type :script
                                   cache true)))))
     (load "/static/web-socket-js/swfobject.js")
     (load "/static/web-socket-js/web_socket.js")
     (load "/js/Function.introspect.js")
     (load "/js/slimy.ui/slimy-ui.js")
     (load "/js/slimy.ui/slimy.js"))))

((lambda ()
   ;; todo: find some lib to load in parallel and execute in sequence
   ;; instead of blocking on each load
   (macrolet ((load (&rest a)
                (when a
                  `(chain
                    $ (ajax
                       (create :url ,(car a)
                               data-type :script
                               cache true
                               success (lambda (data status xhrs)
                                         (chain console
                                                (log "loaded " ,(car a)))
                                         (load ,@(cdr a)))))))))
     (load "/static/web-socket-js/swfobject.js"
           "/static/web-socket-js/web_socket.js"
           "/js/Function.introspect.js"
           "/js/slimy.ui/slimy-ui.js"
           "/js/slimy.ui/slimy.js"))))

