(defpackage :swank-proxy
    (:use :cl :anaphora :alexandria)
  (:export #:proxy-eval
           #:proxy-create-channel
           #:proxy-channel
           #:proxy-listener-channel
           #:proxy-send-to-client
           #:channel-target
           #:start-websockets-proxy-server
           #:start-swank-proxy-server
           #:*swank-proxy-port*))

(provide :swank-proxy)