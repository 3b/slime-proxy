;;; -*- Mode: LISP; slime-proxy-proxy-connection: t -*-
(in-package :slimy)

(defvar #:+swank_proxy+ (or #:+swank_proxy+ (create)))
(defvar #:+swank_proxy_impl+ (or #:+swank_proxy_impl+ (create)))

(setf (@ web-socket-js:*web-socket web-socket-js:__swf-location)
      "web-socket-js/WebSocketMain.swf"
      web-socket-js:+web_socket_swf_location+
      "web-socket-js/WebSocketMain.swf")

(setf (@ web-socket-js:*web-socket web-socket-js:__debug)
      #:true
      web-socket-js:+web_socket_debug+
      #:true)

(defvar-wrapped (+swank_proxy_impl+ ws) (or (@ +swank_proxy_impl+ ws) #:null))
(defvar-wrapped (+swank_proxy_impl+ count) 0)
(defvar-wrapped (+swank_proxy_impl+ active) nil)
(defvar-wrapped (+swank_proxy_impl+ ui) nil)

(defun-wrapped (+swank_proxy_impl+ toplevel-eval) (form)
  ((@ window eval) form))

(defun-wrapped (+swank_proxy_impl+ output) (message)
  (when ui
    (if (= message "")
        ((@ ui line) "")
        ((@ ui line) message)))
  (when (and console (@ console log)
             (/= message ""))
    ((@ console log)
     (+ "slimy: " message)))
  )
(defun-wrapped (+swank_proxy_impl+ clear-output) ()
  (when ui
    (chain ui (clear))))

(defun-wrapped (+swank_proxy_impl+ format-result) (value)
  (if (= "object" (typeof value))
      (try
       ((@ value to-source))
       (:catch (e)
         ((@ #:+json+ #:stringify) value)))
      ((@ #:+json+ #:stringify) value)))


(defun-wrapped (+swank_proxy_impl+ handle-message) (msg)
  #++(clear-output)
  #++(output (+ "onmessage:" msg))
  (try (let ((j ((@ #:+json+ #:parse)
                 msg)))
         (cond
           ((@ j "FORM")
            (let ((r (toplevel-eval (@ j "FORM"))))
              (output "")
              (output (+ "eval [" (@ j "ID")
                         "]: " (@ j "FORM")))
              (when active
                ((@ ws #:send) ((@ #:+json+ #:stringify)
                                (create "OK" #:true
                                        "RESULT" r
                                        "ID" (@ j "ID")))))
              (output (+ " ->: " (format-result
                                       r)))))
           ((@ j "ACTIVATE")
            (output (+ "@@ activate : " (@ j "ACTIVE")))
            (setf active (@ j "ACTIVE")))
           ((@ j "MESSAGE")
            (output (+ "message: " (@ j "MESSAGE")))
            (when (and (@ j "IMG") ui)
              (chain ui (html-line (ps-html
                                    ((:img :src (@ j "IMG"))))))))))
       (:catch (e)
         (when (@ j "FORM")
           (when (and console (@ console log))
             ((@ console log)
              (+ "eval error: " e)))
           (when active
             ((@ ws #:send) (+ ""
                               ((@ #:+json+ #:stringify)
                                (create "OK" #:false
                                        "RESULT" #:null
                                        "ERROR" e
                                        "ID" (@ j "ID")))))))))
  (return))

(defun-wrapped (+swank_proxy+ send-message) (message)
  (when ws
    ((@ ws #:send) message)))

(defun-wrapped (+swank_proxy+ close) ()
  (when ws
    ((@ ws #:close))))

(defun-wrapped (+swank_proxy+ connect) (&optional token)
  (when ws
    (close))
  (when (/= (typeof +swank_proxy_ui+) "undefined")
    ((@ console log) "ui")
    (setf ui +swank_proxy_ui+))
  (let ((q (chain window location search (to-string))))
    (if (and q (= (chain q (char-at 0)) "?"))
        (let ((o (chain q (substring 1)
                        (split "&"))))
          (setf q #:false)
          (loop for i in o
                for s = (chain i (split "="))
                when (and s (= (aref s 0) "token"))
                  do (setf q (aref s 1))))
        (setf q #:false))
    (output (+ "open "
               "ws://" (@ window location hostname) ":12344/swank"
               ;; this should eventually send both page= and auth=
               ;; params, once host is smart enough to parse out
               ;; the one it wants...
               "?" (if (or token q)
                       (+ "auth=" (or token q))
                       (+ "page="(@ window location pathname)))))
    (setf ws (new ((if (= (typeof #:*moz-web-socket) "undefined")
                       #:*web-socket
                       #:*moz-web-socket)
                   (+ "ws://" (@ window location hostname) ":12344/swank"
                      ;; this should eventually send both page= and auth=
                      ;; params, once host is smart enough to parse out
                      ;; the one it wants...
                      "?" (if (or token q)
                              (+ "auth=" (or token q))
                              (+ "page="(@ window location pathname))))))
          (@ ws #:onopen) (lambda ()
                            (output "onopen"))
          (@ ws #:onmessage) (lambda (e)
                               (handle-message (@ e data)))
          (@ ws #:onclose) (lambda ()
                             (output "onclose"))
          (@ ws #:onerror) (lambda ()
                             (output "onerror")))
    (output (+ "ws = " ws " state=" (@ ws #:ready-state)))))

(when (/= (typeof +swank_proxy_ui+) "undefined")
  (setf (@ console-commands "activate")
        (lambda () (output "trying to activate...") (send-message "activate")))
  (setf (@ console-commands "sync")
        (lambda () (send-message "sync")))
  (setf (@ console-commands "kick")
        (lambda () (send-message "kick me")))
  (setf (@ console-commands "connect")
        (lambda (a) (connect a))))

;; possibly this should be delayed until page finishes loading?
(let ((q (chain window location search (to-string))))
  (when (and (= (chain q (char-at 0)) "?")
             (<= 0 (chain q (substring 1)
                          (split "&")
                          (index-of "debug"))))
    (connect)))


#++
(setf (@ ((@ document get-element-by-id ) "hh") inner-h-t-m-l)
      "testing123!")
#++
(setf (@ ((@ document get-element-by-id ) "hh") inner-h-t-m-l)
      count1)

#++
((@ +swank_proxy+ send-message) "sync")

#++
((@ +swank_proxy+ send-message) "activate")
