(in-package :swank-proxy)

(defvar *swank-proxy-port* 12344
  "Port used for swank-proxy websockets server.")

(defvar *log-stream* t)
#++
(progn
  (when (streamp *log-stream*)
    (close *log-stream*))
  (setf *log-stream*
        (open "/tmp/wsproxy.log" :direction :output :if-exists :append
              :if-does-not-exist :create)))

(defun splog (format-str &rest format-args)
  "Logging function used by swank proxy."
  ;; todo: compiler-macro to pre-compile format string if possible
  (apply #'format *log-stream*
         (concatenate 'string "SWANK-PROXY: " format-str)
         format-args)
  (force-output *log-stream*))

(defparameter *continuations* (make-hash-table))
(defparameter *continuations-next-id* 0)

;; if set, default 'active client' validator will only allow active
;; clients from specified IP, ex. "127.0.0.1", set to NIL to allow
;; from anywhere
(defvar *allowed-active-client-ip* "127.0.0.1")
(defun active-client-limit-ip (client)
  (if *allowed-active-client-ip*
      (string= (clws:client-host client)
               *allowed-active-client-ip*)
      t))

;; hook for validating 'active' clients
;; client can be active if one of the functions on the list returns true
;; before one returns :reject
(defparameter *active-client-validate-hook* (list 'active-client-limit-ip))
(defun authorize-active-client (client)
  (loop for hook in *active-client-validate-hook*
     for v = (funcall hook client)
     when (eq v :reject)
     return nil
     else if v
     return t))

(defun authorize-passive-client (resource-name headers client)
  (splog "auth passive client ~s ~s" resource-name
         (alexandria:hash-table-plist headers))
  t)

(defclass swank-proxy-resource (ws:ws-resource)
  ((clients :initform () :accessor resource-clients)
   ;; responses from clients other than 'active' client will be ignored
   ;; aside from possibly being printed.
   ;; 'active' client may also get extra status messages, like
   ;; connect/disconnect, or printouts of messages from other clients
   (active-client :initform nil :accessor active-client)
   #+nil
   (clients-lock :initform (bordeaux-threads:make-lock) :reader resource-clients-lock
                 :documentation "Required to access the clients list")))

(defun swank-proxy-check-origin (c)
  (splog "check origin ~s|~%" c)
  t)

(defun main-swank-proxy-resource ()
  "Returns the websockets resource we use for pretty much everything
to do with swank proxy."
  (or (ws:find-global-resource "/swank")
      (let ((res (make-instance 'swank-proxy-resource)))
        (ws:register-global-resource "/swank"
                                     res
                                     'swank-proxy-check-origin
                                     #+nil
                                     (ws::origin-prefix "http://127.0.0.1" "http://localhost"))
        res)))

(defun activate-client (res client)
  (when (active-client res)
    (splog "deactivate old active client")
    (ws:write-to-client-text (active-client res)
                             (with-output-to-string (s)
                               (yason:encode (alexandria:plist-hash-table
                                              (list "ACTIVATE" t
                                                    "ACTIVE" nil))
                                             s))))
  (setf (active-client res) client)
  (when client
    (splog "activate new active client")
    (ws:write-to-client-text client
                             (with-output-to-string (s)
                               (yason:encode (alexandria:plist-hash-table
                                              (list "ACTIVATE" t
                                                    "ACTIVE" t))
                                             s))))
  (splog "activate done~%"))

(defun log-to-client (client message &key image)
  (ws:write-to-client-text client
                           (with-output-to-string (s)
                             (yason:encode (alexandria:plist-hash-table
                                            `("MESSAGE"
                                              ,message
                                              ,@(when image
                                                      (list "IMG" image))))
                                           s))))

(defun log-to-active (res message &key image)
  (when (active-client res)
    (log-to-client (active-client res) message :image image)))

(defmethod ws:resource-client-connected ((res swank-proxy-resource) client)
  (splog "resource-client-connected...~%")
  (splog "Swank add client ~s (~s total)~%" (clws:client-host client) (1+ (length (resource-clients res))))
  (push client (resource-clients res))
  (when (and (not (active-client res))
             (authorize-active-client client))
    (splog "adding active client from ~s~%" (clws:client-host client))
    (activate-client res client))
  (log-to-active res (format nil "client connected from ~s" (clws:client-host client)))
  nil)

;; new client defaults to sending either "page=path" where <path> is
;; window.location.pathname, or "auth=<foo>" where foo is specified
;; with token=foo in url query params, or as argument to "connect"
;; console command
;; todo: support connections for multiple pages at once?
(defvar *resource-query-token* "page=/test/test.html"
  "if set, clients will be rejected if they don't send a resource of
the form \"/swank?<token>\" where <token> is the contents of this
variable. If not set, \"/swank\" or any resource starting with
\"/swank?\" will be accepted.")

(defun check-resource-and-connection-token (resource-name query)
  ;; TODO: split up query command to allow for multiple criteria, like
  ;; specific page + auth token?
  (splog "checking client resource, token=~s, got ~s / ~s~%" *resource-query-token*
         resource-name query)
  (if *resource-query-token*
      (equalp query *resource-query-token*)
      (equal resource-name "/swank")))

(defmethod ws:resource-accept-connection ((res swank-proxy-resource) resource-name headers client)
  (and (or (authorize-active-client client)
           (authorize-passive-client resource-name headers client))
       (check-resource-and-connection-token resource-name
                                            (ws:client-query-string client))))

(defmethod ws:resource-client-disconnected ((resource swank-proxy-resource) client)
  (splog "Client disconnected from resource ~A: ~A~%" resource (clws:client-host client))
  (setf (resource-clients resource) (remove client (resource-clients resource)))
  (when (eq client (active-client resource))
    (splog "removed active client (from ~s)~%" (clws:client-host client))
    (activate-client
     resource
     (loop for i in (resource-clients resource)
        for x from 0
        when (authorize-active-client i)
        do (splog "promoting client ~s (from ~s) to active client~%" x
                  (clws:client-host i))
        and return i)))
  (log-to-active resource (format nil "client disconnected from ~s" (clws:client-host client))))

(defun valid-data-url (string)
  (labels ((base64chars (start)
             (loop
                for i from start below (length string)
                for c = (aref string i)
                ;; a-z A-Z 0-9 _-
                always (or (char<= #\a c #\z)
                           (char<= #\A c #\Z)
                           (char<= #\0 c #\9)
                           ;; not sure exactly which set of
                           ;; characters is valid, possibly should
                           ;; at least reject whitespace?
                           (char= c #\_)
                           (char= c #\-)
                           (char= c #\/)
                           (char= c #\+)
                           (char= c #\=)
                           (char= c #\space)
                           (char= c #\linefeed))))
           (try-type (type)
             (and (alexandria:starts-with-subseq type string)
                  (base64chars (length type)))))
    (or
     (try-type "data:image/gif;base64,")
     (try-type "data:image/png;base64,")
     (try-type "data:image/jpeg;base64,"))))
#++
(valid-data-url "data:image/gif;base64,R0lGODdhMAAwAPAAAAAAAP///ywAAAAAMAAw
   AAAC8IyPqcvt3wCcDkiLc7C0qwyGHhSWpjQu5yqmCYsapyuvUUlvONmOZtfzgFz
   ByTB10QgxOR0TqBQejhRNzOfkVJ+5YiUqrXF5Y5lKh/DeuNcP5yLWGsEbtLiOSp
   a/TPg7JpJHxyendzWTBfX0cxOnKPjgBzi4diinWGdkF8kjdfnycQZXZeYGejmJl
   ZeGl9i2icVqaNVailT6F5iJ90m6mvuTS4OK05M0vDk0Q4XUtwvKOzrcd3iq9uis
   F81M1OIcR7lEewwcLp7tuNNkM3uNna3F2JQFo97Vriy/Xl4/f1cf5VWzXyym7PH
   hhx4dbgYKAAA7")
#++
(valid-data-url "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAUA
AAAFCAYAAACNbyblAAAAHElEQVQI12P4//8/w38GIAXDIBKE0DHxgljNBAAO
9TXL0Y4OHwAAAABJRU5ErkJggg==")

(defmethod ws:resource-received-text ((res swank-proxy-resource) client message)
  (splog "got frame ~s~%" message)
  ;; make sure we still have a valid slime connection...
  ;;; fixme: figure out how to maintain the link to active slime
  ;;; connection properly
  ;;; 
  (unless (member swank::*emacs-connection* swank::*connections*)
    (setf swank::*emacs-connection* (swank::default-connection)))
  (when swank::*emacs-connection*
    (cond
      ((and (eql client (active-client res))
            (string= message "sync"))
       (splog "clearing stale continuations...~%")
       (loop for i in (loop for i being the hash-keys of *continuations*
                            collect i)
             for c = (gethash i *continuations*)
             when c
               do
                  (swank::send-to-emacs
                   `(:write-string
                     ,(format nil "cancelling continuation ~s~^%" i)
                     :proxy))
                  (funcall c nil "cancelled")
                  (remhash i *continuations*)))
      ((and (string= message "activate"))
       (cond
         ((eql client (active-client res))
          (log-to-client client "already active"))
         ((authorize-active-client client)
          (splog "switching active client...~%")
          (activate-client res client))
         (t
          (log-to-client client "login failed"))))
      ((string= message "kick me")
       (ws:write-to-client-close client :code 31337 :message "Pwned"))
      ((valid-data-url message)
       (log-to-active res (format nil "image [~s]" (position client (resource-clients res))) :image message))
      ((eql client (active-client res))
       (let* ((string-for-emacs (format nil "[~s] ~s~%" (position client (resource-clients res)) message))
              (r (ignore-errors (yason:parse message)))
              (result (when (and r (hash-table-p r)) (gethash "RESULT" r)))
              (id (when (and r (hash-table-p r)) (gethash "ID" r)))
              (err (when (and r (hash-table-p r)) (gethash "ERROR" r))))
         #++(format t "got frame ~s (~s)~%" data s)
         (splog "got response ~s . ~s / ~s~%" result id err)
         (if id
             (let ((cont (gethash id *continuations*)))
               (if cont
                   (if err
                       (funcall cont nil err)
                       (funcall cont t result))
                   #++(format t "got cont id ~s but no cont?~%" id))
               (remhash id *continuations*))
             (swank::send-to-emacs `(:write-string ,string-for-emacs :proxy)))))
      (t
       (let* ((string-for-emacs (format nil "[~s] ~s~%" (position client (resource-clients res)) message)))
         (log-to-active res string-for-emacs)

         (swank::send-to-emacs `(:write-string ,string-for-emacs :proxy)))))))

;; fixme: make this actually be thread-safe.  To do so we should
;; probably improve how message-passing works in clws and use some
;; exported functionality from there to pass messages to the resource
;; thread.
(defun proxy-send-to-clients (form &key continuation clients (resource (main-swank-proxy-resource)))
  "Version of proxy-send-to-client that may be called from any thread.

FORM is a string or some other JSON-encodable form that is send to the
client.

CLIENTS is either a specific list of clients or NIL, in which case it
stands for all the clients of the given resource.

CONTINUATION will be executed on the resource thread upon receipt of a
response from the client, and passed 2 arguments: (1) T if the
evaluation went okay or not, and (2) the result.

RESOURCE is the swank resource"
  (declare (type string form))
  (ws:call-on-resource-thread
   resource
   (lambda ()
     (setf clients (or clients (resource-clients resource)))
     (let* ((cid (when continuation
                   ;; fixme: probably need to clear out the continuations hash every
                   ;; once in a while?
                   (let ((cid (incf *continuations-next-id*)))
                     (setf (gethash cid *continuations*) continuation)
                     cid)))
            (message (with-output-to-string (s)
                       (yason:encode (alexandria:plist-hash-table
                                      (list "FORM" form
                                            "ID" cid))
                                     s))))
       (when (resource-clients resource)
         (ws:write-to-clients-text clients message))
       (unless (active-client resource)
         ;; if there are no active clients connected, call the continuation with not-ok

         (when continuation
           (funcall continuation nil "Not connected to any proxy clients.")))))))

;; This should only execute on the thread listening for
;; proxy-channel events. send-form-to-client is the thread-safe version
(defun proxy-send-to-client (client form &optional continuation)
  "Send a form (string) to the client.  If client is nil, then send to
all clients.  As soon as a message is back from the client,
continuation is called with 2 arguments: (1) T if the evaluation went
okay or not, and (2) the result."
  (declare (type string form))
  (proxy-send-to-clients form
                         :continuation continuation
                         :clients (when client (list client))))


(defvar *swank-proxy-ws-thread* nil
  "Thread executing the websockets event-loop.")

(defvar *swank-proxy-resource-thread* nil
  "Thread executing the websockets resource event-loop.")

(defun start-websockets-proxy-server (&key kill-existing (port *swank-proxy-port*))
  (macrolet ((maybe-kill (special)
               `(progn
                  (when (and ,special (not (bordeaux-threads:thread-alive-p ,special)))
                    (setf ,special nil))
                  (when (and kill-existing ,special)
                    (bordeaux-threads:destroy-thread ,special)
                    (setf ,special nil))))
             (maybe-setf (special value)
               `(unless ,special
                  (setf ,special ,value))))

    (maybe-kill *swank-proxy-ws-thread* )
    (maybe-kill *swank-proxy-resource-thread*)

    (let ((con swank::*emacs-connection*))
      (maybe-setf *swank-proxy-ws-thread*
                  (bordeaux-threads:make-thread
                   (lambda ()
                     (swank::with-connection (con)
                       (ws:run-server port)))
                   :name "swank-proxy websockets server"))

      (maybe-setf *swank-proxy-resource-thread*
                  (bordeaux-threads:make-thread
                   (lambda ()
                     (swank::with-connection (con)
                       (run-swank-proxy-resource-server)))
                   :name "swank-proxy resource handler"))))
  (list *swank-proxy-ws-thread* *swank-proxy-resource-thread*))

(defun run-swank-proxy-resource-server ()
  "Starts up a swank proxy resource server in the current thread."
  (let ((res (main-swank-proxy-resource)))
    (setf (resource-clients res) nil)
    (ws:run-resource-listener res)))

#++
(start-websockets-proxy-server :kill-existing t)
