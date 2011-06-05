(in-package :swank-proxy)

(defvar *swank-proxy-port* 12344
  "Port used for swank-proxy websockets server.")

(defun splog (format-str &rest format-args)
  "Logging function used by swank proxy."
  (apply #'format t
         (concatenate 'string "SWANK-PROXY: " format-str)
         format-args))

(defparameter *continuations* (make-hash-table))
(defparameter *continuations-next-id* 0)

(defclass swank-proxy-resource (ws:ws-resource)
  ((clients :initform () :accessor resource-clients)
   #+nil
   (clients-lock :initform (bordeaux-threads:make-lock) :reader resource-clients-lock
                 :documentation "Required to access the clients list")))

(defun main-swank-proxy-resource ()
  "Returns the websockets resource we use for pretty much everything
to do with swank proxy."
  (or (ws:find-global-resource "/swank")
      (let ((res (make-instance 'swank-proxy-resource)))
        (ws:register-global-resource "/swank"
                                     res
                                     #'ws::any-origin 
                                     #+nil
                                     (ws::origin-prefix "http://127.0.0.1" "http://localhost"))
        res)))

(defmethod ws:resource-accept-connection ((res swank-proxy-resource) resource-name headers client)
  (splog "Swank add client ~s~%" client)
  (push client (resource-clients res))
  (equal "/swank" resource-name))

(defmethod ws:resource-client-disconnected ((resource swank-proxy-resource) client)
  (format t "Client disconnected from resource ~A: ~A~%" resource client)
  (setf (resource-clients resource) (remove client (resource-clients resource))))

(defmethod ws:resource-received-frame ((res swank-proxy-resource) client message)
  (when (string= message "sync")
    (loop for i in (loop for i being the hash-keys of *continuations*
                      collect i)
       for c = (gethash i *continuations*)
       when c
       do
         (swank::send-to-emacs `(:write-string
                                 ,(format nil "cancelling continuation ~s~^%" i)
                                 :proxy))
         (funcall c nil "cancelled")
         (remhash i *continuations*)))
  (when (string= message "kick me")
    (ws:write-to-client client :close))
  (when (find client (resource-clients res))
    (let* ((string-for-emacs (format nil "[~s] ~s~%" (position client (resource-clients res)) message))
           (r (ignore-errors (yason:parse message)))
           (result (when (and r (hash-table-p r)) (gethash "RESULT" r)))
           (id (when (and r (hash-table-p r)) (gethash "ID" r)))
           (err (when (and r (hash-table-p r)) (gethash "ERROR" r))))
      #++(format t "got frame ~s (~s)~%" data s)
      #++(format t "got response ~s . ~s / ~s~%" result id err)
      (if id
          (let ((cont (gethash id *continuations*)))
            (if cont
                (if err
                    (funcall cont nil err)
                    (funcall cont t result))
                #++(format t "got cont id ~s but no cont?~%" id))
            (remhash id *continuations*))
          (swank::send-to-emacs `(:write-string ,string-for-emacs :proxy))))))

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
       (if (resource-clients resource)
           (ws:write-to-clients clients message)
           ;; if there are no clients connected, call the continuation with not-ok
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
                   (let ((swank::*emacs-connection* con))
                     (ws:run-server port)))
                 :name "swank-proxy websockets server"))

    (maybe-setf *swank-proxy-resource-thread*
                (bordeaux-threads:make-thread
                 (lambda ()
                   (let ((swank::*emacs-connection* con))
                     (run-swank-proxy-resource-server)))
                 :name "swank-proxy resource handler"))))
  (list *swank-proxy-ws-thread* *swank-proxy-resource-thread*))

(defun run-swank-proxy-resource-server ()
  "Starts up a swank proxy resource server in the current thread."
  (let ((res (main-swank-proxy-resource)))
    (setf (resource-clients res) nil)
    (ws:run-resource-listener res)))