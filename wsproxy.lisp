(defpackage #:swank-proxy-ws
    (:use :cl :anaphora)
    (:export #:main))

(in-package #:swank-proxy-ws)

(defun splog (format-str &rest format-args)
  "Logging function used by swank proxy."
  (apply #'format t
         (concatenate 'string "SWANK-PROXY: " format-str)
         format-args))

(defparameter *continuations* (make-hash-table))
(defparameter *continuations-next-id* 0)

(defclass swank-proxy-resource (ws:ws-resource)
  ((clients :initform () :accessor resource-clients)
   (clients-lock :initform (bordeaux-threads:make-lock) :reader resource-clients-lock
                 :documentation "Required to access the clients list")))

(ws:register-global-resource 
 "/swank"
 (make-instance 'swank-proxy-resource)
 (ws::origin-prefix "http://127.0.0.1" "http://localhost"))

(defun run-swank-proxy-server ()
  "Starts up a swank proxy server in the current thread."
  (let ((res (ws:find-global-resource "/swank")))
    (setf (resource-clients res) nil)
    (ws:run-resource-listener res)))

(defmethod ws:resource-accept-connection ((res swank-proxy-resource) resource-name headers client)
  (splog "Swank add client ~s~%" client)
  (push client (resource-clients res))
  (equal "/swank" resource-name))

(defmethod ws:resource-client-disconnected ((resource swank-proxy-resource) client)
  (setf (resource-clients resource) (remove client (resource-clients resource))))

(defmethod ws:resource-received-frame ((res swank-proxy-resource) client message)
  (when (find client (resource-clients res))
    (let* ((string-for-emacs (format nil "[~s] ~s~%" (position client (resource-clients res)) message))
           (r (yason:parse message))
           (result (gethash "RESULT" r))
           (id (gethash "ID" r))
           (err (gethash "ERROR" r)))
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
          (swank::send-to-emacs `(:write-string ,string-for-emacs))))))

(defun proxy-send-to-client (client string &optional continuation)
  ""
  (let* ((cid (when continuation
                (let ((cid (incf *continuations-next-id*)))
                  (setf (gethash cid *continuations*) continuation)
                  cid)))
         (message (with-output-to-string (s)
                   (yason:encode (alexandria:plist-hash-table
                                  (list "FORM" string
                                        "ID" cid))
                                 s))))
    ;; used to call handle-send-from-proxy from the resource loop, but
    ;; I see no need to do this asynchronously right now
    (handle-send-from-proxy (ws:find-global-resource "/swank")
                            client
                            message
                            continuation)))

(defun handle-send-from-proxy (resource client data cont)
  (declare (ignore client))
  (if (resource-clients resource)
      (ws:write-to-clients (resource-clients resource) data)
      (when cont
        (funcall cont nil t))))

#++
(ws::run-server 12345)
#++
(run-swank-proxy-server)

(defun start-proxy-server ()
  (let ((con swank::*emacs-connection*))
    (bordeaux-threads:make-thread
     (lambda ()
       (let ((swank::*emacs-connection* con))
         (ws::run-server 12344)))
     :name "swank-proxy socket server")

    (bordeaux-threads:make-thread
     (lambda ()
       (let ((swank::*emacs-connection* con))
         (run-swank-proxy-server)))
     :name "swank-proxy resource handler")))

#++
(defun swank (&key (dont-close nil))
  (swank:create-server :coding-system "utf-8" :dont-close dont-close))









