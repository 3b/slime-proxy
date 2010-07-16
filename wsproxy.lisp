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
                                     (ws::origin-prefix "http://127.0.0.1" "http://localhost"))
        res)))

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
    (handle-send-from-proxy (main-swank-proxy-resource)
                            client
                            message
                            continuation)))

(defun handle-send-from-proxy (resource client data cont)
  (declare (ignore client))
  (if (resource-clients resource)
      (ws:write-to-clients (resource-clients resource) data)
      (when cont
        (funcall cont nil t))))

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