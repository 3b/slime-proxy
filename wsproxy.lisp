(defpackage #:swank-proxy-ws
    (:use :cl)
    (:export #:main))
(in-package #:swank-proxy-ws)

(defparameter *send-marker* (gensym))
(defparameter *continuations* (make-hash-table))
(defparameter *continuations-next-id* 0)

(defclass swank-proxy-server (ws::ws-resource)
  ((ws::read-queue :allocation :class :initform (sb-concurrency:make-mailbox :name "swank-proxy-queue"))
   (clients :initform () :accessor clients)))

(setf (gethash "/swank" ws::*resources*)
      (list (make-instance 'swank-proxy-server)
            (ws::origin-prefix "http://127.0.0.1" "http://localhost")))

(defmethod ws::ws-accept-connection ((res swank-proxy-server) resource-name headers client)
  (format t "add client ~s~%" client)
  (push client (clients res))
  (values (slot-value res 'ws::read-queue) nil nil nil))

(defun handle-frame (server client data)
  (cond
    ( (or (eq data :eof)
          (eq data :dropped))
     (format t "removed client ~s~%" client)
      (setf (clients server) (delete client (clients server)))
      (ws::write-to-client client :close))
    ( (find client (clients server))
     (let* ((s (format nil "[~s] ~s~%" (position client (clients server)) data))
            (r (yason:parse data))
            (result (gethash "RESULT" r))
            (id (gethash "ID" r))
            (err (gethash "ERROR" r)))
       #++(format t "got frame ~s (~s)~%" data s)
       #++(format t "got response ~s . ~s / ~s~%" result id err)
       (if id
           (let ((cont (gethash id *continuations*)))
             (if cont
                 (progn
                   (funcall cont result err))
                 #++(format t "got cont id ~s but no cont?~%" id))
             (remhash id *continuations*))
           (swank::send-to-emacs `(:write-string ,s))))))
  )

(defun handle-send-from-proxy (server client data cont)
  (declare (ignore client))
  (if (clients server)
      (ws::write-to-clients (clients server) data)
      (when cont
        (funcall cont nil t))))

(Defun run-swank-proxy-server ()
  (let* ((server (car (gethash "/swank" ws::*resources*)))
         (q (slot-value server 'ws::read-queue)))
    (sb-concurrency:receive-pending-messages q)
    (setf (clients server) nil)
    (loop
       for (client data) = (sb-concurrency:receive-message q)
       until (eq data :kill)
       when (eq client *send-marker*)
       do (handle-send-from-proxy server (car data) (second data) (third data))
       when client
       do (handle-frame server client data)))

)
#++
(sb-concurrency:send-message (slot-value  (car (gethash "/swank" ws::*resources*)) 'ws::read-queue) '(nil :kill))


(defun proxy-send-to-client (client string &optional continuation)
  (let ((cid nil))
    (when continuation
      (setf cid (incf *continuations-next-id*)
            (gethash cid *continuations*) continuation))
      (sb-concurrency:send-message
       (slot-value (car (gethash "/swank" ws::*resources*)) 'ws::read-queue)
       (list *send-marker*
             (list client
                   (with-output-to-string (*standard-output*)
                     (yason:encode
                      (alexandria:plist-hash-table
                       (list "FORM" string
                             "ID" cid))))
                   continuation))))
  )

#++
(ws::run-server 12345)
#++
(run-swank-proxy-server)

(defun start-proxy-server ()
  (let ((con swank::*emacs-connection*))
    (sb-thread:make-thread
    (lambda ()
      (let ((swank::*emacs-connection* con))
       (ws::run-server 12345)))
    :name "socket server")

    (sb-thread:make-thread
     (lambda ()
       (let ((swank::*emacs-connection* con))
         (run-swank-proxy-server)))
     :name "resource handler")))

#++
(defun swank (&key (dont-close nil))
  (swank:create-server :coding-system "utf-8" :dont-close dont-close))









