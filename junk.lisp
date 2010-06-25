
(in-package #:swank-backend)
(defparameter *proxy-interfaces* nil)
(defparameter *imp-used* nil)
(loop for i in *interface-functions*
   do (setf (get i 'old-implementation)
            (or (get i 'old-implementation)
                (get i 'implementation))
            (get i 'implementation)
            (let ((i i))
              (lambda (&rest args)
                (when *proxy-interfaces*
                  (pushnew (format nil "impl ~s : ~s~%" i args) *imp-used*))
                (let ((f (or (if nil ;*proxy-interfaces*
                                 (gethash i *proxy-interfaces*)
                                 (get i 'old-implementation))
                             (get i 'default))))
                  (if f
                      (apply f args)
                      (error "~s not implemented (proxy=~s" i *proxy-interfaces*)))))))

(loop for i in *interface-functions*
   do (setf (get i 'implementation) (get i 'old-implementation)
            ))

*imp-used*

#++
(FIND-EXTERNAL-FORMAT
 GUESS-EXTERNAL-FORMAT
 BUFFER-FIRST-CHANGE
 RECEIVE-IF
 FRAME-RESTARTABLE-P
 PRINT-FRAME
 MAKE-LOCK
 MAKE-OUTPUT-STREAM
 COMPUTE-BACKTRACE
 CONDITION-EXTRAS
 FORMAT-SLDB-CONDITION
 THREAD-ID
 CURRENT-THREAD
 CALL-WITH-DEBUGGING-ENVIRONMENT
 CALL-WITH-DEBUGGER-HOOK
 SEND
 CALL-WITH-LOCK-HELD
 CALL-WITH-SYNTAX-HOOKS)