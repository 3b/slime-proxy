(in-package #:swank)


(defun ps-arglist-dispatch (op args)
  (declare (ignorable args))
  #++(let ((*package* (find-package :keyword)))
    (format t "p-a-d ~s ~s~%" op args)
    )
  (when (ps::parenscript-function-p op)
    ;(format t "p-a-d -> ~s~%" (ps::parenscript-arglist op))
    (with-available-arglist (decoded-arglist) 
        (decode-arglist (ps::parenscript-arglist op))
      #++(enrich-decoded-arglist-with-extra-keywords decoded-arglist
                                                  (cons op args))
      (values decoded-arglist nil nil))))

;;;; autodoc hooks
;;fixme: separate this into a separate file, only load with autodoc contrib
(defmethod arglist-dispatch :around (op argument)
  ;(format t "arglist dispatch ~s / ~s~T" op argument)
  (when (next-method-p)
    (call-next-method))
  #++((ps::parenscript-function-p sym)
             (let ((args (ps::parenscript-arglist sym)))
               (format t "found s function, args=~s~%" args)
               (cond ((eq args :not-available) nil)
                     (t (princ-to-string (cons name args))))))
)


(defmethod arglist-dispatch :around (op argument)
 ; (format t "arglist dispatch ~s / ~s~T" op argument)
  (when (next-method-p)
    (call-next-method))
  #++((ps::parenscript-function-p sym)
             (let ((args (ps::parenscript-arglist sym)))
               (format t "found s function, args=~s~%" args)
               (cond ((eq args :not-available) nil)
                     (t (princ-to-string (cons name args))))))
)

(defmethod arglist-dispatch (operator arguments)
  ;(format t "--dispatch--~%")
    (cond
      ((and (symbolp operator) (valid-operator-symbol-p operator))
       (multiple-value-bind (decoded-arglist determining-args)
           (compute-enriched-decoded-arglist operator arguments)
         (with-available-arglist (arglist) decoded-arglist
           ;; replace some formal args by determining actual args
           (setf arglist (delete-given-args arglist determining-args))
           (setf (arglist.provided-args arglist) determining-args)
           arglist)))
      ((find-if (lambda (hook) #++(format t "trying hook ~s~%" hook) (funcall hook operator arguments)) *arglist-dispatch-hooks*))
      (t (return-from arglist-dispatch :not-available)))) 