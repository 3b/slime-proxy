(define-slime-contrib slime-proxy
  "Interaction with other environments through SLIME and swank."
  (:authors "3b"
            "Red Daly            <reddaly@gmail.com>")
  (:license "elisp code is GPL, Common Lisp Code is BSD?")
  (:slime-dependencies slime-repl)
  (:swank-dependencies swank-proxy swank-repl)
  (:on-load
   ))

(defgroup slime-proxy nil
  "Interaction with other environments through SLIME."
  :prefix "slime-proxy-"
  :group 'slime)

(defvar slime-proxy-event-loop nil)
(defvar slime-proxy-most-recent-channel-id nil)

(slime-def-connection-var slime-connection-proxy-output-buffer nil
  "The buffer for the REPL.  May be nil or a dead buffer.")

(make-variable-buffer-local
 (defvar slime-proxy-proxy-connection nil))

(defun slime-proxy-output-buffer ()
  "Returns the proxy REPL buffer based on the current buffer."
  ;; FIXME: use a per-buffer variable instead of a per-connection variable
  (let* ((slime-dispatching-connection (slime-proxy-connection)))
    (slime-connection-proxy-output-buffer)))


(defun slime-proxy-connection ()
  "Returns the most relevant proxy connection."
  (flet ((test-process (process)
            (let ((slime-dispatching-connection process))
              (slime-with-connection-buffer (process)
                (when (slime-connection-proxy-output-buffer)
                  t)))))
    (find-if #'test-process (cons (slime-current-connection)
                                  slime-net-processes))))

(defmacro with-proxy-output-buffers (&rest body)
  `(letf (((slime-connection-output-buffer) (slime-proxy-output-buffer)))
     ,@body))

(defun slime-proxy-repl-write-string (string &optional target)
  (case target
    ((or :proxy-repl-result :proxy)
     (with-proxy-output-buffers
      (slime-proxy-repl-write-string string nil)))
    (t
     (funcall 'slime-repl-write-string string target))))

(setq slime-write-string-function 'slime-proxy-repl-write-string)

(defun slime-proxy (target)
  "Open up a slime proxy instance through the remote swank, and
launch a REPL for the proxy."
  (interactive "sTarget for proxy: ")
  (slime-proxy-open-listener target))

(defun slime-proxy-open-listener (target)
  "Create a new listener window."
  ;; create emacs-side channel struct
  (let ((channel (slime-make-channel nil "slime-proxy-channel")))
    ;; now create the swank-side proxy listener
    (slime-eval-async
        `(swank:create-proxy-listener ,(slime-channel.id channel) ,target)
      (slime-rcurry
       (lambda (result channel)
         (let ((slime-dispatching-connection (slime-connection)))
           (destructuring-bind (remote thread-id package prompt) result
             (setq slime-proxy-most-recent-channel-id remote)
             (pop-to-buffer (generate-new-buffer (slime-buffer-name :proxy-repl)))
             (slime-repl-mode)
             (setq slime-proxy-proxy-connection t)
             (setq slime-current-thread thread-id)
                                        ;(message "New buffer with slime connection=%s" (slime-connection))

             (setq slime-buffer-connection (slime-connection))
             (setf slime-buffer-package package)

             (setf (slime-connection-proxy-output-buffer) (current-buffer))

             (set (make-local-variable 'slime-proxy-remote-channel) remote)
             (slime-channel-put channel 'buffer (current-buffer))
             (slime-reset-repl-markers)
                                        ;(slime-channel-send channel `(:prompt ,package ,prompt))

             (letf (((slime-lisp-package-prompt-string) (or prompt "PAREN")))
               (slime-repl-insert-prompt))
             (slime-repl-show-maximum-output))))
       channel))))

(defun slime-proxy-connected-p ()
  "Returns T if we are connected to a proxy server."
  (let ((slime-dispatching-connection (slime-proxy-connection)))
    (and slime-dispatching-connection
         (slime-connected-p)
         (slime-connection-proxy-output-buffer)
         t)))

(defun slime-proxy-event-hook-function (event)
  (cond
   ((and slime-proxy-proxy-connection
         (slime-proxy-connected-p)
         (not slime-proxy-event-loop))
    (let ((slime-proxy-event-loop t)
          (proxy slime-proxy-proxy-connection)
          (slime-proxy-proxy-connection nil)
          (slime-dispatching-connection (slime-proxy-connection)))
                                        ; (message "sending proxied msg %s - %s" proxy event)
      (destructure-case event
        ((:emacs-interrupt thread)
         (slime-send `(:emacs-interrupt ,thread)))
        ((:emacs-rex form package thread continuation)
         (when (and (slime-use-sigint-for-interrupt) (slime-busy-p))
           (slime-display-oneliner "; pipelined request... %S" form))
         (let ((id (incf (slime-continuation-counter))))
                                        ;(message "proxied message, id=%s" id)
                                        ;(message "proxied message, form=%s" form)
           (slime-send `(:emacs-channel-send
                         ,slime-proxy-most-recent-channel-id
                         (:proxy (:emacs-rex ,form ,package ,thread ,id))) )

           ;; wrap the continuation to execute in the proxy's environment
           (lexical-let* ((original continuation)
                          (wrapped (lambda (result)
                                     (with-proxy-output-buffers
                                      (let ((slime-proxy-wrapped-continuation t))
                                        (funcall original result))))))
             (push (cons id wrapped)
                   (slime-rex-continuations)))
                                        ;(message "adjusted continuations (added %i for %s): %s"
                                        ;         id (slime-connection) (mapcar 'car (slime-rex-continuations)))
           (slime-recompute-modelines)))
        ((:buffer-first-change)
         nil)
        ((:operator-arglist )
         (message "slime-proxy ignorning slime: %s" event)
         nil)
;;; fixme dont have proxy-event caught in two places.  Use
;;; only the one below, and get rid of the if/else separating
;;; these.
        ((:proxy-event wrapped-event package prompt-string)
         (case wrapped-event
           (:new-package
            (setf (slime-lisp-package) package)
            (setf (slime-lisp-package-prompt-string) prompt-string)
            (let ((buffer (slime-connection-output-buffer)))
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (setq slime-buffer-package package)))))
           (t (message "slime-proxy ignoring proxy event: %s" wrapped-event)))
         t)

        (t
         (message "slime-proxy ignorning slime: %s" event)))
      t))
   ((and (slime-proxy-connected-p)
         (not slime-proxy-event-loop))
    (destructure-case event
      ((:proxy-event wrapped-event package prompt-string)
       (case wrapped-event
         (:new-package
          ;; fixme: need a proxy version of slime-lisp-package, which
          ;; requires making slime-lisp-package buffer-specific, not
          ;; connection-specific
          (setf (slime-lisp-package) package)
          (setf (slime-lisp-package-prompt-string) prompt-string)
          (lexical-let ((buffer (slime-connection-proxy-output-buffer)))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (setq slime-buffer-package package)))))
         (t (message "slime-proxy ignoring proxy event: %s" wrapped-event)))
       t)
      (t nil)))
   (t nil)))

(defun slime-proxy-initialize-macroexpansion-buffer-hook-function (buffer)
  "Put the macroexpansion buffer into proxy mode."
  (with-current-buffer buffer
    (message "macroexpansion hook %s and continuation? %s"
             buffer
             slime-proxy-wrapped-continuation)
    (setq slime-proxy-proxy-connection
          (and slime-proxy-wrapped-continuation t))))

(add-hook 'slime-event-hooks 'slime-proxy-event-hook-function)
(add-hook 'slime-initialize-macroexpansion-buffer-hook
          'slime-proxy-initialize-macroexpansion-buffer-hook-function)

;;; todo: on slime-net-process-close-hooks, check for proxy connection closing



(defvar slime-echo-arglist-function 'slime-show-arglist-ps)

(defun slime-show-arglist-ps ()
  (let ((op (slime-operator/form-before-point)))
    (when op
      (slime-eval-async `(swank:operator-arglist ,op ,(slime-current-package))
			(lambda (arglist)
			  (when arglist
			    (slime-message "%s" arglist)))))))

(defun slime-operator/form-before-point ()
  (ignore-errors
    (save-excursion
      (backward-up-list 1)
      (down-list 1)
      (slime-sexp-at-point))))

(provide 'slime-proxy)