

;;(defun slime-proxy-connect (&optional connection)
;;  "Connect to a swank proxied behind an existing connection."
;;  (let ((old-connection (slime-current-connection)))
;;    (message "Connecting to Swank proxy through ~s.." old-connection))
;;  (with-current-buffer (get-buffer-create "*inferior-lisp-proxy*")
;;    (let* ((inhibit-quit nil)
;;           (proc (get-buffer-process (current-buffer)))
;;           )
;;      ;;(lisp-mode-variables t)
;;      (slime-set-query-on-exit-flag proc)
;;      (run-hooks 'slime-inferior-process-start-hook)
;;      proc
;;
;;      (set-process-buffer proc buffer)
;;      (set-process-filter proc 'slime-net-filter)
;;      (set-process-sentinel proc 'slime-net-sentinel)
;;      (slime-set-query-on-exit-flag proc)
;;      (when (fboundp 'set-process-coding-system)
;;        (slime-check-coding-system coding-system)
;;        (set-process-coding-system proc coding-system coding-system))
;;      (when-let (secret (slime-secret))
;;        (slime-net-send secret proc))
;;      proc))
;;  (let ((coding-system (or coding-system slime-net-coding-system)))
;;    (slime-check-coding-system coding-system)
;;    (message "Connecting to Swank on port %S.." port)
;;    (let* ((process (slime-net-connect host port coding-system))
;;           (slime-dispatching-connection process))
;;      (slime-setup-connection process))))
;;
;(slime-def-connection-var slime-proxy-connection nil
;  "connection used as a proxy by this connection.")

(define-slime-contrib slime-proxy
  "Interaction with other environments through SLIME and swank."
  (:authors "3b"
            "Red Daly            <reddaly@gmail.com>")
  (:license "elisp code is GPL, Common Lisp Code is BSD?")
  (:slime-dependencies );slime-proxy)
  (:swank-dependencies swank-proxy)
  (:on-load
   ))

(defgroup slime-proxy nil
  "Interaction with other environments through SLIME."
  :prefix "slime-proxy-"
  :group 'slime)

(defvar slime-proxy-event-loop nil)
(defvar slime-proxy-most-recent-channel-id 1)

(make-variable-buffer-local
 (defvar slime-proxy-proxy-connection nil))

(defun slime-proxy-open-listener (target)
  "Create a new listener window."
  (interactive "sTarget for proxy: ")
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
            (pop-to-buffer (generate-new-buffer (slime-buffer-name :proxy-scratch)))
            (slime-repl-mode)
            (setq slime-proxy-proxy-connection t)
            (setq slime-current-thread thread-id)
            ;(message "New buffer with slime connection=%s" (slime-connection))
            (setq slime-buffer-connection (slime-connection))
            (set (make-local-variable 'slime-proxy-remote-channel) remote)
            (slime-channel-put channel 'buffer (current-buffer))
            (slime-reset-repl-markers)
                                        ;(slime-channel-send channel `(:prompt ,package ,prompt))
            (setf slime-buffer-package package)
            (letf (((slime-lisp-package-prompt-string) (or prompt "PAREN")))
              (slime-repl-insert-prompt))
            (slime-repl-show-maximum-output))))
      channel))))


(defun slime-proxy-event-hook-function (event)
  (if (and slime-proxy-proxy-connection
           (not slime-proxy-event-loop))
      (let ((slime-proxy-event-loop t)
            (proxy slime-proxy-proxy-connection)
            (slime-proxy-proxy-connection nil)
            (slime-dispatching-connection (slime-connection)))
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
             (push (cons id continuation) (slime-rex-continuations))
             ;(message "adjusted continuations (added %i for %s): %s" 
             ;         id (slime-connection) (mapcar 'car (slime-rex-continuations)))
             (slime-recompute-modelines)))
          ((:buffer-first-change)
           nil)
          ((:operator-arglist )
           nil)
)

       ; (slime-send event )
        t)
      nil))

(add-hook 'slime-event-hooks 'slime-proxy-event-hook-function)

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
