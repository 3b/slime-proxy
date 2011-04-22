(define-slime-contrib slime-parenscript
  "Interaction with other environments through SLIME and swank."
  (:authors "3b"
            "Red Daly            <reddaly@gmail.com>")
  (:license "elisp code is GPL, Common Lisp Code is BSD?")
  (:slime-dependencies slime-proxy slime-autodoc slime-c-p-c)
  (:swank-dependencies swank-parenscript)
  (:on-load
   ))

(defvar auto-parenscript-proxify-list '("\\.paren\\'"))

(defun parenscript-proxify ()
  "Enable Parenscript mode for this buffer"
  (setq slime-proxy-proxy-connection t))

(defun parenscript-proxify-maybe ()
  "Enable Parenscript mode for this buffer if its name matches
any of the regular expressions defined in
auto-parenscript-proxify-list."
  (when (find-if (lambda (re)
                   (string-match re (buffer-name (current-buffer))))
                 auto-parenscript-proxify-list)
    (parenscript-proxify)))

(defun slime-parenscript-operator-before-point ()
  (ignore-errors
    (save-excursion
      (backward-up-list 1)
      (down-list 1)
      (slime-sexp-at-point))))

(defun slime-parenscript-show-arglist ()
  (let ((op (slime-parenscript-operator-before-point)))
    (when op
      (slime-eval-async `(swank:operator-arglist ,op ,(slime-current-package))
			(lambda (arglist)
			  (if arglist
			    (slime-message "%s" arglist)))))))

(add-hook 'lisp-mode-hook 'parenscript-proxify-maybe)

(provide 'slime-parenscript)