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

(add-hook 'lisp-mode-hook 'parenscript-proxify-maybe)