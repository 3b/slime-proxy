;;; -*- Mode: LISP; slime-proxy-proxy-connection: t -*-
(in-package :slimy)

(defvar #:+swank_proxy_ui+ (or #:+swank_proxy_ui+ (create)))

(defun-wrapped (+swank_proxy_ui+ line) (message)
  (let ((cc ($ "#slime-proxy-console-output")))
    (chain cc
           (append (ps-html ((:div :style "white-space:pre-wrap")))))
    (chain cc (children) (last)
           (text message)))
  (let ((cc ($ "#slime-proxy-console-output")))
    (chain cc
           (scroll-top (- (chain cc (prop "scrollHeight"))
                          (chain cc (height)))))))

(defun-wrapped (+swank_proxy_ui+ html-line) (message)
  (let ((cc ($ "#slime-proxy-console-output")))
    (chain cc
           (append (ps-html ((:div :style "white-space:pre-wrap") message)))
           (scroll-top (- (chain cc (prop "scrollHeight"))
                          (chain cc (height)))))))

(defvar-wrapped (+swank_proxy_ui+ console-commands) (create))

(defun-wrapped (+swank_proxy_ui+ handle-console-input) (input)
  ;;default input parser, not very smart... eventually should be configurable
  (let* ((parts (chain input (split " ")))
         (cmd (aref console-commands (chain parts (shift)))))
    (chain ($ "#slime-proxy-console-input-text") (val ""))
    ;; fixme: decide where this goes or conditionalize it or something
    ((@ +swank_proxy+ send-message) (+ "input: " input))
    (line (+ "> " input ""))
    (when cmd
      (apply cmd parts))))

(defun-wrapped (+swank_proxy_ui+ input-submit-handler) ()
  (let* ((input (chain ($ "#slime-proxy-console-input-text") (val))))
    (handle-console-input input))
  #:false)

(defun-wrapped (+swank_proxy_ui+ toggle-minimize) ()
  (chain ($ "#slime-proxy-console")
         (fade-toggle "slow"))
  (chain ($ "#slime-proxy-icon")
         (fade-toggle "slow"))
  nil)

(defun-wrapped (+swank_proxy_ui+ show-console) ()
  (chain ($ "#slime-proxy-console")
         (fade-in "slow"))
  (chain ($ "#slime-proxy-icon")
         (fade-out "slow"))
  nil)

(defun-wrapped (+swank_proxy_ui+ init) ()
  (let ((ci ($ "#slime-proxy-icon")))
    (when (= 0 (chain ci (size)))
      (chain ($ "body")
             (append (ps-html ((:div :id "slime-proxy-icon") "ci"))))
      (setf ci ($ "#slime-proxy-icon")))
    (chain ci
           (css (create "position" "fixed"
                        "bottom" "0"
                        "left" "0"
                        "color" "green"
                        "border" "1px solid green"
                        "padding" "0"
                        "margin" "0"
                        "background" "rgb(0,0,0) transparent"
                        "background" "rgba(0,0,0,0.9)"
                        "height" "1em"
                        "width" "1em"
                        "text-align" "center"
                        "text-shadow" "0 0 0.8em green, 0 0 5em green"))
           (text "~")
           (click (lambda () (toggle-minimize)))))

  (let ((cc ($ "#slime-proxy-console")))
    ;; create a div to use for the UI if it doens't already exist
    (when (= 0 (chain cc (size)))
      (chain ($ "body")
             (append (ps-html ((:div :id "slime-proxy-console") "cc"))))
      (setf cc ($ "#slime-proxy-console")))
    ;; and configure it TODO: move more of this to separate .css?
    (chain cc
           (css (create "position" "fixed"
                        "bottom" "0"
                        "left" "1em"
                        "color" "green"
                        "border" "green"
                        "border-style" "double"
                        "padding" "0.7em"
                        ;; see http://stackoverflow.com/questions/806000/css-semi-transparent-background-but-not-text for IE versions...
                        "background" "rgb(0,0,0) transparent"
                        "background" "rgba(0,0,0,0.9) "
                        "opacity" "1"
                        "height" "auto"
                        ;"max-height" "30em"
                        "width" "60em"
                        "overflow" "visible"
                        "font-family" "monospace"
                        "font-size" "8pt"))
           (html
            (ps-html
             ((:div :id "slime-proxy-console-close"
                    :style "position:absolute;left:0;top:0;width:1em;height:1em;text-align:center;border:solid green; border-width:0 1px 1px;font-size:1.3em;")
              "X")
             ((:div :id "slime-proxy-console-output"
                    :style "overflow:auto; max-height:30em;")
              (:h2 "..."))
             ((:div :id "slime-proxy-console-input"
                    :style "border-top:1px solid green"
                    :width "auto")
              ((:span :style "position:absolute;margin:none") "> ")
              ((:form :id "slime-proxy-console-input-form"
                      :action "#"
                      :style "width:auto;margin:0;padding:0;border:0"
                      :onsubmit (ps-inline (progn (input-submit-handler)
                                                  (return #:false))))
               ((:input :type :text
                        :id "slime-proxy-console-input-text"
                        :style "text-indent:1.5em;margin:0;width:100%;background:none;color:green;border:0"
                        :on))))))
           (hide))
    (chain ($ "#slime-proxy-console-close") (click (lambda () (toggle-minimize)))))
  (let*((q (chain window location search (to-string)))
        (p (and (= (chain q (char-at 0)) "?")
                (chain q (substring 1)
                       (split "&")))))
    (when (and (not (<= 0 (chain p (index-of "console=off"))))
               (or (<= 0 (chain p (index-of "debug")))
                   (<= 0 (chain p (index-of "console")))
                   (<= 0 (chain p (index-of "console=on")))))
      (show-console)))
  nil)
#++
(init)

(defun-wrapped (+swank_proxy_ui+ embed) ()
  (chain ($ "#slime-proxy-console") (css (create "position" "relative"))))
(defun-wrapped (+swank_proxy_ui+ float) ()
  (chain ($ "#slime-proxy-console") (css (create "position" "fixed"))))
(defun-wrapped (+swank_proxy_ui+ clear) ()
  (chain ($ "#slime-proxy-console-output")
         (empty)))


;;; default console commands

;; todo: useful help message
(setf (@ console-commands "?") (lambda () (line "halp!"))
      (@ console-commands "help") (@ console-commands "?"))

(setf (@ console-commands "clear") (lambda () (clear)))

(setf (@ console-commands "look")
      (lambda ()
        (line "You are on an infinite, featureless plane. You can go N,E,S,W."))
      (@ console-commands "l") (@ console-commands "look"))
(setf (@ console-commands "N") (lambda () (line "You go north."))
      (@ console-commands "n") (@ console-commands "N"))
(setf (@ console-commands "S") (lambda () (line "You go south."))
      (@ console-commands "s") (@ console-commands "S"))
(setf (@ console-commands "E") (lambda () (line "You go east."))
      (@ console-commands "e") (@ console-commands "E"))
(setf (@ console-commands "W") (lambda () (line "You go west."))
      (@ console-commands "w") (@ console-commands "W"))

(setf (@ console-commands "ls")
      (lambda ()
        (line ". ..")
        (line "It's a UNIX system. I know this!")))

(setf (@ console-commands "(+")
      (lambda () (line "3")))


;;; initialize console once document is loaded
(chain ($ "document") (ready (lambda ()
                               ((@ console log) "ready")
                               ((@ +swank_proxy_ui+ init)))))

#++
(ps:chain ($ "#hh") (hide "fast"))
#++
(ps:chain ($ "#hh") (show "slow"))

#++
(ps:chain ($ "#hh") (parent) (append "<h1>test2</h1>"))
#++
(ps:chain ($ "body") (append "<h1>test3</h1>")
          (children)
          (last)
          (hide)
          (show "slow")
          (end) (css (create "border" "0")) (end)
          (css (create "border" "1px solid green")))


#++
(line "foo23!")

#++
(chain ($ "#slime-proxy-console") (get 0))


