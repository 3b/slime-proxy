### Slime proxy

A hack for using slime with things like parenscript and 3bil that
compile code on a host lisp then run it somewhere else.

Currently only usable with parenscript, using websockets for
communication between host lisp and browser.

#### Requirements:
* currently only works on sbcl, since clws hasn't been ported to other lisps yet
* [modified parenscript](http://github.com/3b/parenscript)
* [clws](http://github.com/3b/clws)
* [websockets emulation](http://github.com/gimite/web-socket-js) for browsers without native support
* misc libraries: ironclad, anaphora, cl-ppcre, yason

#### Loading:

load slime-proxy/slime-proxy.el into emacs, then `M-x eval-buffer`

load/start proxy stuff (listens on port 12345 by default, adjust in wsproxy.lisp if needed)

    (require 'slime-proxy)
    (swank-proxy-ws::start-proxy-server)
    (swank::create-proxy-listener)

adjust URLs in slime-proxy/slimy.html

`/wsjs/*` &rarr;  files from web-socket-js

`ws://127.0.0.1:12345/` &rarr; host/port to use to connect to websocket serrver, if changed

(if clients are loading page from somewhere other than localhost, add the host to the `(ws:origin-prefix "http..." ...)` form in wsproxy,lisp)

load slimy.html in browser

switch an emacs buffer to proxy mode:
`M-: (setq slime-proxy-proxy-connection t) RET`
or

    ;;; -*- Mode: LISP; slime-proxy-proxy-connection: t -*-




#### things that (mostly) work
* compile/eval with `C-c C-c`, `C-c C-k`, `M-C-x`, `C-x C-e` and REPL
* non-autodoc arglist display (including introspecting from browser if possible)
* `M-.`
* connection to multiple browsers at once (currently just sends
  everything to all connected browsers, so can behave a bit oddly.
  seems like it might be useful enough to try to figure out how to
  make it work sanely though)

#### things that don't work
* autodoc
* debugger
* pretty much everything else :p
