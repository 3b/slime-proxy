### Slime proxy

A hack for using slime with things like parenscript and 3bil that
compile code on a host lisp then run it somewhere else.

Currently only usable with parenscript, using websockets for
communication between host lisp and browser.

#### Requirements:
* currently only works on sbcl, since clws hasn't been ported to other lisps yet,
  though such a port would only take an hour or so
* Modified parenscript.  Either [3b's fairly up-to-date branch](http://github.com/3b/parenscript) or
  [gonzojive's older one](http://github.com/gonzojive/parenscript)
* [clws](http://github.com/3b/clws)
* [websockets emulation](http://github.com/gimite/web-socket-js) for
  browsers without native support, included as a git submodule
* misc libraries: ironclad, anaphora, cl-ppcre, yason

#### Installation of slime-proxy (and slime-parenscript)

slime-proxy comes with slime-parenscript, which a means of interacting
with browser-side Javascript through Common Lisp and SLIME.  Here we
describe how to install both:

Install symbolic links in the slime/contrib directory.  Go into your
slime contrib directory and add symbolic links to the following files:

    slime-proxy.el
    swank-proxy.lisp
    slime-parenscript.el
    swank-parenscript.lisp

Next you need to make ASD files visibile to ASDF so that they may be
loaded from Common Lisp.  These ASD files are

    slime-parenscript.asd
    slime-proxy.asd

In your .emacs file, there should be a line like

    (slime-setup '(slime-repl slime-fancy slime-indentation slime-autodoc))

Add to that slime-proxy and slime-parenscript

    (slime-setup '(slime-proxy slime-parenscript))

Finally, open up slime and type `M-x slime-proxy`.  It will prompt you
for "Target for proxy," to which you should type "ps".  Now you have
got a REPL, and when you hit `C-c C-c` etc. inside of `.paren` files,
they will be compiled from Parenscript to Javascript and sent to any
connected browser.

#### Connect to slime through a web browser

Load the file `contrib/slimy/slimy.html` in a web browser on
localhost.  Point your web browser there

switch an emacs buffer to proxy mode:
`M-: (setq slime-proxy-proxy-connection t) RET`
or

    ;;; -*- Mode: LISP; slime-proxy-proxy-connection: t -*-

You may need to adjust URLs/ports in `contrib/slimy/slimy.html` and
`wsproxy.lisp` (see `*swank-proxy-port*` in the latter), but probably
not.  The default is 12344.

(If clients are loading page from somewhere other than localhost, add
the host to the `(ws:origin-prefix "http..." ...)` form in
wsproxy,lisp)


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

