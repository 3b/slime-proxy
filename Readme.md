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
  (for best results you will usually want slime-proxy and parenscript
   from the same place)
* [clws](http://github.com/3b/clws)
* [websockets emulation](http://github.com/gimite/web-socket-js) for
  browsers without native support, included as a git submodule
  (Websockets is available natively in Firefox4, but is disabled due to
   security concerns. You can enable it in about:config, or use the
   flash emulation.)
* misc libraries: ironclad, anaphora, cl-ppcre, yason (see .asd files for exact list)

#### Installation of slime-proxy (and slime-parenscript)

slime-proxy comes with slime-parenscript, which a means of interacting
with browser-side Javascript through Common Lisp and SLIME.  Here we
describe how to install both:

Put the following into `~/.emacs` to tell emacs where to find slime-proxy
and `slime-proxy-parenscript`, adjusting `path/to/slime-proxy/` as needed:

    (add-to-list 'load-path "path/to/slime-proxy/")
    (add-to-list 'load-path "path/to/slime-proxy/contrib/slime-parenscript/")

Put the following into `~/.swank.lisp` to tell swank how to load the
CL parts, adjusting `path/to/slime-proxy/` as needed again:

    (push #P "path/to/slime-proxy/" swank::*load-path*)
    (push #P "path/to/slime-proxy/contrib/slime-parenscript/" swank::*load-path*)

Since slime-proxy is rather annoying to load by default, add the
following function to `~/.emacs`:

    (defun slime-proxy-setup ()
      (interactive)
      (slime-setup '(slime-proxy slime-parenscript)))

Then, after restarting emacs or evaluating the forms added to `.emacs`
and `.swank.lisp` by hand, `M-x slime-proxy-setup` will load
`slime-proxy` (but not start it, you can probably add a call to
`(slime-proxy)` to `slime-proxy-setup` if you want that).  Once it is
loaded, you can start it with `M-x slime-proxy`.  It will prompt you
for "Target for proxy," to which you should type "ps".  Now you have
got a REPL, and when you hit `C-c C-c` etc. inside of `.paren` files,
they will be compiled from Parenscript to Javascript and sent to any
connected browser.

Note that the lisp side of `slime-proxy-parenscript` can take a while
to load all its dependencies (particularly if they haven't been
compiled yet), during which time emacs will not respond, and just show
a 'busy' cursor. To avoid this, you can load `swank-parenscript`
through `asdf` before starting `slime-proxy`.


(If you want to load `slime-proxy` by default, you can add
`slime-proxy` and `slime-parenscript` to an existing `slime-setup`
call instead of using `slime-proxy-setup`)


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











