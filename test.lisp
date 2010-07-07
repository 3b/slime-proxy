;;; -*- Mode: LISP; slime-proxy-proxy-connection: t -*-
(in-package :proxy-test)

(list 1 () 1)

((@ *function introspect))

(progn
  (defun lisp-implementation-version ()
    (@ navigator user-agent))
)
(defun foo () "bat ")
(lisp-implementation-version)
(Defun foo (a b) (+ 1 a 2 (* b 3)))
(foo 3 4)
(@ navigator app-code-name)

((@ canvas get-context) )
((@ document get-element-by-id))
((@ swfobject register-object) )
((@ ((@ document get-element-by-id ) "canvas") get-context)   )

(alert "foo1")
(var canvas ((@ document get-element-by-id ) "canvas"))
(var ctx ((@ canvas get-context) "2d"))
(setf (@ ctx fill-style) "rgb(200,0,0)")
((@ ctx fill-rect) 10 210 55 50 )
(setf (@ ctx fill-style) "rgba(0,0,200,0.5)")
((@ ctx fill-rect) 30 190 55 50 )

(setf (@ ctx fill-style) "rgba(0,122,200,0.5)")
((@ ctx begin-path))
((@ ctx move-to) 30 150)
((@ ctx line-to) 150 150)
((@ ctx bezier-curve-to) 60 70 60 70 70 150 )
((@ ctx line-to) 30 30)
((@ ctx fill))

(defmacro path ((context) &rest path)
  `(progn
     ((@ ,context begin-path))
     ,@(loop with commands = '(:m (move-to 2) :l (line-to 2)
                                   :close (close-path 0) :fill (fill 0)
                                   :b (bezier-curve-to 6)
                                   :q (quadratic-curve-to 4))
              for c = (pop path)
              for (cmd argc) = (getf commands c)
              while c
              collect `((@ ,context ,cmd) ,@(loop repeat argc
                                                  collect (pop path)))
                ))
)


(defun bow-tie (ctx fill)
  (setf (@ ctx fill-style) "rgba(200,200,200,0.3)")
  ((@ ctx fill-rect) -30 -30 60 60 )
  (setf (@ ctx fill-style) fill)
  (path (ctx)
        :m 25 25
        :l -25 -25
        :l 25 -25
        :l -25 25
        :close :fill))

(defun dot (ctx)
  ((@ ctx save))
  (setf (@ ctx fill-style) "black")
  ((@ ctx fill-rect) -2 -2 4 4)
  ((@ ctx restore)))


(defun draw ()
  (let* ((canvas ((@ document get-element-by-id ) "canvas"))
         (ctx ((@ canvas get-context) "2d")))
    ((@ ctx save))
    ((@ ctx translate) 45 45)

    ((@ ctx save))
    (bow-tie ctx "red")
    (dot ctx)
    ((@ ctx restore))

    ((@ ctx save))
    ((@ ctx translate) 85 0)
    ((@ ctx rotate) (* 45 (/ pi 180)) )
    (bow-tie ctx "green")
    (dot ctx)
    ((@ ctx restore))

    ((@ ctx save))
    ((@ ctx translate) 0 85)
    ((@ ctx rotate) (* 135 (/ pi 180)) )
    (bow-tie ctx "blue")
    (dot ctx)
    ((@ ctx restore))

    ((@ ctx save))
    ((@ ctx translate) 85 85)
    ((@ ctx rotate) (* 90 (/ pi 180)) )
    (bow-tie ctx "yellow")
    (dot ctx)
    ((@ ctx restore))
    ((@ ctx restore))

 )
  )

((@ ctx translate) +15 +15)

(progn
  ((ps:@ ctx save))
  (setf (@ ctx global-alpha) 0.03)
  ;;((@ ctx translate) +45 +45)
  ;;((@ ctx rotate) (* 135 (/ pi 180)))
  ;;((@ ctx translate) -45 -45)
  ((ps:@ ctx translate) (- (random 50.0) 25) (- (random 50.0) 25))

  (draw)
  ((@ ctx restore)))

(progn
  ((@ ctx save))
  ((@ ctx translate))
  ())
