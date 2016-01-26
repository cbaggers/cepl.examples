;;;; package.lisp

(defpackage #:cepl.examples.camera
  (:use #:cl #:cepl)
  (:export :camera
           :make-camera
           :orthographic-projection
           :perspective-projection
           :world->cam
           :look-at
           :world-up
           :pos
           :dir
           :frame-size
           :fov
           :far
           :near
           :cam->clip-func
           :cam->clip
           :world->cam
           :make-cam-clip-matrix))

(defpackage #:cepl.examples
  (:use #:cl #:cepl))

(defpackage #:cepl.examples+camera
  (:use #:cl #:cepl #:cepl.examples.camera))
