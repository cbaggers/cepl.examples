;;;; package.lisp

;; helper packages

(defpackage #:cepl.examples.misc
  (:use #:cl #:cepl)
  (:export :*examples-dir*))

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

;; packages used in the examples

(defpackage #:cepl.examples
  (:use #:cl #:cepl #:cepl.examples.misc
	#:rtg-math #:varjo-lang))

(defpackage #:cepl.examples+camera
  (:use #:cl #:cepl #:cepl.examples.camera
	#:cepl.examples.misc
	#:rtg-math #:varjo-lang))
