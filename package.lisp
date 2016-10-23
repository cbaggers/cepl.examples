;;;; package.lisp

;; helper packages

(defpackage #:cepl.examples.misc
  (:use #:cl #:cepl)
  (:export :*examples-dir*))

(defpackage #:cepl.examples.camera
  (:use #:cl #:cepl #:varjo-lang)
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

(defpackage :cepl.examples.model-parsers
  (:use :cl :cepl)
  (:export :load-file
           :meshes->lists
           :mesh->lists
           :mesh-list->gpu
           :mesh->gpu
           :scene-meshes->gpu
           :calc-type))

(defpackage :cepl.examples.meshes
  (:use :cl :cffi :split-sequence :cepl)
  (:export :mesh
           :vertices
           :indicies
           :primitive-type
           :transform-mesh
           :transform-mesh-with-matrix
           :polygonize
           :flatten-index))

;; packages used in the examples

(defpackage #:cepl.examples
  (:use #:cl #:cepl #:cepl.examples.misc
	#:rtg-math #:varjo-lang #:livesupport
	#:skitter.sdl2.keys #:skitter.sdl2.mouse-buttons
	#:temporal-functions))

(defpackage #:cepl.examples+camera
  (:use #:cl #:cepl #:cepl.examples.camera
	#:cepl.examples.misc
	#:rtg-math #:varjo-lang #:livesupport
	#:skitter.sdl2.keys #:skitter.sdl2.mouse-buttons
	#:temporal-functions))

(defpackage #:cepl.examples+physics
  (:use #:cl #:cepl #:cepl.examples.camera
	#:cepl.examples.misc
	#:rtg-math #:varjo-lang #:livesupport
	#:skitter.sdl2.keys #:skitter.sdl2.mouse-buttons
	#:temporal-functions
        #:cffi))
