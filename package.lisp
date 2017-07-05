;;;; package.lisp

;; helper packages

(uiop:define-package #:cepl.examples.misc
  (:use #:cl #:cepl)
  (:export :*examples-dir*))

(uiop:define-package #:cepl.examples.camera
  (:use #:cl #:cepl #:vari)
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
           :cam-viewport
           :world->cam
           :make-cam-clip-matrix))

(uiop:define-package :cepl.examples.model-parsers
  (:use :cl :cepl)
  (:export :load-file
           :meshes->lists
           :mesh->lists
           :mesh-list->gpu
           :mesh->gpu
           :scene-meshes->gpu
           :calc-type))

(uiop:define-package :cepl.examples.meshes
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

(uiop:define-package #:cepl.examples
  (:use #:cl #:cepl #:cepl.examples.misc
        #:rtg-math #:vari #:livesupport
        #:cepl.skitter
        #:temporal-functions))

(uiop:define-package #:cepl.examples+camera
  (:use #:cl #:cepl #:cepl.examples.camera
        #:cepl.examples.misc
        #:rtg-math #:vari #:livesupport
        #:cepl.skitter
        #:temporal-functions))

(uiop:define-package #:cepl.examples+physics
  (:use #:cl #:cepl #:cepl.examples.camera
        #:cepl.examples.misc
        #:rtg-math #:vari #:livesupport
        #:cepl.skitter
        #:temporal-functions
        #:cffi))
