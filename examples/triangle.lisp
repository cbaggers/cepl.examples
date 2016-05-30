(in-package :cepl.examples)

(defparameter *array* nil)
(defparameter *stream* nil)
(defparameter *running* nil)

(defstruct-g pos-col
  (position :vec3 :accessor pos)
  (color :vec4 :accessor col))

(defun-g tri-vert ((vert pos-col))
  (values (v! (pos vert) 1.0)
          (col vert)))

(defun-g tri-frag ((color :vec4))
  color)

(def-g-> prog-1 ()
  tri-vert tri-frag)

(defun step-demo ()
  (step-host)
  (update-repl-link)
  (clear)
  (map-g #'prog-1 *stream*)
  (swap))

(defun run-loop ()
  (setf *running* t
        *array* (make-gpu-array (list (list (v!  0.5 -0.36 0) (v! 0 1 0 1))
                                      (list (v!    0   0.5 0) (v! 1 0 0 1))
                                      (list (v! -0.5 -0.36 0) (v! 0 0 1 1)))
                                :element-type 'pos-col)
        *stream* (make-buffer-stream *array*))
  (loop :while (and *running* (not (shutting-down-p))) :do
     (continuable (step-demo))))

(defun stop-loop ()
  (setf *running* nil))
