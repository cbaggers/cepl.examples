(in-package :cepl.examples)

(defparameter *array* nil)
(defparameter *stream* nil)
(defparameter *running* nil)

(defstruct-g pos-col
  (position :vec3 :accessor pos)
  (color :vec4 :accessor col))

(defun-g vert ((vert pos-col))
  (values (v! (pos vert) 1.0)
          (col vert)))

;;----------------------------------------------------------------------

;; In this example we simply show that you can use shader stages that are
;; defined using raw glsl code. You have to specify the the args, uniforms
;; and outputs in a way cepl can understand, but with this info cepl writes
;; the plumbing for you.
;; You can change the glsl code and recompile just like any normal gpu-function

;; Instead of this
;;
(defun-g frag ((color :vec4))
  color)

;; We write this
;;
(def-glsl-stage frag-glsl (("color_in" :vec4) &context :330 :fragment)
  "void main() {
       color_out = color_in;
   }"
  (("color_out" :vec4)))

;;----------------------------------------------------------------------

(def-g-> prog-1 ()
  #'vert #'frag-glsl)

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
