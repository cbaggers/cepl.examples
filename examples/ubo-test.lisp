(in-package :cepl.examples)

(defparameter *array* nil)
(defparameter *ubo* nil)
(defparameter *stream* nil)
(defparameter *running* nil)

(defstruct-g test ()
  (scale :float :accessor scale))

(defun-g vert ((vert g-pc) &uniform (hmm test :ubo))
  (values (v! (* (pos vert) (scale hmm)) 1.0)
          (:smooth (col vert))))

(defun-g frag ((color :vec4))
  color)

(defpipeline prog-1 ()
    (g-> #'vert #'frag))

(defun step-demo ()
  (step-host)
  (update-repl-link)
  (clear)
  (map-g #'prog-1 *stream* :hmm *ubo*)
  (swap))

(defun run-loop ()
  (setf *running* t
        *ubo* (make-ubo '(1.2) 'test)
        *array* (make-gpu-array (list (list (v!  0.5 -0.36 0) (v! 0 1 0 1))
                                      (list (v!    0   0.5 0) (v! 1 0 0 1))
                                      (list (v! -0.5 -0.36 0) (v! 0 0 1 1)))
                                :element-type 'g-pc)
        *stream* (make-buffer-stream *array*))
  (map-g #'prog-1 nil :hmm *ubo*)
  (loop :while *running* :do (continuable (step-demo))))

(defun stop-loop ()
  (setf *running* nil))
