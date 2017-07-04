(in-package :cepl.examples)
;; This gives us a simple moving triangle

(defparameter *vertex-stream* nil)
(defparameter *array* nil)
(defparameter *positions* nil)

(defun-g mtri-vert ((position :vec4) (offset :vec3))
  (values (+ position (v! (* offset 0.9) 0))
          (:flat gl-instance-id)))

(defun-g mtri-frag ((id :int))
  (v! (cos id) (sin id) 0.4 1.0))

(defpipeline-g prog-1 ()
  (mtri-vert :vec4 :vec3)
  (mtri-frag :int))

(defun step-demo ()
  (step-host)
  (update-repl-link)
  (clear)
  (with-instances 100
    (map-g #'prog-1 *vertex-stream*))
  (swap))

(let ((running nil))
  (defun run-loop ()
    (setf running t)
    (setf *array* (make-gpu-array (list (v!  0.0   0.2  0.0  1.0)
                                        (v! -0.2  -0.2  0.0  1.0)
                                        (v!  0.2  -0.2  0.0  1.0))
                                  :element-type :vec4
                                  :dimensions 3))
    (setf *positions* (make-gpu-array (loop :for i :below 100 :collect
                                         (v! (- (random 2f0) 1f0)
                                             (- (random 2f0) 1f0)
                                             0))
                                      :element-type :vec3
                                      :dimensions 100))
    (setf *vertex-stream* (make-buffer-stream
                           (list *array* (cons *positions* 1))))
    (loop :while (and running (not (shutting-down-p))) :do
       (continuable (step-demo))))
  (defun stop-loop () (setf running nil)))
