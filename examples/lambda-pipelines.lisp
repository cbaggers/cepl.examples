(in-package :cepl.examples)

;; This is the 'moving-triangles' example expect using lambda pipelines rather
;; than named ones

(defparameter *vertex-stream* nil)
(defparameter *array* nil)
(defparameter *loop* 0.0)

(defun-g calc-pos ((v-pos :vec4) (id :float))
  (let ((pos (v! (* (s~ v-pos :xyz) 0.3) 1.0)))
    (+ pos (let ((i (/ (+ (float id)) 2)))
             (v! (sin (+ i *loop*))
                 (cos (* 3 (+ (tan i) *loop*)))
                 0.0 0.0)))))

(defun-g mtri-vert ((position :vec4) &uniform (i :float))
  (calc-pos position i))

(defun-g mtri-frag ()
  (v! (cos *loop*) (sin *loop*) 0.4 1.0))


(defun step-demo (pipeline)
  (declare (type function pipeline))
  (step-host)
  (update-repl-link)
  (setf *loop* (+ 0.004 *loop*))
  (clear)
  (loop :for i :below 100 :do
     (let ((i (/ i 2.0)))
       (map-g pipeline *vertex-stream* :i i)))
  (swap))

(let ((running nil))
  (defun run-loop ()
    (setf running t)
    (setf *array* (make-gpu-array (list (v!  0.0   0.2  0.0  1.0)
                                        (v! -0.2  -0.2  0.0  1.0)
                                        (v!  0.2  -0.2  0.0  1.0))
                                  :element-type :vec4
                                  :dimensions 3))
    (setf *vertex-stream* (make-buffer-stream *array*))
    (let ((pipeline (pipeline-g ()
                      '(mtri-vert :vec4)
                      '(mtri-frag))))
      (loop :while (and running (not (shutting-down-p))) :do
         (continuable (step-demo pipeline)))))
  (defun stop-loop () (setf running nil)))
