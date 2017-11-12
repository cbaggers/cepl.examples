(in-package :cepl.examples)

(defvar *running* nil)
(defparameter *vertex-stream* nil)
(defvar *feedback-vec3* nil)
(defvar *feedback-vec4* nil)
(defvar *tfs* nil)

(defun reset ()
  (unless *vertex-stream*
    (setf *vertex-stream*
          (make-buffer-stream
           (make-gpu-array (list (v!  0.0   0.5  0.0  1.0)
                                 (v! -0.5  -0.5  0.0  1.0)
                                 (v!  0.5  -0.5  0.0  1.0))
                           :element-type :vec4
                           :dimensions 3)))
    (setf *feedback-vec3*
          (make-gpu-array nil :element-type :vec3 :dimensions 10))
    (setf *feedback-vec4*
          (make-gpu-array nil :element-type :vec4 :dimensions 10))
    (setf *tfs*
          (make-transform-feedback-stream *feedback-vec3*
                                          *feedback-vec4*)))
  nil)

(defun-g mtri-vert ((position :vec4) &uniform (pos :vec2))
  (values ((:feedback 1) (+ position (v! pos 0 0)))
          ((:feedback 0) (v! 0.1 0 1))))

(defun-g mtri-frag ((col :vec3))
  (v! col 0))

(defpipeline-g prog-1 ()
  :vertex (mtri-vert :vec4)
  :fragment (mtri-frag :vec3))

(defun step-demo ()
  (step-host)
  (update-repl-link)
  (clear)
  (with-transform-feedback (*tfs*)
    (map-g #'prog-1 *vertex-stream* :pos (v! -0.1 0)))
  (map-g #'prog-1 *vertex-stream* :pos (v! 0.3 0.28))
  (swap))


(defun run-loop ()
  (unless *running*
    (print "--- started ---")
    (unwind-protect
         (progn
           (setf *running* t)
           (reset)
           (loop :while (and *running* (not (shutting-down-p))) :do
              (continuable (step-demo))))
      (setf *running* nil)
      (print "--- stopped ---"))))

(defun stop-loop () (setf *running* nil))
