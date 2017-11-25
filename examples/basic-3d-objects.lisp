;; More 3D - Multiple objects rotating
(in-package :cepl.examples+camera)

(defparameter *entities* nil)
(defparameter *camera* nil)

(defun-g b3d-vert ((vert g-pc) &uniform (model->world :mat4) (world->cam :mat4)
                   (cam->clip :mat4))
  (values (* cam->clip
             (* world->cam
                (* model->world
                   (v! (pos vert) 1.0))))
          (:smooth (col vert))))

(defun-g b3d-frag ((interp-color :vec4))
  interp-color)

(defpipeline-g render-widgets ()
  (b3d-vert g-pc)
  (b3d-frag :vec4))

(defclass entity ()
  ((e-stream :initform nil :initarg :e-stream :accessor e-stream)
   (position :initform (v! 0 0 -20) :initarg :pos :accessor pos)
   (rotation :initform (v! 0 0 0) :initarg :rot :accessor rot)
   (scale :initform (v! 1 1 1) :initarg :scale :accessor scale)))

(defun make-entity (&key pos e-stream)
  (make-instance 'entity :pos pos :e-stream e-stream))


(defun init ()
  (setf *camera* (make-camera))
  (setf (pos *camera*) (v! 0 8 0))
  (map-g #'render-widgets nil
         :cam->clip (cam->clip *camera*)
         :world->cam (world->cam *camera*))
  (let* ((verts (make-gpu-array `((,(v! +1  +1  +1)  ,(v! 0  1  0  1))
                                  (,(v! -1  -1  +1)  ,(v! 0  0  1  1))
                                  (,(v! -1  +1  -1)  ,(v! 1  0  0  1))
                                  (,(v! +1  -1  -1)  ,(v! 0.5  0.5  0  1))
                                  (,(v! -1  -1  -1)  ,(v! 0  1  0  1))
                                  (,(v! +1  +1  -1)  ,(v! 0  0  1  1))
                                  (,(v! +1  -1  +1)  ,(v! 1  0  0  1))
                                  (,(v! -1  +1  +1)  ,(v! 0.5  0.5  0  1)))
                                :element-type 'g-pc :dimensions 8))
         (indicies (make-gpu-array '(0 2 1   1 3 0   2 0 3   3 1 2
                                     5 6 4   4 7 5   7 4 6   6 5 7)
                                   :dimensions 24 :element-type :unsigned-short))
         (e-stream (make-buffer-stream verts :index-array indicies)))
    (setf *entities*
          (mapcar (lambda (_) (make-entity :pos _ :e-stream e-stream))
                  (list (v!  0 0 -20) (v!  0 0 -25) (v!  5 0 -20)
                        (v!  0 0 -15) (v! -5 0 -20))))))

(defun update-entity (entity)
  (let ((m2w (reduce #'m4:* (list (m4:translation (pos entity))
                                  (m4:rotation-from-euler (rot entity))
                                  (m4:scale (scale entity))))))
    (setf (rot entity) (v:+ (rot entity) (v! 0.01 0.02 0)))
    (map-g #'render-widgets (e-stream entity) :model->world m2w)))

(defun step-demo ()
  (step-host)
  (update-repl-link)
  (with-viewport (cam-viewport *camera*)
    (clear)
    (map-g #'render-widgets nil :cam->clip (cam->clip *camera*) :world->cam (world->cam *camera*))
    (map nil #'update-entity *entities*)
    (swap)))

(defun reshape (dimensions)
  (setf (frame-size *camera*) dimensions)
  (map-g #'render-widgets nil :cam->clip (cam->clip *camera*) :world->cam (world->cam *camera*)))

(defun window-size-callback (size &rest ignored)
  (declare (ignore ignored))
  (reshape size))

(let ((running nil))
  (defun run-loop ()
    (init)
    (reshape (current-viewport))
    (setf running t)
    (whilst-listening-to ((#'window-size-callback (window 0) :size))
      (loop :while (and running (not (shutting-down-p))) :do
         (continuable (step-demo)))))

  (defun stop-loop () (setf running nil)))
