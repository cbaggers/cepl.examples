(in-package :cepl.examples+physics)

(defvar points
  (list (list (v! -100s0 0s0  100s0)
              (v! 100s0 0s0  100s0)
              (v! 100s0 0s0 -100s0)
              (v! -100s0 0s0 -100s0))))

(def-callback-apply-force-and-torque apply-gravity (body)
  (multiple-value-bind (mass) (body-mass body)
    (setf (body-force body) (v! 0s0 (* -9.8 mass) 0s0 0s0))))

(defun create-floor (world)
  (with-geometry (geom (make-geometry-tree world points))
    (make-body world geom)))

(defun create-ball (world pos)
  (with-geometry (geom (make-sphere-geometry world :radius 1s0))
    (let ((body (make-body world geom :linear-damping 0s0
                           :mass 1s0)))
      (body-set-force-torque-callback body 'apply-gravity)
      (setf (body-matrix4 body) (m4:translation pos))
      body)))

;;- - - - - - - - - - - - - - - - - -

(defvar *world* (make-world))
(defvar *floor* (create-floor *world*))
(defvar *ball-0* (create-ball *world* (v! 0.2 2 -10)))
(defvar *ball-1* (create-ball *world* (v! 0.2 2 -10)))

;;------------------------------------------------------------

(defparameter ball-data nil)
(defparameter ball-index nil)
(defparameter ball-stream nil)
(defparameter viewport (make-viewport '(1024 768)))
(defparameter brick nil)

;;- - - - - - - - - - - - - - - - - -

(defun model->world (x)
  (body-matrix4 x))

(defun world->clip (c)
  (m4:* (cam->clip c) (world->cam c)))

(defun model->clip (m c)
  (m4:* (world->clip c) (model->world m)))

;;- - - - - - - - - - - - - - - - - -

(defparameter camera (make-camera))

(defun-g ball-vert ((vert g-pnt) &uniform (model->clip :mat4))
  (values (* model->clip (v! (pos vert) 1))
          (norm vert)
          (tex vert)))

(defun-g ball-frag ((norm :vec3) (tc :vec2) &uniform (tex :sampler-2d) (fac :float))
  (v! (s~ (texture tex (* tc 1)) :xyz) fac))

(def-g-> draw-ball ()
  #'ball-vert #'ball-frag)

;;- - - - - - - - - - - - - - - - - -

(defun step-demo ()
  (with-viewport viewport
    (clear)
    (world-step *world* step)
    (map-g #'draw-ball ball-stream
           :model->clip (model->clip *ball-0* camera)
           :tex brick)
    (map-g #'draw-ball ball-stream
           :model->clip (model->clip *ball-1* camera)
           :tex brick)
    (swap)))

;;- - - - - - - - - - - - - - - - - -

(defun init ()
  (destructuring-bind (d i) (dendrite.primitives:sphere-data :radius 2)
    (setf ball-data (make-gpu-array d :element-type 'g-pnt)
          ball-index (make-gpu-array i :element-type :ushort)
          ball-stream (make-buffer-stream ball-data :index-array ball-index)
          brick (sample
		 (cepl.sdl2-image:load-image-to-texture
		  (merge-pathnames "brick/col.png" *examples-dir*)))))
  (setf (pos camera) (v! 0 10 10))
  t)

(let ((running t))
  (defun run-loop ()
    (unless brick (init))
    (loop :while (and running (not (shutting-down-p))) :do
       (continuable
         (step-host)
         (update-repl-link)
         (step-demo))))
  (defun stop-loop () (setf running nil)))

;;------------------------------------------------------------
