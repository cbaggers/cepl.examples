(in-package :cepl.examples+camera)

;;- - - - - - - - - - - - - - - - - -

(defparameter box-data nil)
(defparameter box-index nil)
(defparameter box-stream nil)
(defparameter brick nil)

(defstruct box
  (pos (v! 0 0 -10))
  (rot (q:identity)))

(defparameter box-a (make-box :pos (v! 0 0 -5)))
(defparameter box-b (make-box :pos (v! 0.3 0 -3)))

;;- - - - - - - - - - - - - - - - - -

(defun model->world (x)
  (m4:* (m4:translation (box-pos x)) (q:to-mat4 (box-rot x))))

(defun world->clip (c)
  (m4:* (cam->clip c) (world->cam c)))

(defun model->clip (m c)
  (m4:* (world->clip c) (model->world m)))

;;- - - - - - - - - - - - - - - - - -

(defparameter bp (make-blending-params))
(defparameter camera (make-camera))
(defparameter factor 0)

(defun-g box-vert ((vert g-pnt) &uniform (model->clip :mat4))
  (values (* model->clip (v! (pos vert) 1))
          (norm vert)
          (tex vert)))

(defun-g box-frag ((norm :vec3) (tc :vec2) &uniform (tex :sampler-2d) (fac :float))
  (v! (s~ (texture tex (* tc 1)) :xyz) fac))

(def-g-> draw-box ()
  (box-vert g-pnt)
  (box-frag :vec3 :vec2))

;;- - - - - - - - - - - - - - - - - -

(defun step-demo ()
  (incf factor 0.12)
  (setf (box-rot box-a) (q:from-axis-angle
                         (v! (sin factor) (cos factor) 1) 10s0)
        (box-rot box-b) (q:from-axis-angle
                         (v! (sin (/ factor 5)) (cos (/ factor -3)) 1) 10s0))
  (clear)
  (map-g #'draw-box box-stream
         :model->clip (model->clip box-a camera)
         :tex brick)
  (with-blending bp
    (map-g #'draw-box box-stream
           :model->clip (model->clip box-b camera)
           :tex brick
           :fac (+ 0.7 (* (sin factor) 0.3))))
  (swap))

;;- - - - - - - - - - - - - - - - - -

(defun init ()
  (destructuring-bind (d i) (dendrite.primitives:box-data)
    (setf box-data (make-gpu-array d :element-type 'g-pnt)
          box-index (make-gpu-array i :element-type :ushort)
          box-stream (make-buffer-stream box-data :index-array box-index)
          brick (sample
                 (dirt:load-image-to-texture
                  (merge-pathnames "brick/col.png" *examples-dir*))))))

(let ((running t))
  (defun run-loop ()
    (unless brick (init))
    (loop :while (and running (not (shutting-down-p))) :do
       (continuable
         (step-host)
         (update-repl-link)
         (step-demo))))
  (defun stop-loop () (setf running nil)))
