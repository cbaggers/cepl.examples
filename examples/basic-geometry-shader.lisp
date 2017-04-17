(in-package :cepl.examples+camera)

;;- - - - - - - - - - - - - - - - - -

(defparameter sphere-stream nil)
(defparameter brick nil)

(defstruct sphere
  (pos (v! 0 0 -10))
  (rot (q:identity)))

(defparameter sphere-a (make-sphere :pos (v! 0 0 -1)))

;;- - - - - - - - - - - - - - - - - -

(defun model->world (x)
  (m4:* (m4:translation (sphere-pos x)) (q:to-mat4 (sphere-rot x))))

(defun world->clip (c)
  (m4:* (cam->clip c) (world->cam c)))

(defun model->clip (m c)
  (m4:* (world->clip c) (model->world m)))

;;- - - - - - - - - - - - - - - - - -

(defparameter bp (make-blending-params))
(defparameter camera (make-camera))
(defparameter factor 0)

;;- - - - - - - - - - - - - - - - - -

(defun-g sphere-vert ((vert g-pnt) &uniform (model->clip :mat4))
  (values (* model->clip (v! (pos vert) 1))
          (s~ (* model->clip (v! (norm vert) 0)) :xyz)
          (tex vert)))

(defun-g sphere-frag ((norm :vec3) (tc :vec2)
                      &uniform (tex :sampler-2d) (fac :float))
  (v! (s~ (texture tex (* tc 1)) :xyz) fac))

(def-g-> draw-sphere ()
  :vertex (sphere-vert g-pnt)
  :fragment (sphere-frag :vec3 :vec2))

(defun-g normals-vert ((vert g-pnt) &uniform (model->clip :mat4))
  (values (* model->clip (v! (pos vert) 1))
          (s~ (* model->clip (v! (norm vert) 0)) :xyz)))

(defun-g normals-geom ((normals (:vec3 3)))
  (declare (varjo:output-primitive :kind :line-strip :max-vertices 6))
  (labels ((gen-line ((index :int))
             (let ((magnitude 0.2))
               (setf gl-position (gl-position (aref gl-in index)))
               (emit-vertex)
               (setf gl-position
                     (+ (gl-position (aref gl-in index))
                        (* (v! (aref normals index) 0f0)
                           magnitude)))
               (emit-vertex)
               (end-primitive)
               (values))))
    (gen-line 0)
    (gen-line 1)
    (gen-line 2)
    (values)))

(defun-g normals-frag ()
  (v! 1 1 0 1))

(def-g-> draw-normals ()
  :vertex (normals-vert g-pnt)
  :geometry (normals-geom (:vec3 3))
  :fragment (normals-frag))

;;- - - - - - - - - - - - - - - - - -

(defun step-demo ()
  (incf factor 0.01)
  (setf (sphere-rot sphere-a)
        (q:from-axis-angle (v! (sin factor) (cos factor) 1) 10s0))
  (clear)
  (map-g #'draw-sphere sphere-stream
         :model->clip (model->clip sphere-a camera)
         :tex brick)
  (map-g #'draw-normals sphere-stream
         :model->clip (model->clip sphere-a camera))
  (swap))

;;- - - - - - - - - - - - - - - - - -

(defun init ()
  (destructuring-bind (d i) (dendrite.primitives:sphere-data)
    (setf sphere-stream (make-buffer-stream
                         (make-gpu-array d :element-type 'g-pnt)
                         :index-array (make-gpu-array i :element-type :ushort))
          brick (sample
                 (cepl.sdl2-image:load-image-to-texture
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
