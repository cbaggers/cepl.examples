(in-package :cepl.examples+camera)

;;- - - - - - - - - - - - - - - - - -

(defparameter sphere-stream nil)
(defparameter brick nil)

(defstruct sphere
  (pos (v! 0 0 -10))
  (rot (q:identity)))

(defparameter sphere-a (make-sphere :pos (v! 0 0 -1.5)))

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
          (pos vert)))

(defun-g sphere-tess-con ((position (:vec3 3)))
  (declare (varjo::output-patch :vertices 3))
  (let ((tess-level-inner 5f0)
        (tess-level-outer 5f0))
    (when (= gl-invocation-id 0)
      (setf (aref gl-tess-level-inner 0) tess-level-inner
            (aref gl-tess-level-outer 0) tess-level-outer
            (aref gl-tess-level-outer 1) tess-level-outer
            (aref gl-tess-level-outer 2) tess-level-outer)))
  (aref position gl-invocation-id))

(defun-g sphere-tess-eval ((position (:vec3 3)) &uniform (model->clip :mat4))
  (declare (tessellate-to
            :primitive :triangles
            :spacing :equal
            :order :ccw))
  (let* ((p0 (* (x gl-tess-coord) (aref position 0)))
         (p1 (* (y gl-tess-coord) (aref position 1)))
         (p2 (* (z gl-tess-coord) (aref position 2)))
         (pos (normalize (+ p0 p1 p2))))
    (values
     (* model->clip (v! pos 1))
     pos
     gl-tess-coord)))

(defun-g sphere-geom ((position (:vec3 3)) (patch-distance (:vec3 3))
                      &uniform (normal-mat :mat3))
  (declare (output-primitive :kind :triangle-strip
                             :max-vertices 3))
  (let* ((a (- (aref position 2) (aref position 0)))
         (b (- (aref position 2) (aref position 0)))
         (facet-normal (* normal-mat (normalize (cross a b)))))
    (emit ()
          (gl-position (aref gl-in 0))
          (aref patch-distance 0)
          facet-normal
          (v! 1 0 0))
    (emit ()
          (gl-position (aref gl-in 1))
          (aref patch-distance 1)
          facet-normal
          (v! 0 1 0))
    (emit ()
          (gl-position (aref gl-in 2))
          (aref patch-distance 2)
          facet-normal
          (v! 0 0 1))
    (varjo-lang::end-primitive)
    (values)))

(defun-g sphere-frag ((patch-distance :vec3) (facet-normal :vec3)
                      (tri-distance :vec3))
  (labels ((amplify ((d :float) (scale :float) (offset :float))
             (let* ((d (+ (* scale d) offset))
                    (d (clamp d 0 1)))
               (- 1 (expt (* d d -2) 2)))))
    (let* ((light-position (v! 0 1 0))
           (diffuse-material (v! 1 0 0))
           (ambient-material (v! 0.2 0.2 0.2))
           ;;
           (n (normalize facet-normal))
           (l light-position)
           (df (abs (dot n l)))
           (color (+ ambient-material (* df diffuse-material)))
           (d1 (min (min (x tri-distance) (y tri-distance))
                    (z tri-distance)))
           (d2 (min (min (x patch-distance) (y patch-distance))
                    (z patch-distance)))
           (color (* (+ ambient-material (* df diffuse-material))
                     (amplify d1 40 -0.5)
                     (amplify d2 50 -0.5))))
      (v! color 1))))

(def-g-> draw-sphere ((:patch 3))
  :vertex (sphere-vert g-pnt)
  :tessellation-control (sphere-tess-con (:vec3 3))
  :tessellation-evaluation (sphere-tess-eval (:vec3 3))
  :geometry (sphere-geom (:vec3 3) (:vec3 3))
  :fragment (sphere-frag :vec3 :vec3 :vec3))

;;- - - - - - - - - - - - - - - - - -

(defun step-demo ()
  (incf factor 0.001)
  (setf (sphere-rot sphere-a)
        (q:from-axis-angle (v! (sin factor) (cos factor) 1) 10s0))
  (clear)
  (map-g #'draw-sphere sphere-stream
         :model->clip (model->clip sphere-a camera))
  (swap))

;;- - - - - - - - - - - - - - - - - -

(defun init ()
  (destructuring-bind (d i) (dendrite.primitives:sphere-data
                             :lines-of-longitude 10
                             :lines-of-latitude 10)
    (setf sphere-stream (make-buffer-stream
                         (make-gpu-array d :element-type 'g-pnt)
                         :index-array (make-gpu-array i :element-type :ushort))
          brick (sample
                 (cepl.sdl2-image:load-image-to-texture
                  (merge-pathnames "brick/col.png" *examples-dir*))))))

(let ((running nil))
  (defun run-loop ()
    (unless brick (init))
    (setf running t)
    (loop :while (and running (not (shutting-down-p))) :do
       (continuable
         (step-host)
         (update-repl-link)
         (step-demo))))
  (defun stop-loop () (setf running nil)))
