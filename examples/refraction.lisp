;; Simple refraction example
(in-package :cepl.examples+camera)

;; NOTE: Ensure you have loaded cepl-image-helper & cepl-model-helper
;;       (or just load cepl-default)

;;--------------------------------------------------------------
;; setup

(defparameter *bird* nil)
(defparameter *wibble* nil)
(defparameter *camera* nil)
(defparameter *light* nil)
(defparameter *bird-tex* nil)
(defparameter *bird-tex2* nil)
(defparameter *wib-tex* nil)
(defparameter *bird-sampler* nil)
(defparameter *bird-sampler2* nil)
(defparameter *wib-sampler* nil)
(defparameter *loop-pos* 0.0)
(defparameter *scene* nil)
(defparameter *scene-sampler* nil)

(defclass entity ()
  ((gstream :initform nil :initarg :gstream :accessor gstream)
   (position :initform (v! 0 0 -1) :initarg :pos :accessor pos)
   (rotation :initform (v! 0 0 0) :initarg :rot :accessor rot)
   (scale :initform (v! 1 1 1) :initarg :scale :accessor scale)
   (mesh :initarg :mesh :reader mesh)))

(defclass light ()
  ((position :initform (v! 20 20 -20) :initarg :pos :accessor pos)
   (radius :initform 1.0 :initarg :radius :accessor radius)))

(defun load-model (file-path nth-mesh &optional hard-rotate)
  (let* ((imp-mesh (elt (classimp:meshes (classimp:import-into-lisp file-path))
                        nth-mesh))
         (result (cepl.examples.model-parsers:mesh->gpu imp-mesh))
         (mesh (make-instance 'cepl.examples.meshes:mesh
                              :primitive-type :triangles
                              :vertices (first result)
                              :index (second result)))
         (mesh~1 (if hard-rotate
                     (cepl.examples.meshes:transform-mesh
                      mesh :rotation hard-rotate)
                     mesh)))
    (let ((gstream (make-buffer-stream
                    (cepl.examples.meshes:vertices mesh)
                    :index-array (cepl.examples.meshes:indicies mesh))))
      (make-instance 'entity :rot (v! 1.57079633 1 0) :gstream gstream
                     :pos (v! 0 -0.4 -1) :mesh mesh~1))))

(defun init ()
  (setf *scene* (make-fbo 0 :d))
  (setf *scene-sampler* (sample (attachment-tex *scene* 0)))
  (setf *light* (make-instance 'light))
  (setf *camera* (make-camera))
  (setf *wibble* (load-model (merge-pathnames "wibble.3ds" *examples-dir*)
                             0 (v! pi 0 0)))
  (setf (v:z (pos *wibble*)) -3.0)
  (setf *bird* (load-model (merge-pathnames "bird/bird.3ds" *examples-dir*) 1 (v! pi 0 0)))
  (setf *bird-tex* (dirt:load-image-to-texture
                    (merge-pathnames "water.jpg" *examples-dir*)))
  (setf *bird-tex2* (dirt:load-image-to-texture
                     (merge-pathnames "bird/char_bird_col.png" *examples-dir*)))
  (setf *wib-tex* (dirt:load-image-to-texture
                   (merge-pathnames "brick/col.png" *examples-dir*)))
  (setf *bird-sampler* (sample *bird-tex*))
  (setf *bird-sampler2* (sample *bird-tex2*))
  (setf *wib-sampler* (sample *wib-tex*)))

;;--------------------------------------------------------------
;; drawing

(defun-g standard-vert ((data g-pnt) &uniform (model-to-cam :mat4)
                        (cam-to-clip :mat4))
  (values (* cam-to-clip (* model-to-cam (v! (pos data) 1.0)))
          (pos data)
          (norm data)
          (v! 0.4 0 0.4 0)
          (tex data)))

(defun-g standard-frag
    ((model-space-pos :vec3) (vertex-normal :vec3) (diffuse-color :vec4)
     (tex-coord :vec2)
     &uniform (model-space-light-pos :vec3) (light-intensity :vec4)
     (ambient-intensity :vec4) (textur :sampler-2d))
  (let* ((light-dir (normalize (- model-space-light-pos
                                  model-space-pos)))
         (cos-ang-incidence
          (clamp (dot (normalize vertex-normal) light-dir) 0.0 1.0))
         (t-col (texture textur (v! (x tex-coord) (- (y tex-coord))))))
    (+ (* t-col light-intensity cos-ang-incidence)
       (* t-col ambient-intensity))))

(defun-g refract-vert ((data g-pnt) &uniform (model-to-cam :mat4)
                       (cam-to-clip :mat4))
  (values (* cam-to-clip (* model-to-cam (v! (pos data) 1.0)))
          (tex data)))

(defun-g refract-frag ((tex-coord :vec2) &uniform (textur :sampler-2d)
                       (bird-tex :sampler-2d) (fbo-tex :sampler-2d)
                       (loop :float))
  (let* ((o (v! (mod (* loop 0.05) 1.0)
                (mod (* loop 0.05) 1.0)))
         (ot (* (s~ (texture textur (+ o tex-coord)) :xy) 0.1))
         (a (texture textur tex-coord))
         (b (+ (v! (* (x gl-frag-coord) (/ 1.0 640.0))
                   (* (y gl-frag-coord) (/ 1.0 480.0)))
               (* (s~ a :xy) 0.020)
               ot))
         (c (texture fbo-tex b))
         (r (* (texture bird-tex (* (v! 1 -1) tex-coord)) 0.1)))
    (+ r c)))

(defpipeline-g standard-pass ()
  (standard-vert g-pnt)
  (standard-frag :vec3 :vec3 :vec4 :vec2)
  :post #'reshape)

(defpipeline-g refract-pass ()
  (refract-vert g-pnt)
  (refract-frag :vec2)
  :post #'reshape)

(defun two-pass (scene wib-stream bird-stream model-to-cam model-to-cam2
                 model-space-light-pos)
  (clear scene)
  (map-g-into scene #'standard-pass wib-stream
              :textur *wib-sampler*
              :ambient-intensity (v! 0.2 0.2 0.2 1.0)
              :light-intensity (v! 1 1 1 0)
              :model-space-light-pos model-space-light-pos
              :model-to-cam model-to-cam)
  (map-g #'refract-pass bird-stream
         :loop *loop-pos*
         :fbo-tex *scene-sampler*
         :bird-tex *bird-sampler2*
         :textur *bird-sampler*
         :model-to-cam model-to-cam2))

(defun draw ()
  (clear)
  (let* ((world-to-cam-matrix (world->cam *camera*))
         (cam-light-vec (m4:*v (entity-matrix *wibble*)
                               (v! (pos *light*) 1.0))))
    (map-g #'standard-pass (gstream *wibble*)
           :textur *wib-sampler*
           :ambient-intensity (v! 0.2 0.2 0.2 1.0)
           :light-intensity (v! 1 1 1 0)
           :model-space-light-pos (v:s~ cam-light-vec :xyz)
           :model-to-cam (m4:* world-to-cam-matrix (entity-matrix *wibble*)))
    (two-pass *scene* (gstream *wibble*) (gstream *bird*)
              (m4:* world-to-cam-matrix (entity-matrix *wibble*))
              (m4:* world-to-cam-matrix (entity-matrix *bird*))
              (v:s~ cam-light-vec :xyz)))
  (swap))



(defun entity-matrix (entity)
  (reduce #'m4:* (list (m4:translation (pos entity))
                       (m4:rotation-from-euler (rot entity))
                       (m4:scale (scale entity)))))

;;--------------------------------------------------------------
;; controls

(defun mouse-callback (moved &rest ignored)
  (declare (ignore ignored))
  (when (mouse-down-p mouse.left)
    (cond
      ;; move in z axis
      ((key-down-p key.lshift)
       (setf (pos *bird*)
             (v3:+ (pos *bird*) (v! 0 0 (/ (v:y moved) 100.0)))))
      ;; move in y axis
      ((key-down-p key.lctrl)
       (setf (pos *bird*)
             (v3:+ (pos *bird*) (v! 0 (/ (v:y moved) -100.0) 0))))
      ;; rotate
      (t (setf (rot *bird*)
               (v3:+ (rot *bird*) (v! (/ (v:y moved) -100.0)
                                      (/ (v:x moved) -100.0)
                                      0.0)))))))

;;--------------------------------------------------------------
;; window

(defun reshape (&optional (new-dimensions (current-viewport)))
  (setf (frame-size *camera*) new-dimensions)
  (map-g #'standard-pass nil :cam-to-clip (cam->clip *camera*))
  (map-g #'refract-pass nil :cam-to-clip (cam->clip *camera*)))

(defun window-size-callback (size &rest ignored)
  (declare (ignore ignored))
  (reshape size))

;;--------------------------------------------------------------
;; main loop

(let ((running nil))
  (defun run-loop ()
    (init)
    (setf running t)
    (whilst-listening-to ((#'window-size-callback (window 0) :size)
                          (#'mouse-callback (mouse 0) :move))
      (loop :while (and running (not (shutting-down-p))) :do
         (continuable
           (step-demo)
           (update-repl-link)))))
  (defun stop-loop () (setf running nil)))


(defun step-demo ()
  (step-host)
  (setf *loop-pos* (+ *loop-pos* 0.04))
  (setf (pos *light*) (v! (* 10 (sin (* 0.01 *loop-pos*)))
                          10
                          (* 10 (cos (* 0.01 *loop-pos*)))))
  (with-viewport (cam-viewport *camera*)
    (draw)))
