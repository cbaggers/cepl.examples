(in-package :cepl.examples+camera)

;;--------------------------------------------------------------
;; setup

(defparameter *wibble* nil)
(defparameter *camera* nil)
(defparameter *light* nil)
(defparameter *tex* nil)
(defparameter *sampler* nil)
(defparameter *normal-map* nil)
(defparameter *normal-sampler* nil)
(defparameter *loop-pos* 0.0)

(defclass entity ()
  ((gstream :initform nil :initarg :gstream :accessor gstream)
   (position :initform (v! 0 0 -1) :initarg :pos :accessor pos)
   (rotation :initform (v! 0 0 0) :initarg :rot :accessor rot)
   (scale :initform (v! 1 1 1) :initarg :scale :accessor scale)
   (mesh :initarg :mesh :reader mesh)))

(defclass light ()
  ((position :initform (v! 20 20 -20) :initarg :pos :accessor pos)
   (radius :initform 1.0 :initarg :radius :accessor radius)))

(defun load-model (filename &optional hard-rotate)
  (let* ((result (first (cepl.examples.model-parsers:load-file filename)))
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
                     :pos (v! 0 -0.3 -3) :mesh mesh~1))))

(defun init ()
  (setf *light* (make-instance 'light))
  (setf *camera* (make-camera))
  (reshape (current-viewport))
  (let ((wibble-path (merge-pathnames "./wibble.3ds" *examples-dir*))
        (brick-dif-path  (merge-pathnames "./brick/col.png" *examples-dir*))
        (brick-norm-path  (merge-pathnames "./brick/norm.png" *examples-dir*)))
    (setf *wibble* (load-model wibble-path (v! pi 0 0)))
    (setf *tex* (dirt:load-image-to-texture brick-dif-path))
    (setf *sampler* (sample *tex*))
    (setf *normal-map* (dirt:load-image-to-texture brick-norm-path))
    (setf *normal-sampler* (sample *normal-map*))))

;;--------------------------------------------------------------
;; drawing

(defun-g nm-vert ((data g-pnt) &uniform (model-to-cam :mat4) (cam-to-clip :mat4))
  (values (* cam-to-clip (* model-to-cam (v! (pos data) 1.0)))
          (pos data)
          (norm data)
          (v! 0.4 0 0.4 0)
          (tex data)))

(defun-g nm-frag ((model-space-pos :vec3) (vertex-normal :vec3)
                  (diffuse-color :vec4) (tex-coord :vec2) &uniform
                  (model-space-light-pos :vec3) (light-intensity :vec4)
                  (ambient-intensity :vec4) (textur :sampler-2d)
                  (norm-map :sampler-2d))
  (let* ((light-dir (normalize (- model-space-light-pos
                                  model-space-pos)))
         (t-norm (- (* (s~ (texture norm-map tex-coord) :xyz) 2)
                    (v! 1 1 1)))
         (cos-ang-incidence
          (clamp (dot (normalize (* (+ vertex-normal t-norm) 0.5)) light-dir)
                 0.0 1.0))
         (t-col (texture textur tex-coord)))
    (+ (* t-col light-intensity cos-ang-incidence)
       (* t-col ambient-intensity))))

(def-g-> frag-point-light ()
  (nm-vert g-pnt)
  (nm-frag :vec3 :vec3 :vec4 :vec2)
  :post #'reshape)

(defun entity-matrix (entity)
  (reduce #'m4:* (list (m4:translation (pos entity))
                       (m4:rotation-from-euler (rot entity))
                       (m4:scale (scale entity)))))

(defun draw ()
  (clear)
  (let* ((world-to-cam-matrix (world->cam *camera*))
         (model-to-cam-matrix (m4:* world-to-cam-matrix
                                    (entity-matrix *wibble*)))
         ;;(normal-to-cam-matrix (m4:to-matrix3 model-to-cam-matrix))
         (cam-light-vec (m4:*v (entity-matrix *wibble*)
                               (v! (pos *light*) 1.0))))
    (map-g #'frag-point-light (gstream *wibble*)
           :model-space-light-pos (v:s~ cam-light-vec :xyz)
           :light-intensity (v! 1 1 1 0)
           :model-to-cam model-to-cam-matrix
           :norm-map *normal-sampler*
           :ambient-intensity (v! 0.2 0.2 0.2 1.0)
           :textur *sampler*))
  (swap))

;;--------------------------------------------------------------
;; controls

(defun mouse-callback (moved &rest ignored)
  (declare (ignore ignored))
  (when (mouse-down-p mouse.left )
    (cond
      ;; move in z axis
      ((key-down-p key.lshift)
       (setf (pos *wibble*)
             (v3:+ (pos *wibble*) (v! 0 0 (/ (v:y moved) 100.0)))))
      ;; move in y axis
      ((key-down-p key.lctrl)
       (setf (pos *wibble*)
             (v3:+ (pos *wibble*) (v! 0 (/ (v:y moved) -100.0) 0))))
      ;; rotate
      (t (setf (rot *wibble*)
               (v:+ (rot *wibble*) (v! (/ (v:y moved) -100.0)
                                       (/ (v:x moved) -100.0)
                                       0.0)))))))

;;--------------------------------------------------------------
;; window

(defun reshape (&optional (new-dimensions (current-viewport)))
  (setf (frame-size *camera*) new-dimensions)
  (map-g #'frag-point-light nil :cam-to-clip (cam->clip *camera*)))

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
  (setf *loop-pos* (+ *loop-pos* 0.02))
  (setf (pos *light*) (v! (* 10 (sin *loop-pos*))
                          10
                          (* 10 (cos *loop-pos*))))
  (with-viewport (cam-viewport *camera*)
    (draw)))
