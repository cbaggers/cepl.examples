;; fragment point light - unfinished
(in-package :cepl.examples+camera)

;; NOTE: Ensure you have loaded cepl-image-helper & cepl-model-helper
;;       (or just load cepl-default)

;;--------------------------------------------------------------
;; setup

(defparameter *wibble* nil)
(defparameter *camera* nil)
(defparameter *light* nil)
(defparameter *tex* nil)
(defparameter *tex-sampler* nil)
(defparameter *pos-tex* nil)
(defparameter *pos-sampler* nil)
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
  (let* ((result (second (cepl.examples.model-parsers:load-file
                          filename)))
         (mesh (make-instance 'cepl.examples.meshes:mesh
                              :primitive-type :triangles
                              :vertices (first result)
                              :index (second result)))
         (mesh~1 (if hard-rotate
                     (cepl.examples.meshes:transform-mesh mesh :rotation hard-rotate)
                     mesh)))
    (let ((gstream (make-buffer-stream
		    (cepl.examples.meshes:vertices mesh)
		    :index-array (cepl.examples.meshes:indicies mesh))))
      (make-instance 'entity :rot (v! 1.57079633 1 0) :gstream gstream
                     :pos (v! 0 -0.4 -1) :mesh mesh~1))))

(defun init ()
  (setf *light* (make-instance 'light))
  (setf *camera* (make-camera))
  (setf *wibble* (load-model "./bird/bird.3ds"))
  (setf *tex* (cepl.sdl2-image:load-image-to-texture "./bird/char_bird_col.png"))
  (setf *pos-tex* (make-texture nil :dimensions 1000
                                :element-type :rgba32f
                                :buffer-storage t))
  (setf *tex-sampler* (sample *tex*))
  (setf *pos-sampler* (sample *pos-tex*))
  (push-g (loop :for i :below 1000 :collect
	     (v! (- (random 20.0) 10) (- (random 20.0) 10)
		 (- -20 (random 10.0)) 1))
	  *pos-tex*))

;;--------------------------------------------------------------
;; drawing

(defun-g instance-vert ((data g-pnt) &uniform (model-to-cam :mat4)
                        (cam-to-clip :mat4) (offsets :sampler-buffer))
  (values (let ((tpos (texel-fetch offsets gl-instance-id)))
            (+ (* cam-to-clip (* model-to-cam (v! (pos data) 1.0)))
               tpos
               (v! 0 0 -4 7)))
          (pos data)
          (norm data)
          (v! 0.4 0 0.4 0)
          (tex data)))

(defun-g instance-frag ((model-space-pos :vec3) (vertex-normal :vec3)
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
         (t-col (texture textur (* (v! 1 -1) tex-coord))))
    (+ (* t-col light-intensity cos-ang-incidence)
       (* t-col ambient-intensity))))

(def-g-> instanced-birds ()
  #'instance-vert #'instance-frag
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
    (with-instances 1000
      (map-g #'instanced-birds (gstream *wibble*)
             :model-space-light-pos (v:s~ cam-light-vec :xyz)
             :light-intensity (v! 1 1 1 0)
             :model-to-cam model-to-cam-matrix
             ;; :normal-model-to-cam normal-to-cam-matrix
             :ambient-intensity (v! 0.2 0.2 0.2 1.0)
             :textur *tex-sampler*
             :offsets *pos-sampler*)))
  (swap))

;;--------------------------------------------------------------
;; controls

(defun mouse-callback (event &rest ignored)
  (declare (ignore ignored))
  (let ((d (skitter:xy-pos-relative event)))
    (setf (rot *wibble*) (v:+ (rot *wibble*) (v! (/ (v:y d) -100.0)
						 (/ (v:x d) -100.0)
						 0.0)))))

;;--------------------------------------------------------------
;; window

(defun reshape (&optional (new-dimensions (current-viewport)))
  (setf (frame-size *camera*) new-dimensions)
  (map-g #'instanced-birds nil :cam-to-clip (cam->clip *camera*)))

(defun window-size-callback (event &rest ignored)
  (declare (ignore ignored))
  (reshape (skitter:size-2d-vec event)))

;;--------------------------------------------------------------
;; main loop

(let ((running nil))
  (defun run-loop ()
    (init)
    (setf running t)
    (skitter:whilst-listening-to
	((#'window-size-callback (skitter:window 0) :size)
	 (#'mouse-callback (skitter:mouse 0) :pos))
      (loop :while (and running (not (shutting-down-p))) :do
	 (continuable
	   (step-demo)
	   (update-repl-link)))))
  (defun stop-loop () (setf running nil)))

(defun step-demo ()
  (step-host)
  (setf *loop-pos* (+ *loop-pos* 0.04))
  (setf (pos *light*) (v! (* 10 (sin *loop-pos*))
                          10
                          (* 10 (cos *loop-pos*))))
  (draw))
