(in-package :cepl.examples+camera)

;; NOTE: Ensure you have loaded cepl-image-helper (or cepl-default)

(defvar tx)
(defvar strm nil)
(defvar cam)

(defun-g vert ((vert :vec3) &uniform (mod-clip :mat4))
  (values (* mod-clip (v! vert 1))
          vert))

(defun-g frag ((tc :vec3) &uniform (tex :sampler-cube))
  (texture tex tc))

(defpipeline skybox () (g-> #'vert #'frag))

(defun make-cubemap-tex (&rest paths)
  (with-c-arrays (ca (mapcar (lambda (p)
                               (devil-helper:load-image-to-c-array
                                (merge-pathnames p *examples-dir*)))
                             paths))
    (make-texture ca :element-type :rgb8 :cubes t)))

(defun init ()
  (setf tx (make-cubemap-tex "ThickCloudsWater/left.png"
                             "ThickCloudsWater/right.png"
                             "ThickCloudsWater/up.png"
                             "ThickCloudsWater/down.png"
                             "ThickCloudsWater/front.png"
                             "ThickCloudsWater/back.png"))
  (let* ((bx (dendrite.primitives:box-data))
         (data (make-gpu-array (first bx) :element-type 'g-pnt))
         (ind (make-gpu-array (dendrite.primitives:swap-winding-order (second bx))
                              :element-type :ushort)))
    (setf strm (make-buffer-stream data :index-array ind
                                   :retain-arrays t)))
  (setf cam (make-camera)))

(defun step-demo ()
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (map-g #'skybox strm :tex tx :mod-clip (m4:m* (cam->clip cam) (world->cam cam)))
  (update-display))

(defvar mouse-ang (v! 0 0))
(evt:def-named-event-node mouse-listener (e evt:|mouse|)
  (when (typep e 'evt:mouse-motion)
    (let ((d (evt:delta e)))
      (setf mouse-ang (v2:+ (v! (/ (v:x d) -150.0)
                                (/ (v:y d) -150.0))
                            mouse-ang)
            (dir cam) (v! (sin (v:x mouse-ang))
                          (sin (v:y mouse-ang))
                          (cos (v:x mouse-ang)))))))

(let ((live::running nil))
  (defun run-loop ()
    (init)
    (setf live::running t)
    (format t "-starting-")
    (loop :while live::running
       :do (continuable
	     (update-swank)
	     (cepl.events:pump-events)
	     (step-demo)))
    (print "-shutting down-")
    nil)
  (defun stop-loop () (setf live::running nil)))

(evt:def-named-event-node sys-listener (e evt:|sys|)
  (when (typep e 'evt:will-quit) (stop-loop)))
