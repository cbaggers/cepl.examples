(in-package :cepl.examples)

(defparameter *tex* nil)
(defparameter *stream* nil)
(defparameter *running* nil)
(defparameter *sam* nil)
(defparameter *sam2* nil)

(defun-g sampling-vert ((vert g-pt))
  (values (v! (pos vert) 1.0) (tex vert)))

(defun-g sampling-frag ((tc :vec2) &uniform (tex :sampler-2d))
  (texture tex tc))

(def-g-> sample-it ()
  (sampling-vert g-pt)
  (sampling-frag :vec2))

(defun-t flip ()
  (repeat (before (seconds 1) t)
          (before (seconds 1) nil)))

(defun step-demo ()
  (step-host)
  (update-repl-link)
  (clear)
  (map-g #'sample-it *stream* :tex (if (flip) *sam* *sam2*))
  (swap))

(defun run-loop ()
  (setf *running* t
        *stream* (make-buffer-stream
                  (make-gpu-array (list (list (v!  0.5 -0.36 0) (v! -1 1))
                                        (list (v!    0   0.5 0) (v! 1 1))
                                        (list (v! -0.5 -0.36 0) (v! 0 -1)))
                                  :element-type 'g-pt)
                  :retain-arrays t)
        *tex* (cepl.sdl2-image:load-image-to-texture
               (merge-pathnames "brick/col.png" *examples-dir*))
        *sam* (sample *tex*)
        *sam2* (sample *tex* :wrap :clamp-to-edge))
  (loop :while *running* :do (continuable (step-demo))))

(defun stop-loop ()
  (setf *running* nil))
