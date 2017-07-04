;; Texturing and fragment effects
(in-package :cepl.examples)

(defparameter *count* 0.0)
(defparameter *texture* nil)
(defparameter *sampler* nil)
(defparameter *v-stream* nil)

(defun-g tex-vert ((vert g-pt))
  (values (v! (pos vert) 1)
          (:smooth (tex vert))))

(defun-g tex-frag ((tex-coord :vec2) &uniform (tex :sampler-2d)
                   (count :float) (pos-offset :vec4))
  (let* ((rip-size 0.02) (centre (v! 0.0 0.0)) (damp 0.6)
         (peaks 9.0)
         (dif (- tex-coord centre))
         (dist (dot dif dif))
         (height (/ (+ (cos (+ count (* dist peaks)))
                       (sin (- count (* (y tex-coord) peaks))))
                    2.0))
         (rip-offset (* (* (normalize dif) rip-size) height damp)))
    (+ (texture tex (+ rip-offset tex-coord))
       (v! (* -0.2 height) (* -0.2 height) 0.0 0.0))))

(def-g-> ripple-with-wobble ()
  (tex-vert g-pt) (tex-frag :vec2))

(defun step-demo ()
  (step-host)
  (update-repl-link)
  (clear)
  (map-g #'ripple-with-wobble *v-stream*
         :tex *sampler* :count *count* :pos-offset (v! 0 0 0 0))
  (incf *count* 0.08)
  (swap))

(let ((running nil))
  (defun run-loop ()
    (setf running t)
    (let* ((img-data (loop :for i :below 64 :collect
                        (loop :for j :below 64 :collect (random 254)))))
      (setf *v-stream*
            (make-buffer-stream
             (make-gpu-array `((,(v! -0.5 -0.366 0) ,(v! -1  1))
                               (,(v!  0.5 -0.366 0) ,(v!  1  1))
                               (,(v!    0    0.5 0) ,(v!  0 -1)))
                             :dimensions 3 :element-type 'g-pt)
             :retain-arrays t))
      (setf *texture* (with-c-array
                          (temp (make-c-array img-data :dimensions '(64 64)
                                              :element-type :uint8))
                        (make-texture temp)))
      (setf *sampler* (sample *texture*))
      (loop :while (and running (not (shutting-down-p))) :do
         (continuable (step-demo)))))
  (defun stop-loop () (setf running nil)))
