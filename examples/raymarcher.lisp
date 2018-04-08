(in-package #:cepl.examples)

;; Simple Raymarcher

(defparameter *gpu-array* nil)
(defparameter *vertex-stream* nil)
(defparameter *loop* 0.0)

(defun-g calc-normal ((func (function (:vec3) :float))
                                 (pos :vec3))
  (normalize
   (v! (- (funcall func (+ pos (v! 0.01 0.0 0.0)))
          (funcall func (- pos (v! 0.01 0.0 0.0))))
       (- (funcall func (+ pos (v! 0.0 0.01 0.0)))
          (funcall func (- pos (v! 0.0 0.01 0.0))))
       (- (funcall func (+ pos (v! 0.0 0.0 0.01)))
          (funcall func (- pos (v! 0.0 0.0 0.01)))))))

(defun-g sphere ((p :vec3) (r :float))
  (- (length p) r))

(defun-g box ((p :vec3) (b :vec3))
  (let ((d (- (abs p) b)))
    (+ (min (max (x d) (max (y d) (z d))) 0.0)
       (length (max d 0.0)))))

(defun-g thing2 ((p :vec3) (r :float) (l :float))
  (+ (* 0.2 (+ (y p)
               (cos (+ (* (+ (cos l) 6.0)
                          (* 2 (x p))) l))
               (* 3 (sin (* 3 l)))))
     (- (length p) r)))

(defun-g ray-vert ((position :vec4))
  (values position
          (swizzle position :xy)))

(defun-g ray-frag ((posxy :vec2) &uniform (loop :float) (eye-pos :vec3))
  (let* ((eye-pos (v! 0.0 0.0 -5.0))
         (eye-dir (normalize (v! (x posxy) (y posxy) 1.0)))
         (e eye-pos)
         (output (v! 0.03 0.03 0.05)))
    (flet ((dist ((p :vec3))
             (thing2 p 1.4 loop)))
      (for (i 0) (< i 20) (++ i)
           (let ((d (dist e)))
             (when (<= d 0.0)
               (let* ((norm (calc-normal #'dist e))
                      (ldir (normalize (v! 1 1 -1)))
                      (wat-color (v! 0.3 0.0 (+ 0.5 (* 0.4 (y norm)))))
                      (lam (dot norm ldir))
                      (light (clamp (+ 0.2 lam) 0 1)))
                 (setf output (* wat-color light))
                 (break)))
             (setf d (max d 0.01))
             (setf e (+ e (* eye-dir d))))))
    (v! output 1.0)))

(defpipeline-g raymarcher ()
  (ray-vert :vec4)
  (ray-frag :vec2))

(let ((running nil))
  (defun run-loop ()
    (setf *gpu-array* (make-gpu-array (list (v! -1.0  -1.0  0.0  1.0)
                                            (v!  1.0  -1.0  0.0  1.0)
                                            (v!  1.0   1.0  0.0  1.0)
                                            (v!  1.0   1.0  0.0  1.0)
                                            (v! -1.0   1.0  0.0  1.0)
                                            (v! -1.0  -1.0  0.0  1.0))
                                      :element-type :vec4
                                      :dimensions 6))
    (setf *vertex-stream* (make-buffer-stream *gpu-array*))
    (setf running t)
    (loop :while (and running (not (shutting-down-p))) :do
       (continuable (draw))))
  (defun stop-loop () (setf running nil)))


(defun draw ()
  (step-host)
  (update-repl-link)
  (setf *loop* (+ 0.01 *loop*))
  (clear)
  (map-g #'raymarcher *vertex-stream* :loop *loop* :eye-pos (v! 0 0 -5))
  (swap))
