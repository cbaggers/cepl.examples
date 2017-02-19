(in-package :cepl.examples)

(defstruct field fbo sampler)

(defparameter field-a nil)
(defparameter field-b nil)
(defparameter *quad* nil)
(defparameter *quad-stream* nil)

(defun-g pass-through-vert ((vert g-pt))
  (values (v! (pos vert) 1) (tex vert)))

(defun-g pass-through-frag ((tc :vec2) &uniform (board :sampler-2d))
  (let ((c (v:x (texture board tc))))
    (v! c 0 c 0)))

(defun-g the-meat! ((tc :vec2) &uniform (board :sampler-2d))
  (let* ((offset (/ 1.0 1024.0))
         (score (v:x (+ (texture board (+ tc (v! (- offset) (- offset))))
                        (texture board (+ tc (v! (- offset)  0)))
                        (texture board (+ tc (v! (- offset)  offset)))
                        (texture board (+ tc (v!  0 (- offset))))
                        (texture board (+ tc (v!  0  offset)))
                        (texture board (+ tc (v!  offset (- offset))))
                        (texture board (+ tc (v!  offset  0)))
                        (texture board (+ tc (v!  offset  offset))))))
         (current (texture board tc)))
    (cond
      ((or (< score 2) (> score 3)) (v! 0 0 0 0))
      ((and (= score 3) (= (v:x current) 0)) (v! 1 0 0 0))
      (t current))))

(def-g-> copy-pass () #'pass-through-vert #'pass-through-frag)
(def-g-> gol-pass () #'pass-through-vert #'the-meat!)

(defun gol-render (source destination)
  (map-g-into (field-fbo destination) #'gol-pass *quad-stream*
              :board (field-sampler source))
  (map-g #'copy-pass *quad-stream* :board (field-sampler destination)))

(let ((flip-flop t))
  (defun game-o-life ()
    (if (setf flip-flop (not flip-flop))
        (gol-render field-b field-a)
        (gol-render field-a field-b))))

(defun init_ ()
  (let* ((a (make-c-array
             (loop for i below 1024 collect
                  (loop for i below 1024 collect (if (= 1 (random 5))
                                                     (v!uint8 255 0 0 0)
                                                     (v!uint8 0 0 0 0))))
             :dimensions '(1024 1024) :element-type :uint8-vec4))
         (tex-a (make-texture a))
         (tex-b (make-texture a)))
    (setf field-a (make-field :fbo (make-fbo `(0 ,tex-a))
                              :sampler (sample tex-a))
          field-b (make-field :fbo (make-fbo `(0 ,tex-b))
                              :sampler (sample tex-b))
          *quad* (make-gpu-array
                  (list (list (v! -1.0   1.0 0 0) (v!  0.0   1.0))
                        (list (v! -1.0  -1.0 0 0) (v!  0.0   0.0))
                        (list (v!  1.0  -1.0 0 0) (v!  1.0   0.0))
                        (list (v! -1.0   1.0 0 0) (v!  0.0   1.0))
                        (list (v!  1.0  -1.0 0 0) (v!  1.0   0.0))
                        (list (v!  1.0   1.0 0 0) (v!  1.0   1.0)))
                  :element-type 'g-pt
                  :dimensions 6)
          *quad-stream* (make-buffer-stream *quad* :retain-arrays t))))

(defun step-main ()
  (clear)
  (game-o-life)
  (swap))

(let ((running nil))
  (defun run-loop ()
    (init_)
    (setf running t)
    (format t "-starting-")
    (loop :while running
       :do (continuable
             (update-repl-link)
             (step-host)
             (step-main)))
    (print "-shutting down-")
    nil)
  (defun stop-loop () (setf running nil)))
