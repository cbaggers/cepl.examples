(in-package :cepl.examples)

(defvar field-a nil)
(defvar field-b nil)
(defvar *quad* nil)
(defvar *quad-stream* nil)

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

(defpipeline copy-pass () (g-> #'pass-through-vert #'pass-through-frag))
(defpipeline gol-pass () (g-> #'pass-through-vert #'the-meat!))

(defpipeline gol-render (fbo-current fbo-new)
    (g-> (fbo-new (gol-pass *quad-stream* :board (attachment fbo-current 0)))
         (nil (copy-pass *quad-stream* :board (attachment fbo-new 0)))))

(let ((flip-flop t))
  (defun game-o-life ()
    (if (setf flip-flop (not flip-flop))
        (map-g #'gol-render field-b field-a)
        (map-g #'gol-render field-a field-b))))

(defun init_ ()
  (let ((a (make-c-array
            (loop for i below 1024 collect
                 (loop for i below 1024 collect (if (= 1 (random 5))
                                                    (v!ubyte 255 0 0 0)
                                                    (v!ubyte 0 0 0 0))))
            :dimensions '(1024 1024) :element-type :ubyte-vec4)))
    (setf field-a (make-fbo `(:c ,(make-texture a)))
          field-b (make-fbo `(:c ,(make-texture a)))
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
