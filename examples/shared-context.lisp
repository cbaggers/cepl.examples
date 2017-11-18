(in-package :cepl.examples)

(defparameter *running* nil)
(defparameter *quad-stream* nil)
(defparameter *some-sampler* nil)

(defun-g pass-through-vert ((vert g-pt))
  (values (v! (pos vert) 1) (tex vert)))

(defun-g blit-frag ((uv :vec2) &uniform (sam :sampler-2d))
  (texture sam uv))

(defpipeline-g blit-it ()
  (pass-through-vert g-pt)
  (blit-frag :vec2))

(defun reset ()
  (setf *quad-stream*
        (make-buffer-stream
         (make-gpu-array
          (list (list (v! -1.0   1.0 0 0) (v!  0.0   1.0))
                (list (v! -1.0  -1.0 0 0) (v!  0.0   0.0))
                (list (v!  1.0  -1.0 0 0) (v!  1.0   0.0))
                (list (v! -1.0   1.0 0 0) (v!  0.0   1.0))
                (list (v!  1.0  -1.0 0 0) (v!  1.0   0.0))
                (list (v!  1.0   1.0 0 0) (v!  1.0   1.0)))
          :element-type 'g-pt
          :dimensions 6)
         :retain-arrays t))

  ;; This is hacky but is to check that an object made on one context
  ;; can be used on the other.
  (let ((ctx (cepl.context:make-context-shared-with-current-context)))
    (bt:make-thread
     (lambda ()
       (with-cepl-context (woop ctx)
         (setf *some-sampler*
               (sample
                (dirt:load-image-to-texture
                 (asdf:system-relative-pathname
                  :cepl.examples "examples/g.png"))))
         (gl:finish)
         (cepl.context::free-context woop))))
    nil))

(defun step-main ()
  (clear)
  (map-g #'blit-it *quad-stream* :sam *some-sampler*)
  (swap))

(defun run-loop ()
  (reset)
  (format t "-starting-")
  (setf *running* t)
  (unwind-protect
       (loop :while *running* :do
          (continuable
            (update-repl-link)
            (step-host)
            (step-main)))
    (print "-shutting down-")
    (setf *running* nil)))

(defun stop-loop () (setf *running* nil))
