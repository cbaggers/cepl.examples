(in-package :cepl.examples)

;; hacky script to let me run all the tests for a few seconds each so I can
;; spot any potential issues

(defun run-em-all ()
  (swank:set-default-directory
   (uiop:pathname-directory-pathname
    (asdf:system-relative-pathname :cepl.examples "examples/")))
  (flet ((run-test (package path)
           (let ((path (print
                        (asdf:system-relative-pathname
                         :cepl.examples (format nil "examples/~a" path)))))
             (compile-file path)
             (load path))
           (print "starting")
           (let ((std-o *standard-output*))
             (bt:make-thread
              (lambda ()
                (sleep 4)
                (print "Stopping" std-o)
                (force-output)
                (funcall (symbol-function (find-symbol "STOP-LOOP" package))))))
           (funcall (symbol-function (find-symbol "RUN-LOOP" package)))
           (cls)))
    (let ((examples '("bloom.lisp"
                      "game-of-life.lisp"
                      "inline-glsl.lisp"
                      "instance-array-triangles.lisp"
                      "lambda-pipelines.lisp"
                      "moving-triangles.lisp"
                      "raymarcher.lisp"
                      "sampling.lisp"
                      "shared-context.lisp"
                      "texture-example.lisp"
                      "transform-feedback.lisp"
                      "triangle.lisp"
                      "ubo-test.lisp"))
          (plus-cam '("basic-3d-objects.lisp"
                      "basic-geometry-shader.lisp"
                      "blending.lisp"
                      "cubemap.lisp"
                      "instancing.lisp"
                      "normal-mapping.lisp"
                      "refraction.lisp"
                      "tessellation-inline-glsl.lisp"
                      "tessellation.lisp")))
      (loop :for path :in examples :do
         (run-test :cepl.examples path))
      (loop :for path :in plus-cam :do
         (run-test :cepl.examples+camera path)))))
