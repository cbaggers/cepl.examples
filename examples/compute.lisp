(in-package :cepl.examples)

(defstruct-g (ssbo-test-data :layout std-430)
  (vals (:int 30)))

(defvar *ssbo* nil)
(defvar *running* nil)

(defun-g yay-compute (&uniform
                      (woop ssbo-test-data :ssbo)
                      (modifier :float))
  (declare (local-size :x 1 :y 1 :z 1))
  (setf (aref (ssbo-test-data-vals woop)
              (int (x gl-work-group-id)))
        (int (* (x gl-work-group-id) modifier)))
  (values))

(defpipeline-g run-compute ()
  :compute yay-compute)

(defun init ()
  (unless *ssbo*
    (let ((arr (make-gpu-array
                nil
                :dimensions 1
                :element-type 'ssbo-test-data)))
      (setf *ssbo* (make-ssbo arr)))))

(defun step-example (modifier)
  (map-g #'run-compute (make-compute-space 30) :woop *ssbo*
         :modifier (float modifier 0f0))
  (let ((fence (make-gpu-fence)))
    (wait-on-gpu-fence fence)
    (free fence)
    (print (pull-g (ssbo-data *ssbo*)))
    (sleep 1)))

(defun run-loop ()
  (setf *running* t)
  (init)
  (unwind-protect
       (loop :while (and *running* (not (shutting-down-p)))
          :for modifier :from 1 :do
          (update-repl-link)
          (continuable (step-example modifier)))
    (setf *running* nil)))

(defun stop-loop ()
  (setf *running* nil))
