;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
;;
;; ======================================================
;; This is an example camera class.
;; It doesnt so much special and isnt very optimized but
;; it exists to show that cepl doesnt require you to use
;; the cepl.camera library to make a camera.
;;
;; This camera also doesnt use the 'vec-space type from cepl
;; so this shows what code looks like without that
;; feature
;; ======================================================

(in-package :cepl.examples.camera)

;;;--------------------------------------------------------------

(defclass camera ()
  ((cam->clip :type (simple-array single-float (16)) :reader cam->clip)
   (viewport :initform (make-viewport) :initarg :viewport :reader cam-viewport)
   (near :type single-float :reader near :initarg :near)
   (far :type single-float :reader far :initarg :far)
   (fov :type single-float :reader fov :initarg :fov)))


(defgeneric update-cam->clip (camera))
(defgeneric look-at (camera point-vec3))

(defgeneric (setf near) (distance camera))
(defgeneric (setf far) (distance camera))
(defgeneric (setf fov) (angle camera))
(defgeneric frame-size (camera))
(defgeneric (setf frame-size) (frame camera))

(defmethod update-cam->clip ((camera camera))
  (setf (slot-value camera 'cam->clip)
        (rtg-math.projection:perspective
         (coerce (first (frame-size camera)) 'single-float)
         (coerce (second (frame-size camera)) 'single-float)
         (coerce (near camera) 'single-float)
         (coerce (far camera) 'single-float)
         (coerce (fov camera) 'single-float))))

(defmethod (setf near) (distance (camera camera))
  (setf (slot-value camera 'near) distance)
  (update-cam->clip camera))

(defmethod (setf far) (distance (camera camera))
  (setf (slot-value camera 'far) distance)
  (update-cam->clip camera))

(defmethod (setf fov) (angle (camera camera))
  (setf (slot-value camera 'fov) angle)
  (update-cam->clip camera))

(defmethod frame-size ((camera camera))
  (viewport-dimensions (cam-viewport camera)))

(defmethod (setf frame-size) (frame (camera camera))
  (let ((frame
         (etypecase frame
            (simple-array (list (aref frame 0)
				(aref frame 1)))
            (cepl:viewport (cepl:viewport-dimensions frame))
            (list frame))))
    (setf (viewport-dimensions (cam-viewport camera)) frame))
  (update-cam->clip camera))

(defgeneric world->cam (camera))

(defclass pos-dir-cam (camera)
  ((world-up :type (simple-array single-float (3))
             :initform (v3:make 0.0 1.0 0.0)
             :initarg :world-up
             :accessor world-up)
   (position :type (simple-array single-float (3))
             :initform (v3:make 0.0 0.0 0.0)
             :initarg :pos
             :accessor pos)
   (direction :type (simple-array single-float (3))
              :initform (v3:make 0.0 0.0 -1.0)
              :initarg :dir
              :accessor dir)))

(defmethod look-at ((camera pos-dir-cam) point-vec3)
  (with-slots (world-up position direction) camera
    (setf direction (v3:normalize (v3:- point-vec3 position)))))

(defmethod world->cam ((camera pos-dir-cam))
  (with-slots (world-up position direction) camera
    (let* ((up (v3:normalize
                (v3:- world-up
                        (v3:*s direction (v3:dot world-up direction)))))
           (side (v3:cross direction up))
           (rotate (m3:from-rows side up (v3:negate direction)))
           (eye-inv (v3:negate (m3:*v rotate position)))
           (result (m4:rotation-from-mat3 rotate)))
      (setf (m4:melm result 0 3) (aref eye-inv 0)
            (m4:melm result 1 3) (aref eye-inv 1)
            (m4:melm result 2 3) (aref eye-inv 2))
      result)))

(defun make-camera (&optional (near 1.0) (far 1000.0) (fov 120.0))
  (let* ((camera (make-instance 'pos-dir-cam :near near :far far :fov fov)))
    (update-cam->clip camera)
    camera))

;;;--------------------------------------------------------------
