(in-package :gfx)

(defclass camera ()
  ((eye-x :accessor eye-x) 
   (eye-y :accessor eye-y)
   (eye-z :accessor eye-z)
   (center-x :accessor center-x)
   (center-y :accessor center-y)
   (center-z :accessor center-z)
   (up-x :accessor up-x)
   (up-y :accessor up-y)
   (up-z :accessor up-z)
   (azimuth :accessor azimuth)
   (zenith :accessor zenith))
  (:documentation "Camera objects useful for viewing 3D scenes. Gives you a very simple to use interactive camera to view your scene."))

(defun track-mouse-zoom (camera dx dy Gain)
  (declare (ignore dx))
  (let* ((vx (- (center-x camera) (eye-x camera)))
         (vy (- (center-y camera) (eye-y camera)))
         (vz (- (center-z camera) (eye-z camera)))
         (length (sqrt (+ (* vx vx) (* vy vy) (* vz vz))))
         (distance (* (- Dy) Gain)))
    (setq vx (/ vx Length))
    (setq vy (/ vy Length))
    (setq vz (/ vz Length))
    (when (< distance length)
      (setf  (eye-x camera) (+ (eye-x camera) (* vx Distance))
	     (eye-y camera) (+ (eye-y camera) (* vy Distance))
	     (eye-z camera) (+ (eye-z camera) (* vz Distance))))))

(defun track-mouse-spin (camera dx dy Gain)
  (flet ((POLAR->CATHESIAN (Radius Azimuth Zenith)
	     (values
		  (* Radius (cos Zenith) (sin Azimuth) )
		  (* Radius (sin Zenith))
		  (* Radius (cos Zenith) (cos Azimuth)))))
    (let* ((eye-x (eye-x camera))
	   (eye-y (eye-y camera))
	   (eye-z (eye-z camera))
	   (center-x (center-x camera))
	   (center-y (center-y camera))
	   (center-z (center-z camera))
	   (vx (- Eye-x Center-x))
	   (vy (- Eye-y Center-y))
	   (vz (- Eye-z Center-z))
	   (r (sqrt (+ (expt vx 2) (expt vy 2) (expt vz 2)))))
      (incf (azimuth camera) (* dx (* 0.4 Gain)))
      (incf (zenith camera) (* (- dy) (* 0.4 Gain)))
      (multiple-value-bind (x2 y2 z2)
	  (polar->cathesian r (+ (azimuth camera) pi) (- (/ pi 2) (zenith camera)))
	(multiple-value-bind (x3 y3 z3) 
	    (polar->cathesian r (+ (azimuth camera) pi) (+ (- (/ pi 2) (zenith camera)) 1.0))
	  (setf (eye-x camera) (coerce (+ center-x x2) 'single-float)
		(eye-y camera) (coerce (+ center-y y2) 'single-float)
		(eye-z camera) (coerce (+ center-z z2) 'single-float)
		(up-x camera) (coerce (- x3 x2) 'single-float)
		(up-y camera) (coerce (- y3 y2) 'single-float)
		(up-z camera) (coerce (- z3 z2) 'single-float)))))))

(defun track-mouse-pan (camera dx dy Gain)
  (let ((x (- (eye-x camera) (center-x camera)))
        (y (- (eye-y camera) (center-y camera)))
        (z (- (eye-z camera) (center-z camera)))
        (center-x (center-x camera))
        (center-y (center-y camera))
        (center-z (center-z camera)))
    (let* ((sina (sin (- (azimuth camera))))
           (cosa (cos (- (azimuth camera))))
           (mx (* dx -1.2 Gain))
           (my (* dy +1.2 Gain))
           (dex (+ (* mx cosa ) (- (* my sina (cos (zenith camera))))))
           (dey  (+ (* mx sina ) (* my cosa  (cos (zenith camera)))) )
           (dez   (* my (sin (zenith camera))) ))
      (setf (eye-x camera) (float (+ center-x x dex) 1.0)
	    (eye-y camera) (float (+ center-y y dez) 1.0)
	    (eye-z camera) (float (+ center-z z dey) 1.0)
	    (center-x camera) (float (+ center-x dex) 1.0)
	    (center-y camera) (float (+ center-y dez) 1.0)
	    (center-z camera) (float (+ center-z dey) 1.0)))))

(defun compute-polar-orientation (camera)
  (flet ((carthesian->polar (x y z)
	   (values
	    (if (zerop z)
		(if (> x 0)
		    #.(/ pi 2)
		    #.(/ pi -2))
	      (let ((result (atan (/ x z))))
		(if (< z 0) 
		    (+ pi Result)
		  Result)))
	    (asin (/ y (sqrt (+ (expt x 2) (expt y 2) (expt z 2))))))))
    (multiple-value-bind (azimuth zenith)
	(carthesian->polar (- (eye-x camera) (center-x camera))
			   (- (eye-y camera) (center-y camera))
			   (- (eye-z camera) (center-z camera)))
      (setf (azimuth camera) (+ pi azimuth)
	    (zenith camera) (- (* pi .5) zenith)))))


(defun reset-camera (camera &key (eye-x 0.0) (eye-y 0.0) (eye-z 5.0)
			      (center-x 0.0) (center-y 0.0) (center-z 0.0))
  "Set the camera’s position and target."
  (setf (eye-x camera) eye-x
	(eye-y camera) eye-y
	(eye-z camera) eye-z)
  (setf (center-x camera) center-x
	(center-y camera) center-y
	(center-z camera) center-z)
  (setf (up-x camera) 0.0
	(up-y camera) 1.0
	(up-z camera) 0.0)
  (compute-polar-orientation camera)
  camera)


(defun look-at (camera)
  "Perform the lookAt function through the camera object. This is useful for calculating the view matrix."
  (kit.math:look-at (kit.math:vec3 (eye-x camera) (eye-y camera) (eye-z camera))
		     (kit.math:vec3 (center-x camera) (center-y camera) (center-z camera))
		     (kit.math:vec3 (up-x camera) (up-y camera) (up-z camera))))



(defun camera-position (camera)
  "Set the position of the camera."
  (list (eye-x camera) (eye-y camera) (eye-z camera)))

(defun camera-target (camera)
  "Set the target of the camera."
  (list (center-x camera) (center-y camera) (center-z camera)))



(defmethod initialize-instance :after ((self camera) &key (eye-x 0.0) (eye-y 0.0) (eye-z 5.0)
						       (center-x 0.0) (center-y 0.0) (center-z 0.0))
  (reset-camera self :eye-x eye-x :eye-y eye-y :eye-z eye-z
		:center-x center-x :center-y center-y :center-z center-z))


