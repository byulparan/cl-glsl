
;; 2025.04.06 byulparan@gmail.com
;; 
;; 

(in-package #:gfx)

(defun load-box-primitive (&key (width 1.0) (height 1.0) (depth 1.0))
  (flet ((generate-box-mesh (width height depth)
	   (let ((vertices '())
		 (indices '())
		 (i 0)) ; current index
	     (let* ((hx (/ width 2.0))
		    (hy (/ height 2.0))
		    (hz (/ depth 2.0)))
	       (flet ((v (x y z) (list x y z))
		      (add-quad (p0 p1 p2 p3 normal)
			;; Adds 4 vertices and 2 triangles (6 indices)
			(let ((start-i i))
			  (push (append p0 '(0.0 0.0) normal) vertices)
			  (push (append p1 '(1.0 0.0) normal) vertices)
			  (push (append p2 '(1.0 1.0) normal) vertices)
			  (push (append p3 '(0.0 1.0) normal) vertices)
			  (push (+ start-i 0) indices)
			  (push (+ start-i 1) indices)
			  (push (+ start-i 2) indices)
			  (push (+ start-i 0) indices)
			  (push (+ start-i 2) indices)
			  (push (+ start-i 3) indices)
			  (setf i (+ i 4)))))
		 ;; Front face (+Z)
		 (add-quad
		  (v (- hx) (- hy) hz) (v hx (- hy) hz)
		  (v hx hy hz) (v (- hx) hy hz)
		  '(0.0 0.0 1.0))
		 ;; Back face (-Z)
		 (add-quad
		  (v hx (- hy) (- hz)) (v (- hx) (- hy) (- hz))
		  (v (- hx) hy (- hz)) (v hx hy (- hz))
		  '(0.0 0.0 -1.0))
		 ;; Left face (-X)
		 (add-quad
		  (v (- hx) (- hy) (- hz)) (v (- hx) (- hy) hz)
		  (v (- hx) hy hz) (v (- hx) hy (- hz))
		  '(-1.0 0.0 0.0))
		 ;; Right face (+X)
		 (add-quad
		  (v hx (- hy) hz) (v hx (- hy) (- hz))
		  (v hx hy (- hz)) (v hx hy hz)
		  '(1.0 0.0 0.0))
		 ;; Top face (+Y)
		 (add-quad
		  (v (- hx) hy hz) (v hx hy hz)
		  (v hx hy (- hz)) (v (- hx) hy (- hz))
		  '(0.0 1.0 0.0))
		 ;; Bottom face (-Y)
		 (add-quad
		  (v (- hx) (- hy) (- hz)) (v hx (- hy) (- hz))
		  (v hx (- hy) hz) (v (- hx) (- hy) hz)
		  '(0.0 -1.0 0.0))
		 ;; Return values
		 (values (alexandria:flatten (reverse vertices)) (reverse indices)))))))
    (multiple-value-bind (vertices indices)
	(generate-box-mesh width height depth)
      (gfx:make-gpu-stream '((pos :vec3) (coord :vec2) (norm :vec3))
			   vertices :index-data indices))))




(defun load-sphere-primitive (&key (radius 1.0) (sectors 36) (stacks 18))
  "Create function from ChatGPT"
  (flet ((generate-sphere-mesh (radius sectors stacks)
	   (let ((vertices '())
		 (indices '()))
	     (let* ((m-pi 3.141592653589793)
		    (sector-step (/ (* 2 m-pi) sectors))
		    (stack-step (/ m-pi stacks)))
	       (dotimes (i (+ stacks 1))
		 (let* ((stack-angle (- (/ m-pi 2) (* i stack-step)))
			(xy (* radius (cos stack-angle)))
			(z (* radius (sin stack-angle))))
		   (dotimes (j (+ sectors 1))
		     (let* ((sector-angle (* j sector-step))
			    ;; position
			    (x (* xy (cos sector-angle)))
			    (y (* xy (sin sector-angle)))
			    (position (list x y z))
			    ;; normal
			    (len (sqrt (+ (* x x) (* y y) (* z z))))
			    (normal (list (/ x len) (/ y len) (/ z len)))
			    ;; texcoord
			    (u (/ j (float sectors)))
			    (v (/ i (float stacks)))
			    (texcoord (list u v)))
		       ;; append vertex to list
		       (push (list :position position :normal normal :texcoord texcoord) vertices)))))
	       ;; index generation
	       (dotimes (i stacks)
		 (dotimes (j sectors)
		   (let* ((first (+ (* i (+ sectors 1)) j))
			  (second (+ first sectors 1)))
		     (setf indices (append indices
					   (list first second (+ first 1)
						 second (+ second 1) (+ first 1))))))))
	     ;; reverse vertices to correct order (because push used)
	     (values (reverse vertices) indices))))
    (multiple-value-bind (vertices indices)
	(generate-sphere-mesh radius sectors stacks)
      (gfx:make-gpu-stream '((pos :vec3) (coord :vec2) (norm :vec3))
			   (loop for data in vertices
				 appending (append (getf data :position)
						   (getf data :texcoord)
						   (getf data :normal)))
			   :index-data indices))))




