
;; 2025.04.06 byulparan@gmail.com
;; 
;; 

(in-package #:gfx)

(defun load-box-primitive ()
  (gfx:make-gpu-stream '((pos :vec3) (coord :vec2) (norm :vec3))
		       (list
			1.0 -1.0 1.0 1.0 1.0 0.0 -1.0 0.0 
	        	-1.0 -1.0 -1.0 0.0 0.0 0.0 -1.0 0.0 
			1.0 -1.0 -1.0 1.0 0.0 0.0 -1.0 0.0 
		        -1.0 1.0 -1.0 0.0 1.0   0.0 1.0 0.0 
			1.0 1.0  1.0 1.0 0.0  0.0 1.0 0.0 
			1.0 1.0 -1.0  1.0 1.0  0.0 1.0 0.0 
			1.0 1.0 -1.0  1.0 1.0   1.0 -0.0 0.0 
			1.0 -1.0 1.0 0.0 0.0  1.0 -0.0 0.0 
			1.0 -1.0 -1.0 1.0 0.0   1.0 -0.0 0.0 
			1.0 1.0 1.0  1.0 1.0  0.0 -0.0 1.0 
			-1.0 -1.0 1.0 0.0 0.0   0.0 -0.0 1.0 
			1.0 -1.0 1.0 1.0 0.0   0.0 -0.0 1.0 
			-1.0 -1.0 1.0  1.0 0.0   -1.0 -0.0 -0.0 
			-1.0 1.0 -1.0  0.0 1.0 -1.0 -0.0 -0.0 
			-1.0 -1.0 -1.0 0.0 0.0  -1.0 -0.0 -0.0 
			1.0 -1.0 -1.0 0.0 0.0   0.0 0.0 -1.0 
			-1.0 1.0 -1.0 1.0 1.0   0.0 0.0 -1.0 
			1.0 1.0 -1.0 0.0 1.0  0.0 0.0 -1.0 
			-1.0 -1.0 1.0  0.0 1.0   0.0 -1.0 0.0 
			-1.0 1.0 1.0  0.0 0.0     0.0 1.0 0.0 
			1.0 1.0 1.0 0.0 1.0  1.0 -0.0 0.0 
			-1.0 1.0 1.0 0.0 1.0   0.0 -0.0 1.0 
			-1.0 1.0 1.0 1.0 1.0  -1.0 -0.0 -0.0 
			-1.0 -1.0 -1.0 1.0 0.0  0.0 0.0 -1.0 )
		       :index-data
		       (list
			0 1 2
			3 4 5
			6 7 8
			9 10 11
			12 13 14
			15 16 17
			0 18 1
			3 19 4
			6 20 7
			9 21 10
			12 22 13
			15 23 16)))




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




