(in-package #:gfx)

(defun load-box-primitive ()
  (gfx:make-gpu-stream '((pos :vec3) (coord :vec2) (norm :vec3) )
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
