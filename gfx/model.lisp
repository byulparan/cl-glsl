(in-package :gfx)

(defstruct model scene node-table meshes animation-transition)
(defstruct node name transform anim-index)
(defstruct mesh name node node-table bones gpu-stream textures)
(defstruct animation-transition current factor status)


(defun process-mesh (ai-mesh scene node node-table)
  (let* ((vertices nil)
	 (indices nil)
	 (textures nil)
	 (bone-ids nil)
	 (bone-weights nil)
	 (max-weight 8))
    (when (ai:bones ai-mesh)
      (setf bone-ids (make-array (length (ai:vertices ai-mesh)))
	    bone-weights (make-array (length (ai:vertices ai-mesh))))
      (dotimes (i (length (ai:vertices ai-mesh)))
	(setf (aref bone-ids i) (make-array max-weight :initial-element 0.0)
	      (aref bone-weights i) (make-array max-weight :initial-element 0.0)))
      (loop for bone across (ai:bones ai-mesh)
	    for bone-index from 0
	    do  (loop for weight across (ai:weights bone)
		      for vertex-id = (ai:id weight)
		      do 
			 (dotimes (i max-weight)
			   (when (= (aref (aref bone-weights vertex-id) i) 0.0)
			     (setf (aref (aref bone-ids vertex-id) i) (* 1.0 bone-index)
				   (aref (aref bone-weights vertex-id) i) (ai:weight weight))
			     (return))))))
    (dotimes (i (length (ai:vertices ai-mesh)))
      (let* ((position (aref (ai:vertices ai-mesh) i)))
	(push (aref position 0) vertices)
	(push (aref position 1) vertices)
	(push (aref position 2) vertices))
      (when (ai:texture-coords ai-mesh)
	(if (not (zerop (length (ai:texture-coords ai-mesh))))
	    (let* ((tex-coords (aref (aref (ai:texture-coords ai-mesh) 0) i)))
	      (push (aref tex-coords 0) vertices)
	      (push (aref tex-coords 1) vertices))
	  (progn
	    (push -1.0 vertices)
	    (push -1.0 vertices))))
      (when (ai:normals ai-mesh)
	(let* ((normal (aref (ai:normals ai-mesh) i)))
	  (push (aref normal 0) vertices)
	  (push (aref normal 1) vertices)
	  (push (aref normal 2) vertices)))
      (when (ai:bones ai-mesh)
	(let* ((id (aref bone-ids i))
	       (weight (aref bone-weights i)))
	  (dotimes (k max-weight)
	    (push (aref id k) vertices))
	  (dotimes (k max-weight)
	    (push (aref weight k) vertices)))))
    (dotimes (i (length (ai:faces ai-mesh)))
      (let* ((face (aref (ai:faces ai-mesh) i)))
	(dotimes (j (length face))
	  (push (aref face j) indices))))
    (let* ((material (aref (ai:materials scene) (ai:material-index ai-mesh))))
      (setf textures (mapcar #'caddr (gethash "$tex.file" material))))
    (make-mesh :name (ai:name ai-mesh)
	       :node node
	       :node-table node-table
	       :bones (ai:bones ai-mesh)
	       :gpu-stream (if (ai:bones ai-mesh) (gfx:make-gpu-stream '((pos :vec3) (coord :vec2) (norm :vec3) 
									 (bone-ids :vec4 2) (bone-weights :vec4 2))
								       (nreverse vertices)
								       :index-data (nreverse indices))
			     (gfx:make-gpu-stream '((pos :vec3) (coord :vec2) (norm :vec3))
						  (nreverse vertices)
						  :index-data (nreverse indices)))
	       :textures textures)))



;; ================================================================================

(defun load-model (filename &key (processing-flags '(:ai-process-triangulate)))
  "
 1. 전체 노드를 순회하면서 부모 노드의 트랜스폼을 곱하여 자신의 트랜스폼을 업데이트
 2. 순회 중 자신의 애니메이션 노드가 존재한다면 anim-index 해당 에니메이션 노드의 인덱스를 저장
 3. node-table 에 노드를 저장
 4. 순회 중 노드의 메쉬가 존재한다면 process-mesh 를 수행하여 메쉬 오브젝트 생성 후 모델에 저장
"
  (let* ((scene (ai:import-into-lisp (truename filename) :processing-flags processing-flags))
	 (animations (ai:animations scene))
	 (node-table (make-hash-table :test #'equal))
	 (meshes nil))
    (labels ((find-animation (name)
	       (loop for anim across animations
		     collect (loop for channel across (ai:channels anim)
			      for i from 0
			      do (when (string= name (ai:node-name channel))
				   (return i)))))
	     (process-node (ai-node parent-transform)
	       (let* ((transform (sb-cga:matrix* parent-transform
						 (sb-cga:transpose-matrix (ai:transform ai-node))))
		      (node (make-node :name (ai:name ai-node)
				       :anim-index (when animations (find-animation (ai:name ai-node)))
				       :transform transform)))
		 (setf (gethash (node-name node) node-table) node)
		 (loop for mesh-index across (ai:meshes ai-node)
		       do (pushnew (process-mesh (aref (ai:meshes scene) mesh-index) scene node node-table) meshes))
		 (loop for child across (ai:children ai-node)
		       do (process-node child transform)))))
      (process-node (ai:root-node scene) (sb-cga:identity-matrix))
      (make-model :scene scene
		  :node-table node-table
		  :meshes (nreverse meshes)))))


(defun calc-vector-interpolation (time keys)
  (flet ((find-key-index ()
	   (dotimes (i (- (length keys) 1))
	     (when (< time (ai:key-time (aref keys (+ i 1))))
	       (return i)))))
    (if (= (length keys) 1) (ai:value (aref keys 0))
      (let* ((key-index (find-key-index)))
	(if key-index
	    (let* ((delta-time (- (ai:key-time (aref keys (1+ key-index)))
				  (ai:key-time (aref keys key-index))))
		   (factor (/ (- time (ai:key-time (aref keys key-index))) delta-time))
		   (start-value (ai:value (aref keys key-index)))
		   (end-value (ai:value (aref keys (+ key-index 1)))))
	      (sb-cga:vec (float (+ (aref start-value 0) (* (- (aref end-value 0) (aref start-value 0)) factor)) 1.0)
			  (float (+ (aref start-value 1) (* (- (aref end-value 1) (aref start-value 1)) factor)) 1.0)
			  (float (+ (aref start-value 2) (* (- (aref end-value 2) (aref start-value 2)) factor)) 1.0)))
	  (ai:value (aref keys (- (length keys) 1))))))))


(defun calc-quat-interpolation (time keys)
  (flet ((find-key-index ()
	   (dotimes (i (- (length keys) 1))
	     (when (< time (ai:key-time (aref keys (+ i 1))))
	       (return i)))))
    (if (= (length keys) 1)  (ai:value (aref keys 0))
      (let* ((key-index (find-key-index)))
	(if key-index
	    (let* ((delta-time (- (ai:key-time (aref keys (1+ key-index)))
				  (ai:key-time (aref keys key-index))))
		   (factor (/ (- time (ai:key-time (aref keys key-index))) delta-time))
		   (start-value (ai:value (aref keys key-index)))
		   (end-value (ai:value (aref keys (+ key-index 1)))))
	      (nqlerp start-value end-value factor))
	  (ai:value (aref keys (- (length keys) 1))))))))



(defun animation (model index time)
  (assert (> (length (ai:animations (model-scene model))) index) nil
	  "Out of index to the animation the model has: ~d" index)
  (unless (model-animation-transition model)
    (setf (model-animation-transition model) (make-animation-transition :current index
									:factor .0
									:status :normal)))
  (let* ((animation (model-animation-transition model)))
    (when (and (/= (animation-transition-current animation) index))
      (setf (animation-transition-status animation) :transition))
    (if (eql (animation-transition-status animation) :transition)
	(progn
	  (animation-transition model time (animation-transition-current animation) index
				(animation-transition-factor animation))
	  (incf (animation-transition-factor animation) .05)
	  (when (> (animation-transition-factor animation) 1.0)
	    (setf (animation-transition-current animation) index
		  (animation-transition-factor animation) .0
		  (animation-transition-status animation) :normal)))
      (animation-loop model (animation-transition-current animation) time))))




(defun animation-loop (model index time)
  "
 1. 모든 노드는 자식의 트랜스폼에 영향을 끼치므로 전체 노드의 트랜스폼을 계산해야 함.
 2. 모든 노드를 순회하며 자신의 노드에 anim-index 가 존재한다면 해당 에니메이션의 트랜스폼을 자신의 트랜스폼으로 업데이트
 2. 만약 anim-index 가 없다면 부모 트랜스폼과 곱하여 자신의 트랜스폼을 업데이트
"
  (let* ((animation (aref (ai:animations (model-scene model)) index))
	 (duration (ai:duration animation)))
    (setf time (mod time duration))
    (labels ((read-node-hierarchy (ai-node parent-transform)
	       (let* ((name (ai:name ai-node))
		      (node (gethash name (model-node-table model)))
		      (node-transform (sb-cga:matrix* parent-transform
						      (sb-cga:transpose-matrix (ai:transform ai-node))))
		      (node-anim-index (nth index (node-anim-index node))))
		 (when node-anim-index
		   (let* ((node-anim (aref (ai:channels animation) node-anim-index)))
		     (let* ((pos (calc-vector-interpolation time (ai:position-keys node-anim)))
			    (quat (calc-quat-interpolation time (ai:rotation-keys node-anim)))
			    (scale (calc-vector-interpolation time (ai:scaling-keys node-anim))))
		       (setf node-transform
			 (sb-cga:matrix* parent-transform
					 (sb-cga:translate pos)
					 (quat->matrix quat)
					 (sb-cga:scale scale))))))
		 (setf (node-transform node) node-transform)
		 (loop for child across (ai:children ai-node)
		       do (read-node-hierarchy child node-transform)))))
      (read-node-hierarchy (ai:root-node (model-scene model)) (sb-cga:identity-matrix)))))



(defun animation-transition (model time start-index end-index factor)
  "transition from start-index to end-index on animations."
  (let* ((start-animation (aref (ai:animations (model-scene model)) start-index))
	 (time1 (mod time (ai:duration start-animation)))
	 (end-animation (aref (ai:animations (model-scene model)) end-index))
	 (time2 (mod time (ai:duration end-animation))))
    (labels ((read-node-hierarchy (ai-node parent-transform)
	       (let* ((name (ai:name ai-node))
		      (node (gethash name (model-node-table model)))
		      (node-transform (sb-cga:matrix* parent-transform
						      (sb-cga:transpose-matrix (ai:transform ai-node))))
		      (node-anim-index (node-anim-index node)))
		 (when (car node-anim-index)
		   (let* ((start-node-anim (aref (ai:channels start-animation) (nth start-index node-anim-index)))
			  (end-node-anim (aref (ai:channels end-animation) (nth end-index node-anim-index))))
		     (let* ((start-pos (calc-vector-interpolation time1 (ai:position-keys start-node-anim)))
			    (start-quat (calc-quat-interpolation time1 (ai:rotation-keys start-node-anim)))
			    (start-scale (calc-vector-interpolation time1 (ai:scaling-keys start-node-anim)))
			    (end-pos (calc-vector-interpolation time2 (ai:position-keys end-node-anim)))
			    (end-quat (calc-quat-interpolation time2 (ai:rotation-keys end-node-anim)))
			    (end-scale (calc-vector-interpolation time2 (ai:scaling-keys end-node-anim)))
			    (pos (sb-cga:vec+ start-pos (sb-cga:vec* (sb-cga:vec- end-pos start-pos) factor)))
			    (quat (nqlerp start-quat end-quat factor))
			    (scale (sb-cga:vec+ start-scale (sb-cga:vec* (sb-cga:vec- end-scale start-scale) factor))))
		       (setf node-transform
			 (sb-cga:matrix* parent-transform
					 (sb-cga:translate pos)
					 (quat->matrix quat)
					 (sb-cga:scale scale))))))
		 (setf (node-transform node) node-transform)
		 (loop for child across (ai:children ai-node)
		       do (read-node-hierarchy child node-transform)))))
      (read-node-hierarchy (ai:root-node (model-scene model)) (sb-cga:identity-matrix)))))




(defun mesh-bone-transforms (mesh)
  "메쉬의 bones 를 순회하며 node-table 에서 해당 bone 의 노드를 가져와 노드의 트랜스폼과 bone의 offset 트랜스폼을 계산"
  (assert (mesh-bones mesh) nil "Don't have bones ~a" mesh)
  (let* ((bones (mesh-bones mesh))
	 (bone-transforms (make-array (length bones))))
    (loop for bone across bones
	  for index from 0
	  do (let* ((node (gethash (ai:name bone) (mesh-node-table mesh)))
		    (matrix (sb-cga:matrix*
			     (node-transform node)
			     (sb-cga:transpose-matrix (ai:offset-matrix bone)))))
	       (setf (aref bone-transforms index) matrix)))
    bone-transforms))

(defun mesh-transform (mesh)
  (node-transform (mesh-node mesh)))


;; ================================================================================
;; utility
;; ================================================================================

(defun quat->matrix (q)
  (let ((w (- (aref q 0)))
        (x (aref q 1))
        (y (aref q 2))
        (z (aref q 3)))
    (declare (single-float w x y z))
    (sb-cga:matrix
     (- 1.0 (* 2.0 (+ (expt y 2) (expt z 2))))
     (* 2.0 (+ (* x y) (* z w)))
     (* 2.0 (- (* x z) (* y w)))
     0.0

     (* 2.0 (- (* x y) (* z w)))
     (- 1.0 (* 2.0 (+ (expt x 2) (expt z 2))))
     (* 2.0 (+ (* y z) (* x w)))
     0.0

     (* 2.0 (+ (* x z) (* y w)))
     (* 2.0 (- (* y z) (* x w)))
     (- 1.0 (* 2.0 (+ (expt x 2) (expt y 2))))
     0.0
     0.0 0.0 0.0 1.0)))


(defun nqlerp (a b f)
  (let ((f2 (- 1.0 f)))
    ;; make sure we get shortest path between orientations
    ;; (if (a dot b) < 0, negate b)
    (let ((d (+ (* (aref a 0) (aref b 0))
                (* (aref a 1) (aref b 1))
                (* (aref a 2) (aref b 2))
                (* (aref a 3) (aref b 3)))))
      (when (< d 0)
        (map-into b #'- b)))
    (macrolet ((dim (n)
                 `(+ (* f2 (aref a ,n)) (* f (aref b ,n)))))
      (let* ((r0 (dim 0))
             (r1 (dim 1))
             (r2 (dim 2))
             (r3 (dim 3))
             (l (sqrt (+ (expt r0 2) (expt r1 2) (expt r2 2) (expt r3 2)))))
        (make-array 4 :element-type 'single-float
                    :initial-contents (list (float (/ r0 l) 1f0)
                                            (float (/ r1 l) 1f0)
                                            (float (/ r2 l) 1f0)
                                            (float (/ r3 l) 1f0)))))))


