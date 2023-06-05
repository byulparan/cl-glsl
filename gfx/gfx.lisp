(in-package :gfx)
;;; utility
(defun deg-to-rad (deg)
  (coerce (* pi (/ deg 180.0)) 'single-float))

(defun get-internal-seconds ()
  (* (/ 1.0 INTERNAL-TIME-UNITS-PER-SECOND) (get-internal-real-time)))


;;; GPU STREAM
(defun type-size (type)
  (labels ((cnt (type)
	     (ecase type
	       (:vec3 3)
	       (:vec2 2)
	       (:float 1)
	       (:int 1))))
    (cnt type)))

(defun make-gpu-stream (type data &key index-data (core-profile t))
  (let* ((type-size (mapcar #'type-size (mapcar #'second type)))
	 (gpu-stream (make-%gpu-stream :types type
				       :strides (apply #'+ type-size)
				       :offsets (cons 0 (butlast type-size))
				       :names (mapcar #'first type)
				       :core-profile core-profile
				       :update-time (get-internal-real-time))))
    (gpu-stream-set gpu-stream data :index-data index-data)
    gpu-stream))

(defun gpu-stream-set (gpu-stream data &key index-data)
  (let* ((sizes (mapcar #'type-size (mapcar #'second (%gpu-stream-types gpu-stream))))
	 (size (apply #'+ sizes)))
    (etypecase data
      (list (let ((data-array (alexandria:flatten data)))
	      (setf (%gpu-stream-array gpu-stream)
		    (make-array (length data-array)
				:element-type 'single-float
				:initial-contents data-array
				#+lispworks :allocation
				#+lispworks :static)
		    (%gpu-stream-length gpu-stream) (/ (length data-array) size))))
      (fixnum (setf (%gpu-stream-array gpu-stream) (make-array (* size data)
							       :element-type 'single-float
							       #+lispworks :allocation
							       #+lispworks :static)
		    (%gpu-stream-length gpu-stream) data))
      ((simple-array single-float (*)) (setf (%gpu-stream-array gpu-stream) data
					     (%gpu-stream-length gpu-stream) (/ (length data) size))))
    (when index-data
      (etypecase index-data
	(list (let* ((index-array (alexandria:flatten index-data)))
		(setf (%gpu-stream-index-array gpu-stream)
		  (make-array (length index-array) :element-type '(unsigned-byte 32)
			      :initial-contents index-array))))
	((simple-array (unsigned-byte 32) (*)) (setf (%gpu-stream-index-array gpu-stream) index-data)))))
  (setf (%gpu-stream-update-time gpu-stream) (get-internal-real-time)))


(defun gpu-stream-length (gpu-stream)
  (if (%gpu-stream-index-array gpu-stream) (length (%gpu-stream-index-array gpu-stream))
    (%gpu-stream-length gpu-stream)))


(defun vertex (stream name index)
  (let* ((types (%gpu-stream-types stream))
	 (size (type-size (second (assoc name types))))
	 (offset (nth (position name types :key #'car) (%gpu-stream-offsets stream)))
	 (stride (%gpu-stream-strides stream))
	 (array (%gpu-stream-array stream))
	 (result-value (ecase size
			 (1 nil)
			 (2 (make-array 2 :element-type 'single-float))
			 (3 (make-array 3 :element-type 'single-float)))))
    (if (= size 1) (setf result-value (aref array (+  offset (* index stride))))
      (loop for i below size
	    do (setf (aref result-value i)  (aref array (+ i offset (* index stride))))))
    result-value))


(defun (setf vertex) (values stream name index)
  (let* ((types (%gpu-stream-types stream))
	 (size (type-size (second (assoc name types))))
	 (offset (nth (position name types :key #'car) (%gpu-stream-offsets stream)))
	 (stride (%gpu-stream-strides stream))
	 (array (%gpu-stream-array stream))
	 (in-values (if (numberp values) (list values) values)))
    (assert (= (length in-values) size))
    (loop for i below size
	  do (setf (aref array (+ i offset (* index stride))) (elt in-values i)))
    (setf (%gpu-stream-update-time stream) (get-internal-real-time))
    values))



;;; PIPELINE
(defvar *gfx-all-type*)
(defvar *gfx-function-table*)
(defvar *gfx-macro-table*)
(defvar *gfx-variable-table*)

;; 전역 함수 안에서 유니폼 변수에 접근 하는 우아하고 깔끔한 방법을 찾지 못했다.
;; 전역 함수는 오직 전역 변수, 함수 인자만 사용하도록 강제하는 방법도 있지만 유연성이 떨어지고
;; 오히려 코드 양이 늘어난다. 그래서 임시방편으로 전역 함수(defun-g) 안에서만 접근 할 수 있는
;; 유니폼 변수 전용 테이블을 만들어서 사용.
(defvar *gfx-uniform-table* nil
  "전역 함수 내에서 유니폼 변수들을 사용 할 용도의 변수 테이블.")

(defvar *gfx-spec-table*)

(defvar *all-pipeline-table* (make-hash-table))

(defvar *library-function-table* (alexandria:copy-hash-table glsl::*function-table*))
(defvar *library-macro-table* (alexandria:copy-hash-table glsl::*macro-table*))
(defvar *library-spec-table* (make-hash-table))

(defstruct gfx-spec name parent child type source kind connected-pipelines)



(defun reinit-shader-system ()
  (loop for lib being the hash-values of *library-spec-table*
	do (setf (gfx-spec-connected-pipelines lib) nil))
  (setf *gfx-all-type* (copy-list glsl::*all-glsl-type*)
	*gfx-uniform-table* (make-hash-table)
	*gfx-variable-table* (alexandria:copy-hash-table glsl::*variable-table*)
	*gfx-function-table* (alexandria:copy-hash-table *library-function-table*)
	*gfx-macro-table* (alexandria:copy-hash-table *library-macro-table*)
	*gfx-spec-table* (alexandria:copy-hash-table *library-spec-table*)))

(reinit-shader-system)

(defmacro with-update-global (name typeform kind &body body)
  `(let ((old-spec (gethash ',name *gfx-spec-table*)))
     (when old-spec
       (dolist (p (gfx-spec-parent old-spec))
	 (alexandria:removef (gfx-spec-child (gethash p *gfx-spec-table*)) ',name)))
     (let* ((glsl::*variable-count* 0)
	    (glsl::*function-table* *gfx-function-table*)
	    (glsl::*macro-table* *gfx-macro-table*)
	    (glsl::*variable-table* (if (eql ,kind :function) *gfx-uniform-table*
				      *gfx-variable-table*))
	    (glsl::*global-functions* nil)
	    (glsl::*used-global* nil)
	    (glsl::*return-value* nil)
	    (glsl::*all-glsl-type* *gfx-all-type*))
       (when (eql ,kind :function)
	 (loop for key being the hash-key of *gfx-variable-table*
		 using (hash-value value) 
	       do (setf (gethash key glsl::*variable-table*) value)))
       ,@body
       (let* ((new-type ,typeform))
	 (dolist (p glsl::*used-global*)
	   (pushnew ',name (gfx-spec-child (gethash p *gfx-spec-table*))))
	 (setf (gethash ',name *gfx-spec-table*)
	   (make-gfx-spec :name ',name
			  :parent glsl::*used-global*
			  :child (when old-spec (gfx-spec-child old-spec))
			  :type new-type
			  :source (car glsl::*global-functions*)
			  :kind ,kind
			  :connected-pipelines (when old-spec (gfx-spec-connected-pipelines old-spec))))
	 (when old-spec
	   (unless (equal (gfx-spec-type old-spec) new-type)
	     (dolist (c (gfx-spec-child old-spec))
	       (setf (gfx-spec-source (gethash c *gfx-spec-table*)) nil)))
	   (dolist (pipeline (gfx-spec-connected-pipelines old-spec))
	     (compile-pipeline (gethash pipeline *all-pipeline-table*)))))
       ',name)))

(defmacro defmacro-g (name args &body body)
  `(let* ((glsl::*macro-table* *gfx-macro-table*))
     (glsl::v-defmacro ,name ,args
       ,@body)))

(defmacro defun-g (name args &body body)
  (flet ((process-arg (arg)
	   (destructuring-bind (name type &optional scope count)
	       arg
	     (if (not count) type
	       (list :array type count)))))
    `(with-update-global ,name (list glsl::*return-value* ',(mapcar #'process-arg args))
	 :function
       (glsl::compile-form
	'(labels ((,name ,args ,@body)))))))

(defmacro defvar-g (name body)
  `(with-update-global ,name glsl::*return-value*
       :variable
     (glsl::compile-form
      '(labels ((,name ,body))))))

(defmacro defstruct-g (name &body form)
  `(let* ((glsl::*structures* nil)
	  (glsl::*function-table* *gfx-function-table*)
	  (glsl::*all-glsl-type* *gfx-all-type*)
	  (type-name ',name)
	  (new-type ',(mapcar #'(lambda (f) (if (third f) (list :array (second f) (third f)) (second f))) form))
	  (old-spec (gethash type-name *gfx-spec-table*))
	  (glsl::*used-global* nil))
     (when old-spec
       (dolist (p (gfx-spec-parent old-spec))
	 (alexandria:removef (gfx-spec-child (gethash p *gfx-spec-table*)) ',name)))
     (glsl::compile-form '(defstruct ,name ,@form))
     (dolist (p glsl::*used-global*)
       (pushnew ',name (gfx-spec-child (gethash p *gfx-spec-table*))))
     (setf *gfx-all-type* glsl::*all-glsl-type*)
     (setf (gethash type-name *gfx-spec-table*)
       (make-gfx-spec :name ',name
		      :parent glsl::*used-global*
		      :child (when old-spec (gfx-spec-child old-spec))
		      :type new-type
		      :source (car glsl::*structures*)
		      :kind :structure
		      :connected-pipelines (when old-spec (gfx-spec-connected-pipelines old-spec))))
     (when old-spec
       (unless (equal (gfx-spec-type old-spec) new-type)
	 (dolist (c (gfx-spec-child old-spec))
	   (setf (gfx-spec-source (gethash c *gfx-spec-table*)) nil)))
       (dolist (pipeline (gfx-spec-connected-pipelines old-spec))
	 (compile-pipeline (gethash pipeline *all-pipeline-table*))))
     ',name))

(defun get-parent-funcs (name)
  (cond ((consp name) (cons (get-parent-funcs (car name))
			    (get-parent-funcs (cdr name))))
	(t (alexandria:when-let
	       ((f-spec (gethash name *gfx-spec-table*)))
	     (cons name
		   (loop for parent in (gfx-spec-parent f-spec)
			 collect (get-parent-funcs parent)))))))


;;; uniform 변수를 밖으로 모아야 함.
(defun process-global (stream)
  (let* ((node (nreverse (remove-duplicates (alexandria:flatten (get-parent-funcs glsl::*used-global*))))))
    (setf glsl::*used-global* node)
    (dolist (s node)
      (let ((src (gfx-spec-source (gethash s *gfx-spec-table*))))
	(unless src (error "~a gfx-spec need recompile" (gfx-spec-name (gethash s *gfx-spec-table*))))
	(format stream src))
      (format stream "~%"))))

(defun compile-pipeline (pipeline)
  "compile s-exression to GLSL lang."
  (let* ((name (%pipeline-name pipeline))
	 (version (%pipeline-version pipeline))
	 (glsl::*all-glsl-type* *gfx-all-type*)
	 (glsl::*variable-table* *gfx-variable-table*)
	 (glsl::*function-table* *gfx-function-table*)
	 (glsl::*macro-table* *gfx-macro-table*)
	 (used-function nil))
    (dolist (f (%pipeline-used-funcs pipeline))
      (let* ((spec (gethash f *gfx-spec-table*)))
	(unless (gfx-spec-source spec)
	  (error "~a needs recompile" (gfx-spec-name spec)))))
    (setf (%pipeline-shader-src pipeline)
      (loop for source in (list (%pipeline-vertex-src pipeline) (%pipeline-geometry-src pipeline) (%pipeline-fragment-src pipeline))
	    for type in (list :vertex :geometry :fragment)
	    collect (when source
		      (let* ((glsl::*used-global* nil))
			(prog1
			      (glsl::process-shader
			       (glsl::make-shader :version version :type type
						  :io-spec (second source)
						  :uniforms (%pipeline-uniforms pipeline)
						  :source (third source)
						  :function #'process-global))
			  (alexandria:appendf used-function glsl::*used-global*))))))
    (loop for used-func in (%pipeline-used-funcs pipeline)
    	  do (alexandria:removef (gfx-spec-connected-pipelines (gethash used-func *gfx-spec-table*)) name))
    (setf (%pipeline-used-funcs pipeline) used-function)
    (loop for used-func in (%pipeline-used-funcs pipeline)
    	  do (pushnew name (gfx-spec-connected-pipelines (gethash used-func *gfx-spec-table*))))
    (setf (%pipeline-update-time pipeline) (get-internal-real-time))))


(defun pull-g (name)
  (alexandria:if-let ((pipeline (gethash name *all-pipeline-table*)))
    (%pipeline-shader-src pipeline)
    (alexandria:when-let ((func (gethash name *gfx-spec-table*)))
      func)))

(defmacro defpipeline (name uniforms &body stages)
  (assert (every (lambda (stage) (= 3 (length stage))) stages) nil "each stages should be have 3 element(type, io-spec, body)")
  (assert (every (lambda (stage) (find (car stage) '(:vertex :geometry :fragment))) stages) nil
	  "pipeline stage should be one of '(:vertex :geometry :fragment). but your stages are ~a" (mapcar #'car stages))
  (when (atom name) (setf name (list name :version glsl::*glsl-version*)))
  (let* ((version (third name))
	 (name (car name))
	 (stream-type (mapcar #'second (getf (second (first stages)) :in))))
    (alexandria:with-gensyms (pipeline legacy program)
      `(let* ((,legacy (gethash ',name *all-pipeline-table*))
	      (,pipeline (make-%pipeline :name ',name
					 :version ,version
					 :used-funcs (when ,legacy (%pipeline-used-funcs ,legacy))
					 :uniforms ',uniforms
					 :vertex-src ',(find :vertex stages :key #'car)
					 :geometry-src ',(find :geometry stages :key #'car)
					 :fragment-src ',(find :fragment stages :key #'car))))
	 (compile-pipeline ,pipeline)
	 (setf (gethash ',name *all-pipeline-table*) ,pipeline)))))

