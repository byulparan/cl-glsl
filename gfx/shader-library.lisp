(defpackage :shader-lib
  (:shadowing-import-from #:gfx #:defpipeline :fill)
  (:nicknames :sl)
  (:use #:cl :glsl))

(in-package :shader-lib)

(defmacro define-function-library (name args &body body)
  (let ((newargs (mapcar #'(lambda (a) (intern (format nil "~a-~a" (first a) (second a)))) args)))
    `(progn
       (let* ((gfx::*gfx-spec-table* gfx::*library-spec-table*)
	      (gfx::*gfx-function-table* gfx::*library-function-table*))
	 (gfx:defun-g ,name ,args
	   ,@body)
	 (export ',name))
       (defun ,name ,newargs
	 (declare (ignore ,@newargs))))))

(defmacro define-macro-library (name args &body body)
  `(progn
     (defmacro ,name ,args
       ,@body)
     (let* ((glsl::*macro-table* gfx::*library-macro-table*))
       (glsl:v-defmacro ,name ,args
	 ,@body))
     (export ',name)))


(define-macro-library do-repeat ((var n) &body body)
  (cons 'progn
	(loop for i from 0 below n
	      collect `(let ((,var ,i))
			 ,@body))))


(define-function-library internal-op ((d1 :float) (d2 :float))
  (let* ((result 0.0))
    (if! (< d1 d2) (setf result d1)
	 (setf result d2))
    result))

(unexport 'internal-op)

(progn
  (let* ((glsl::*macro-table* gfx::*library-macro-table*))
    (v-defmacro op-u
	(d1 &body body)
      (if body
          (let* ((d2 (car body)) (item `(internal-op ,d1 ,d2)))
            (loop for b in (cdr body)
                  do (setf item `(internal-op ,item ,b)))
            item)
        d1)))
  (export 'op-u))


(define-function-library internal-op2 ((d1 :vec2) (d2 :vec2))
  (let* ((result (v! 0.0 0.0)))
    (if! (< (x d1) (x d2)) (setf result d1)
	 (setf result d2))
    result))

(unexport 'internal-op2)

(progn
  (let* ((glsl::*macro-table* gfx::*library-macro-table*))
    (v-defmacro op-u2
	(d1 &body body)
      (if body
          (let* ((d2 (car body)) (item `(internal-op2 ,d1 ,d2)))
            (loop for b in body
                  do (setf item `(internal-op2 ,item ,b)))
            item)
        d1)))
  (export 'op-u2))




(define-function-library internal-op-s ((d1 :float) (d2 :float))
  (let* ((result 0.0))
    (if! (> (- d2) d1)
	 (setf result (- d2))
	 (setf result d1))
    result))

(unexport 'internal-op-s)

(progn
  (let* ((glsl::*macro-table* gfx::*library-macro-table*))
    (v-defmacro op-s
	(d1 &body body)
      (if body
          (let* ((d2 (car body)) (item `(internal-op-s ,d1 ,d2)))
            (loop for b in (cdr body)
                  do (setf item `(internal-op-s ,item ,b)))
            item)
        d1)))
  (export 'op-s))


(define-function-library op-s2 ((d1 :vec2) (d2 :vec2))
  (let* ((result (v! 0.0 0.0)))
    (if! (> (- (x d1)) (x d2))
	 (setf result (v! (- (x d1)) (y d1)))
	 (setf result d2))
    result))




(define-function-library sd-plane ((p :vec3) (n :vec4))
  (+ (dot p (xyz n)) (w n)))

(define-function-library sd-base ((p :vec3))
  (y p))

(define-function-library sd-box ((p :vec3) (b :vec3))
  (let* ((d (- (abs p) b)))
    (+ (min (max (x d) (max (y d) (z d))) 0.0) (length (max d 0.0)))))

(define-function-library sd-sphere ((p :vec3) (s :float))
  (- (length p) s))

(define-function-library sd-cylinder ((p :vec3) (a :vec3) (b :vec3) (r :float))
  (let* ((ab (- b a))
	 (ap (- p a))
	 (t (/ (dot ab ap) (dot ab ab))))
    (let* ((c (+ a (* t ab)))
	   (x (- (length (- p c)) r))
	   (y (* (- (abs (- t .5)) .5) (length ab)))
	   (e (length (max (v! x y) 0.0)))
	   (i (min (max x y) 0.0)))
      (+ e i))))

(define-function-library sd-torus ((p :vec3) (r :vec2))
  (let* ((x (- (length (xz p)) (x r))))
    (- (length (v! x (y p))) (y r))))

(define-function-library sd-capture ((p :vec3) (a :vec3) (b :vec3) (r :float))
  (let* ((ab (- b a))
	 (ap (- p a))
	 (t (/ (dot ab ap) (dot ab ab))))
    (setf t (clamp t .0 1.0))
    (let* ((c (+ a (* t ab))))
      (- (length (- p c)) r))))



(define-function-library xrot ((tt :float))
  (m! 1.0 0.0 0.0
      0.0 (cos tt) (- (sin tt))
      0.0 (sin tt) (cos tt)))

(define-function-library yrot ((tt :float))
  (m! (cos tt) 0.0 (- (sin tt))
      0.0 1.0 0.0
      (sin tt) 0.0 (cos tt)))

(define-function-library zrot ((tt :float))
  (m! (cos tt) (- (sin tt)) 0.0
      (sin tt) (cos tt) 0.0
      0.0 0.0 1.0))


(define-function-library rotate-2d ((r :float))
  (m! (cos r) (sin r) (- (sin r)) (cos r)))

(define-function-library rotate-3d ((angle :float) (axis :vec3))
  (let* ((a (normalize axis))
	 (s (sin angle))
	 (c (cos angle))
	 (r (- 1.0 c)))
    (m! (+ (* (x a) (x a) r) c)
	(+ (* (y a) (x a) r) (* (z a) s))
	(- (* (z a) (x a) r) (* (y a) s))
	(- (* (x a) (y a) r) (* (z a) s))
	(+ (* (y a) (y a) r) c)
	(+ (* (z a) (y a) r) (* (x a) s))
	(+ (* (x a) (z a) r) (* (y a) s))
	(- (* (y a) (z a) r) (* (x a) s))
	(+ (* (z a) (z a) r) c))))


(define-function-library translate ((vec :vec3))
  (transpose
   (m! 1.0 0.0 0.0 (x vec)
       0.0 1.0 0.0 (y vec)
       0.0 0.0 1.0 (z vec)
       0.0 0.0 0.0 1.0)))

(define-function-library rotate ((angle :float) (vec :vec3))
  (let* ((rot (rotate-3d angle vec)))
    (m! (aref rot 0) 0.0
	(aref rot 1) 0.0
	(aref rot 2) 0.0
	(vec3 0.0) 1.0)))

(define-function-library scale ((vec :vec3))
  (m! (x vec) 0.0 0.0 0.0
      0.0 (y vec) 0.0 0.0
      0.0 0.0 (z vec) 0.0
      0.0 0.0 0.0 1.0))




(define-function-library rand11 ((a :float))
  (fract (* (sin a) 10403.9)))

(define-function-library rand12 ((f :float))
  (fract (* (cos f) (v! 10003.579 37049.7))))

(define-function-library rand21 ((uv :vec2))
  (let* ((f (+ (x uv) (* (y uv) 37.0))))
    (fract (* (sin f) 104003.9))))

(define-function-library rand22 ((uv :vec2))
  (let* ((f (+ (x uv) (* (y uv) 37.0))))
    (fract (* (cos f) (v! 10003.579 37049.7)))))



;; 
;;

(gfx:reinit-shader-system)
