(in-package :sb-math)

(export '(csplines make-csplines spline-piece-idx eval-splines splines-coords splines-values splines-coeffs smooth))

;; cubic splines

(defstruct (csplines
	     (:conc-name splines-)
	     (:constructor init-csplines))
  coords values coeffs)

(defun make-csplines (coords vals &key (start-bound-cond 0) (end-bound-cond 0))
  (let ((n (dim0 coords)))
    (assert (= n (dim0 vals)))
    (unless (typep coords '(simple-array double-float)) (setf coords (coerce-matrix coords 'double-float)))
    (unless (typep vals   '(simple-array double-float)) (setf vals   (coerce-matrix vals 'double-float)))
    (init-csplines
     :coords coords
     :values vals
     :coeffs (%cubspl coords (setf (col (make-matrix `(,n 4) :element-type 'double-float) 0) vals)
		      start-bound-cond end-bound-cond))))

;; splines evaluation

(defun spline-piece-idx (coord splines)
  (let* ((coords (splines-coords splines))
	 (dim (dim0 coords)))
    (cond ((<= coord (aref coords 0)) 0)
	  ((>= coord (aref coords (1- dim))) (1- dim))
	  (t (1- (position-if #'(lambda (x) (> x coord)) coords))))))

(defun eval-splines (coord splines &key (deriv-idx 0))
  (let* ((coerce-needed (not (typep coord 'double-float)))
	 (val
	  (%ppvalu (splines-coords splines)
		   (splines-coeffs splines)
		   (spline-piece-idx coord splines)
		   4 (if coerce-needed (coerce coord 'double-float) coord) deriv-idx)))
    (if coerce-needed (coerce val 'single-float) val)))

(defun smooth (coords values new-coords &key
	       (start-bound-cond 0) (end-bound-cond 0) (deriv-idx 0))
  (let ((out (if (arrayp new-coords)
		 (copy new-coords)
		 (copy (setf new-coords
			     (linspace (aref coords 0)
				       (aref coords (1- (dim0 coords)))
				       new-coords)))))
	(spl (make-csplines coords values
			    :start-bound-cond start-bound-cond
			    :end-bound-cond end-bound-cond)))
    (dotimes (i (dim0 out))
      (setf (aref out i) (eval-splines (aref out i) spl :deriv-idx deriv-idx)))
    (values out new-coords)))
