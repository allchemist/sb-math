(in-package :sb-math)

(export '(csplines make-csplines spline-piece-idx eval-splines splines-coords splines-values splines-coeffs smooth-splines))

;; cubic splines

(defstruct (csplines
	     (:conc-name splines-)
	     (:constructor init-csplines))
  coords values coeffs)

(defun make-csplines (coords vals &key (start-bound 0) (end-bound 0))
  (let ((n (dim0 coords)))
    (assert (= n (dim0 vals)))
    (init-csplines
     :coords coords
     :values vals
     :coeffs (%cubspl coords (setf (col (make-matrix `(,n 4) :element-type 'double-float) 0) vals)
		      start-bound end-bound))))

;; splines evaluation

(defun spline-piece-idx (coord splines)
  (let* ((coords (splines-coords splines))
	 (pos (position-if #'(lambda (x) (> x coord)) coords)))
    (or
     pos
     (1- (dim0 coords)))))

(defun eval-splines (coord splines &key (deriv-idx 0))
  (coerce
   (%ppvalu (splines-coords splines)
	    (splines-coeffs splines)
	    (spline-piece-idx coord splines)
	    4 (coerce coord 'double-float) deriv-idx)
   (type-of coord)))

(defun smooth-splines (vector splines linspace-params &key (deriv-idx 0))
  (let ((result (make-matrix (third linspace-params)))
	(coords (apply #'linspace linspace-params)))
    (dolist (i (length coords))
      (setf (aref result i)
	    (eval-splines (elt coords i) splines :deriv-idx deriv-idx)))
    result))
    
