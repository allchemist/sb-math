(in-package :sb-math)

(defstruct (csplines (:constructor init-csplines))
  coords values coeffs)

(defun make-csplines (coords vals &key (start-bound 0) (end-bound 0))
  (let ((n (dim0 coords)))
    (assert (= n (dim0 vals)))
    (init-csplines
     :coords coords
     :values vals
     :coeffs (%cubspl coords (setf (col (make-matrix `(,n 4) :element-type 'double-float) 0) vals)
		      start-bound end-bound))))

(defun cspline-piece-idx (coord csplines)
  (let* ((coords (csplines-coords csplines))
	 (pos (position-if #'(lambda (x) (> x coord)) coords)))
    (or
     pos
     (1- (dim0 coords)))))

(defun eval-csplines (coord csplines &key (deriv-idx 0))
  (coerce
   (%ppvalu (csplines-coords csplines)
	    (csplines-coeffs csplines)
	    (cspline-piece-idx coord csplines)
	    4 (coerce coord 'double-float) deriv-idx)
   (type-of coord)))
