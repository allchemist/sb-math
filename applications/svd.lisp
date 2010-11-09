(in-package :sb-math)

(defun pseudo-inverse (A)
  (multiple-value-bind (S U VT)
      (svd A :left :all :right :all)
    (gemm 
     (gemm VT (setf (diag (make-matrix-like A)) (map-matrix-/ S)) :transa :trans :transb :trans)
     U :transb :trans)))

(defun em-norm (A)
  (elt (svd A) 0))

(defun fm-norm (A)
  (sqrt (amsum (map-matrix-square (svd A)))))

(defun cond-number (A)
  (let* ((S (svd A))
	 (last-sv (aref S (1- (length S)))))
    (if (zerop last-sv)
	(error "Matrix is unconditioned: last singular value is zero")
	(/ (aref S 0) last-sv))))

(defun linear-least-squares (A Y)
  (gemv (pseudo-inverse A) Y))
