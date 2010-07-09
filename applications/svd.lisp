(in-package :sb-math)

(defun pseudo-inverse (A)
  (let ((diag (make-matrix (reverse (array-dimensions A))
			   :element-type (array-element-type A))))
  (multiple-value-bind (S U VT)
      (svd A :left :all :right :all :real-values t)
    (do-diag (diag i)
      (setf (aref diag i i) (/ (aref S i))))
    (gemm (gemm VT diag) :transa :trans) U :transb :trans)))

(defun em-norm (A)
  (elt (svd A) 0))

(defun fm-norm (A)
  (sqrt (asum (map-matrix
	       (svd A)
	      #'square))))

(defun cond-number (A)
  (let* ((S (svd A))
	 (last-sv (aref S (1- (length S)))))
    (if (zerop last-sv)
	(error "Matrix is unconditioned: last singular value is zero")
	(/ (aref S 0) last-sv))))

(defun linear-least-squares (A Y)
  (gemv (pseudo-inverse A) Y))
