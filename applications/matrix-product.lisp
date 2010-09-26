(in-package :sb-math)

#|
;; generic matrix product

(defgeneric matrix-product (m1 m2 &rest keys &key dest &allow-other-keys))

(defmethod matrix-product ((m1 vector) (m2 vector)
			   &rest keys &key dest &allow-other-keys)
  (print 'vector-to-vector)
  (apply #'ger m1 m2 :dest dest keys))

(defmethod matrix-product ((m1 array) (m2 vector)
			   &rest keys &key dest &allow-other-keys)
  (print 'matrix-to-vector)
  (apply #'gemv m1 m2 :dest dest keys)) 

(defmethod matrix-product ((m1 array) (m2 array)
			   &rest keys &key dest &allow-other-keys)
  (print 'matrix-to-matrix)
  (apply #'gemm m1 m2 :dest dest keys))

(defmethod matrix-product ((m1 triangular-matrix) (m2 vector)
			   &rest keys &key dest &allow-other-keys)
  (print 'triangular-to-vector)
  (apply #'tpmv m1 (if dest (copy m2 dest) (copy m2)) keys))


(defmethod matrix-product ((m1 symmetric-matrix) (m2 vector)
			   &rest keys &key dest &allow-other-keys)
  (print 'symmeric-to-vector)
  (apply #'spmv m1 m2 :dest dest keys))

(defmethod matrix-product ((m1 hermitian-matrix) (m2 vector)
			   &rest keys &key dest &allow-other-keys)
  (print 'hermitian-to-vector)
  (apply #'hpmv m1 m2 :dest dest keys))
|#

;; basis rotation

(defun orthogonal-basis-transform (basis rotation)
  (gemm (gemm rotation basis) rotation :transb :trans))

(defun general-basis-transform (basis rotation)
  (gemm (gemm rotation basis) (lu-inverse rotation)))

;; chain matrix multiplication

(defun cmm3 (m1 m2 m3)
  (if (< (+ (* (dim0 m1) (dim1 m1) (dim1 m2))
	    (* (dim0 m1) (dim1 m2) (dim1 m3)))
	 (+ (* (dim0 m2) (dim1 m2) (dim1 m3))
	    (* (dim0 m1) (dim1 m1) (dim1 m3))))
      (gemm (gemm m1 m2) m3)
      (gemm m1 (gemm m2 m3))))
