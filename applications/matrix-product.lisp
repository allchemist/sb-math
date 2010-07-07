(in-package :sb-math)

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
