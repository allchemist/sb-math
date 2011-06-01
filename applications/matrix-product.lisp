(in-package :sb-math)

(export '(mm cmm orthogonal-bais-transform general-basis-transform))

;; generic matrix product

(defgeneric mm (m1 m2 &rest keys &key dest &allow-other-keys))

(defmethod mm ((m1 vector) (m2 vector)
			   &rest keys &key dest &allow-other-keys)
  (apply #'ger m1 m2 :dest dest keys))

(defmethod mm ((m1 array) (m2 vector)
			   &rest keys &key dest &allow-other-keys)
  (apply #'gemv m1 m2 :dest dest keys))

(defmethod mm ((m1 array) (m2 array)
			   &rest keys &key dest &allow-other-keys)
  (apply #'gemm m1 m2 :dest dest keys))

#|(defmethod matrix-product ((m1 triangular-matrix) (m2 vector)
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
  (apply #'hpmv m1 m2 :dest dest keys))|#

;; basis rotation

(defun orthogonal-basis-transform (basis rotation &key pure)
  (gemm (gemm rotation basis :dest (unless pure basis))
	rotation :transb :trans :dest (unless pure basis)))

(defun general-basis-transform (basis rotation &key pure)
  (gemm (gemm rotation basis :dest (unless pure basis))
	(inv rotation :pure pure) :dest (unless pure basis)))

;; chain matrix multiplication

(defun cmm (&rest matrices)
  (ecase (length matrices)
    (0 nil)
    (1 (first matrices))
    (2 (apply #'gemm matrices))
    (3 (apply #'cmm3 matrices))
    (4 (apply #'cmm4 matrices))))

(defun cmm3 (m1 m2 m3)
  (if (< (+ (* (dim0 m1) (dim1 m1) (dim1 m2))
	    (* (dim0 m1) (dim1 m2) (dim1 m3)))
	 (+ (* (dim0 m2) (dim1 m2) (dim1 m3))
	    (* (dim0 m1) (dim1 m1) (dim1 m3))))
      (gemm (gemm m1 m2) m3)
      (gemm m1 (gemm m2 m3))))

(defun cmm4 (m1 m2 m3 m4)
  (let ((r1 (dim0 m1)) (c1 (dim1 m1))
	(r2 (dim0 m2)) (c2 (dim1 m2))
	(r3 (dim0 m1)) (c3 (dim1 m1))
	(r4 (dim0 m2)) (c4 (dim1 m2))
	(vals (make-list 5)))
    (setf (elt vals 0) (+ (* r3 r4 c4) (* r2 r3 c4) (* r1 r2 c4)))
    (setf (elt vals 1) (+ (* r2 r3 c3) (* r2 r4 c4) (* r1 r2 c4)))
    (setf (elt vals 2) (+ (* r2 r3 c3) (* r1 r2 c3) (* r1 r4 c4)))
    (setf (elt vals 3) (+ (* r1 r2 c2) (* r1 r3 c3) (* r1 r4 c4)))
    (setf (elt vals 4) (+ (* r1 r2 c2) (* r3 r4 c4) (* r1 r3 c4)))
    (ecase (position (apply #'min vals) vals :test #'eq)
      (0 (gemm m1 (gemm m2 (gemm m3 m4))))
      (1 (gemm m1 (gemm (gemm m2 m3) m4)))
      (2 (gemm (gemm m1 (gemm m2 m3)) m4))
      (3 (gemm (gemm (gemm m1 m2) m3) m4))
      (4 (gemm (gemm m1 m2) (gemm m3 m4))))))
