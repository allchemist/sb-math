(in-package :sb-math2)

;; wrappers for common functions

(define-blas-wrapper swap :matrix-args (X Y)
  :lets ((size (the fixnum (array-total-size X))))
  :pre ((assert (= size (the fixnum (array-total-size Y))) nil "Improper dimensions for swap"))
  :alien-args (size X 1 Y 1))

(define-blas-wrapper copy :matrix-args (X Y)
  :lets ((size (the fixnum (array-total-size X))))
  :alien-args (size X 1 Y 1))

(define-blas-wrapper axpy :matrix-args (X Y) :float-args alpha
  :lets ((size (the fixnum (array-total-size X))))
  :pre ((assert (<= size (the fixnum (array-total-size Y))) nil "Improper dimensions for axpy"))
  :alien-args (size alpha X 1 Y 1))  :return Y)

(define-blas-wrapper scal :matrix-args X :float-args alpha
  :alien-args ((array-total-size X) alpha X 1))

(define-blas-wrapper gemv :matrix-args (A X dest) :float-args (alpha beta) :rest-args transA
  :alien-args ('CBlasRowMajor transA (dim0 A) (dim1 A) alpha A (dim1 A) X 1 beta dest 1))

(define-blas-wrapper gemm :matrix-args (A B dest) :float-args (alpha beta) :rest-args (transA transB)
   :lets ((M 0) (N 0) (K 0) (K1 0) (LDA 0) (LDB 0) (LDC 0))
   :pre
   ((declare (type fixnum M N K K1 LDA LDB LDC))
    (cond ((and (eq transA :notrans) (eq transB :notrans))
	   (setf M (dim0 A)
		 N (dim1 B)
		 K (dim1 A)
		 K1 (dim0 B)
		 LDA K
		 LDB N
		 LDC N))
	  ((and (not (eq transA :notrans)) (eq transB :notrans))
	   (setf M (dim1 A)
		 N (dim1 B)
		 K (dim0 A)
		 K1 (dim0 B)
		 LDA M
		 LDB N
		 LDC N))
	  ((and (eq transA :notrans) (not (eq transB :notrans)))
	   (setf M (dim0 A)
		 N (dim0 B)
		 K (dim1 A)
		 K1 (dim1 B)
		 LDA K
		 LDB K
		 LDC N))
	  ((and (not (eq transA :notrans)) (not (eq transB :notrans)))
	   (setf M (dim1 A)
		 N (dim0 B)
		 K (dim0 A)
		 K1 (dim1 B)
		 LDA M
		 LDB K
		 LDC N)))
    (assert (= K K1) nil "Improper dimensions for gemm"))
   :alien-args
   ('CblasRowMajor transA transB M N K alpha A LDA B LDB beta dest LDC))


;; individual declarations of non-standard functions

(declaim (inline sdsdot dsdot sdot ddot cdotu cdotc zdotu zdotc
		 sger dger cger zger))

;; dot

(defun sdsdot (X Y)
  (declare (type (simple-array single-float) X Y)
	   (optimize speed (safety 0) (space 0)))
  (the single-float
    (%sdsdot (the fixnum (min (the fixnum (array-total-size X))
			      (the fixnum (array-total-size Y))))
	     0d0 (array-sap X) 1 (array-sap Y) 1)))

(defun dsdot (X Y)
  (declare (type (simple-array single-float) X Y)
	   (optimize speed (safety 0) (space 0)))
  (the double-float
    (%dsdot (the fixnum (min (the fixnum (array-total-size X))
			     (the fixnum (array-total-size Y))))
	    (array-sap X) 1 (array-sap Y) 1)))

(defun sdot (X Y)
  (declare (type (simple-array single-float) X Y)
	   (optimize speed (safety 0) (space 0)))
  (the single-float
    (%sdot (the fixnum (min (the fixnum (array-total-size X))
			    (the fixnum (array-total-size Y))))
	   (array-sap X) 1 (array-sap Y) 1)))

(defun ddot (X Y)
  (declare (type (simple-array double-float) X Y)
	   (optimize speed (safety 0) (space 0)))
  (the double-float 
    (%ddot (the fixnum (min (the fixnum (array-total-size X))
			    (the fixnum (array-total-size Y))))
	   (array-sap X) 1 (array-sap Y) 1)))

(defun cdotu (X Y)
  (declare (type (simple-array (complex single-float)) X Y)
	   (optimize speed (safety 0) (space 0)))
  (let ((dotu (the (complex single-float) #C(1.0 0.0))))
    (%cdotu_sub (the fixnum (min (the fixnum (array-total-size X))
				 (the fixnum (array-total-size Y))))
		(array-sap X) 1 (array-sap Y) 1 (complex-sap dotu))
    (the (complex single-float) dotu)))

(defun cdotc (X Y)
  (declare (type (simple-array (complex single-float)) X Y)
	   (optimize speed (safety 0) (space 0)))
  (let ((dotc (the (complex single-float) #C(1.0 0.0))))
    (%cdotc_sub (the fixnum (min (the fixnum (array-total-size X))
				 (the fixnum (array-total-size Y))))
		(array-sap X) 1 (array-sap Y) 1 (complex-sap dotc))
    (the (complex single-float) dotc)))

(defun cdotu (X Y)
  (declare (type (simple-array (complex double-float)) X Y)
	   (optimize speed (safety 0) (space 0)))
  (let ((dotu (the (complex double-float) #C(1.0 0.0))))
    (%zdotu_sub (the fixnum (min (the fixnum (array-total-size X))
				 (the fixnum (array-total-size Y))))
		(array-sap X) 1 (array-sap Y) 1 (complex-sap dotu))
    (the (complex double-float) dotu)))

(defun cdotc (X Y)
  (declare (type (simple-array (complex double-float)) X Y)
	   (optimize speed (safety 0) (space 0)))
  (let ((dotc (the (complex double-float) #C(1.0 0.0))))
    (%zdotc_sub (the fixnum (min (the fixnum (array-total-size X))
				 (the fixnum (array-total-size Y))))
		(array-sap X) 1 (array-sap Y) 1 (complex-sap dotc))
    (the (complex double-float) dotc)))

;; ger

(defun sger (A X Y alpha conj)
  (declare (ignore conj)
	   (type (simple-array single-float) A X Y)
	   (optimize speed (safety 0) (space 0))
	   (declare (type single-float alpha)))
  (let ((m (dim0 X)) (n (dim0 Y)))
    (declare (type fixnum m n))
    (%sger 'CBlasRowMajor m n alpha (array-sap X) 1 (array-sap Y) 1 (array-sap A) n))
  (the (simple-array single-float) A))

(defun dger (A X Y alpha conj)
  (declare (ignore conj)
	   (type (simple-array double-float) A X Y)
	   (optimize speed (safety 0) (space 0))
	   (type double-float alpha))
  (let ((m (dim0 X)) (n (dim0 Y)))
    (declare (type fixnum m n))
    (%dger 'CBlasRowMajor m n alpha (array-sap X) 1 (array-sap Y) 1 (array-sap A) n))
  (the (simple-array double-float) A))

(defun cger (A X Y alpha conj)
  (declare (ignore conj)
	   (type (simple-array (complex single-float)) A X Y)
	   (optimize speed (safety 0) (space 0))
	   (type single-float alpha))
  (let ((m (dim0 X)) (n (dim0 Y)))
    (declare (type fixnum m n))
    (funcall (ecase conj (:noconj '%cgeru) (:conj '%cgerc))
	     'CBlasRowMajor m n (complex-sap alpha) (array-sap X) 1 (array-sap Y) 1 (array-sap A) n))
  (the (simple-array (complex single-float)) A))

(defun zger (A X Y alpha conj)
  (declare (ignore conj)
	   (type (simple-array (complex double-float)) A X Y)
	   (optimize speed (safety 0) (space 0))
	   (type double-float alpha))
  (let ((m (dim0 X)) (n (dim0 Y)))
    (declare (type fixnum m n))
    (funcall (ecase conj (:noconj '%zgeru) (:conj '%zgerc))
	     'CBlasRowMajor m n (complex-sap alpha) (array-sap X) 1 (array-sap Y) 1 (array-sap A) n))
  (the (simple-array (complex double-float)) A))
