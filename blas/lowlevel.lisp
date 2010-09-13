(in-package :sb-math)

;; non-standard functions

(declaim (inline sdsdot dsdot sdot ddot cdotu cdotc zdotu zdotc
		 sger dger cger zger))

;; dot

(defun sdsdot (X Y alpha)
  (%sdsdot (min (array-total-size X) (array-total-size Y))
	   alpha (array-sap X) 1 (array-sap Y) 1))

(defun dsdot (X Y)
  (%dsdot (min (array-total-size X) (array-total-size Y))
	  (array-sap X) 1 (array-sap Y) 1))

(defun sdot (X Y)
  (%sdot (min (array-total-size X) (array-total-size Y))
	 (array-sap X) 1 (array-sap Y) 1))

(defun ddot (X Y)
  (%ddot (min (array-total-size X) (array-total-size Y))
	 (array-sap X) 1 (array-sap Y) 1))

(defun cdotu (X Y)
  (let ((dotu #C(1.0 0.0)))
    (%cdotu_sub (min (array-total-size X) (array-total-size Y))
		(array-sap X) 1 (array-sap Y) 1 (complex-sap dotu))
    dotu))

(defun cdotc (X Y)
  (let ((dotc #C(1.0 0.0)))
    (%cdotc_sub (min (array-total-size X) (array-total-size Y))
		(array-sap X) 1 (array-sap Y) 1 (complex-sap dotc))
    dotc))

(defun zdotu (X Y)
  (let ((dotu #C(1d0 0d0)))
    (%zdotu_sub (min (array-total-size X) (array-total-size Y))
		(array-sap X) 1 (array-sap Y) 1 (complex-sap dotu))
    dotu))

(defun zdotc (X Y)
  (let ((dotc #C(1d0 0d0)))
    (%zdotc_sub (min (array-total-size X) (array-total-size Y))
		(array-sap X) 1 (array-sap Y) 1 (complex-sap dotc))
    dotc))

;; ger

(defun sger (A X Y alpha conj)
  (declare (ignore conj))
  (let ((m (dim0 X)) (n (dim0 Y)))
    (%sger 'CBlasRowMajor m n alpha (array-sap X) 1 (array-sap Y) 1 (array-sap A) n)))

(defun dger (A X Y alpha conj)
  (declare (ignore conj))
  (let ((m (dim0 X)) (n (dim0 Y)))
    (%dger 'CBlasRowMajor m n alpha (array-sap X) 1 (array-sap Y) 1 (array-sap A) n)))

(defun cger (A X Y alpha conj)
  (let ((m (dim0 X)) (n (dim0 Y)))
    (funcall (ecase conj (:noconj #'%cgeru) (:conj #'%cgerc))
	     'CBlasRowMajor m n (complex-sap alpha) (array-sap X) 1 (array-sap Y) 1 (array-sap A) n)))

(defun zger (A X Y alpha conj)
  (let ((m (dim0 X)) (n (dim0 Y)))
    (funcall (ecase conj (:noconj #'%zgeru) (:conj #'%zgerc))
	     'CBlasRowMajor m n (complex-sap alpha) (array-sap X) 1 (array-sap Y) 1 (array-sap A) n)))

;; wrappers for common functions

(define-blas-wrapper swap :arrays (X Y)
  :lets ((size (the fixnum (array-total-size X))))
  :pre ((assert (= size (the fixnum (array-total-size Y))) nil "Improper dimensions for swap"))
  :alien-args (size X 1 Y 1))

(define-blas-wrapper copy :arrays (X Y)
  :lets ((size (the fixnum (array-total-size X))))
  :alien-args (size X 1 Y 1))

(define-blas-wrapper axpy :arrays (X Y) :floats (alpha)
  :lets ((size (the fixnum (array-total-size X))))
  :pre ((assert (<= size (the fixnum (array-total-size Y))) nil "Improper dimensions for axpy"))
  :alien-args (size alpha X 1 Y 1))

(define-blas-wrapper scal :arrays (X) :floats (alpha)
  :alien-args ((array-total-size X) alpha X 1))

(define-blas-wrapper gemv :arrays (A X dest) :floats (alpha beta) :rest (transA)
  :alien-args ('CBlasRowMajor transA (dim0 A) (dim1 A) alpha A (dim1 A) X 1 beta dest 1))

(define-blas-wrapper gemm :arrays (A B dest) :floats (alpha beta) :rest (transA transB)
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
