(in-package :sb-math)

(export
 '(swap copy copy-with-offset axpy m+ m- m*c
   inner-prod e-norm normalize
   gemv ger trmv symv hemv syr her gemm))

(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

;;; ==============================================================
;;; BLAS 1

;; standard names, with wrappers

(defun swap (X Y)
  (declare (type array X Y)
	   (optimize speed (space 0))
	   (inline array-element-type array-dimensions))
  (float-choice-funcall (array-element-type X) swap nil X Y))

 (defun copy (X &optional Y)
   (declare (type array X)
	    (optimize speed (space 0))
	    (inline array-element-type array-dimensions))
  (let ((type (array-element-type X)))
    (if Y
	(assert (<= (the fixnum (array-total-size X))
		    (the fixnum (array-total-size Y)))
		nil "Improper dimensions for copy")
	(setf Y (make-matrix (array-dimensions X) :element-type type)))
    (float-choice-funcall type copy nil X Y)))

(defun copy-with-offset (X Y offset)
  (declare ;(sb-ext:muffle-conditions sb-ext:code-deletion-note)
  ;; ignore notes about deleting unreachable code
  ;; its OK, and also very good
	   (type simple-array X Y)
	   (type fixnum offset)
	   (optimize speed))
  (assert (<= (the fixnum (array-total-size X))
	      (the fixnum (- (the fixnum (array-total-size Y)) offset)))
	  nil "Improper dimensions for copy-with-offset")
  (sb-sys:with-pinned-objects (X Y)
    (let ((type (array-element-type X)))
       (float-choice-funcall type copy %
			     (the fixnum (array-total-size X)) (array-sap X) 1
			     (the system-area-pointer
			       (sb-sys:sap+ (array-sap Y)
					    (the integer
					      (* offset
						 (float-sizeof type)))))
			     1)
      Y)))

(defun axpy (X Y alpha)
  (declare (type simple-array X Y)
	   (optimize speed))
  (float-choice-funcall (array-element-type X) axpy nil X Y alpha))

(defun m+ (m1 m2) (axpy m2 m1 1.0))
(defun m- (m1 m2) (axpy m2 m1 -1.0))

(defun m*c (X alpha)
  (declare (optimize speed)
	   (type simple-array X))
  (let ((type (array-element-type X)))
    (float-choice-funcall type scal nil X alpha)))

;; non-standard names

(defun inner-prod (X Y &key (conjY :noconj))
  (declare (type simple-array X Y)
	   (optimize speed))
  (sb-sys:with-pinned-objects (X Y)
    (float-type-choice
     (array-element-type X) (sdot X Y) (ddot X Y)
     (ecase conjY (:conj (cdotc X Y)) (:noconj (cdotu X Y)))
     (ecase conjY (:conj (zdotc X Y)) (:noconj (zdotu X Y))))))

(defun e-norm (X)
  (declare (optimize speed)
	   (type simple-array X))
  (sb-sys:with-pinned-objects (X)
    (float-type-choice
     (array-element-type X)
     (the single-float (%snrm2 (the fixnum (array-total-size X)) (array-sap X) 1))
     (the double-float (%dnrm2 (the fixnum (array-total-size X)) (array-sap X) 1))
     (the single-float (%scnrm2 (the fixnum (array-total-size X)) (array-sap X) 1))
     (the double-float (%dznrm2 (the fixnum (array-total-size X)) (array-sap X) 1)))))

(defun normalize (X)
  (m*c X (/ (e-norm X))))
  
;;; ==============================================================
;;; BLAS 2

(defun gemv (A X &key dest (alpha 1.0) (beta 0.0) (transA :notrans))
  (declare (optimize speed)
	   (type simple-array A X))
  (let ((type (array-element-type A))
	(dim0 0) (dim1 0))
    (declare (type fixnum dim0 dim1))
    (if (eq transA :notrans)
	(setf dim0 (dim0 A)
	      dim1 (dim1 A))
	(setf dim0 (dim1 A)
	      dim1 (dim0 A)))
    (assert (<= dim1 (dim0 X)) nil "Improper dimensions for gemv")
    (if dest
	(assert (<= dim0 (dim0 dest)) nil "Improper dimensions for gemv")
	(setf dest (the simple-array (make-matrix dim0 :element-type type))))
    (float-choice-funcall type gemv nil
      A X (the simple-array dest) alpha beta transA)))

;; KLUDGE
;; dest-size driven gemv
(export 'gemvd)
(defun gemvd (A X &key dest (alpha 1.0) (beta 0.0) (transA :notrans))
  (declare (optimize speed)
	   (type simple-array A X))
  (let ((type (array-element-type A))
	(dim0 0) (dim1 0))
    (declare (type fixnum dim0 dim1))
    (if (eq transA :notrans)
	(setf dim0 (dim0 A)
	      dim1 (dim1 A))
	(setf dim0 (dim1 A)
	      dim1 (dim0 A)))
    (assert (<= dim1 (dim0 X)) nil "Improper dimensions for gemv")
    (if dest
	(assert (>= dim0 (dim0 dest)) nil "Improper dimensions for gemv")
	(setf dest (the simple-array (make-matrix dim0 :element-type type))))
    (float-choice-funcall type gemvd nil
      A X (the simple-array dest) alpha beta transA)))

(defun ger (X Y &key dest (alpha 1.0) (conj :noconj))
  (declare (type simple-array X Y)
	   (optimize speed))
  (let ((type (array-element-type X)))
    (if dest
	(assert (and (= (dim0 X) (dim0 dest))
		     (= (dim0 Y) (dim1 dest)))
		nil "Improper dimensions for ger")
	(setf dest (the simple-array (make-matrix (list (dim0 X) (dim0 Y)) :element-type type))))
    (sb-sys:with-pinned-objects (dest X Y alpha)
      (float-choice-funcall type ger nil
        (the simple-array dest) X Y alpha conj))))

;; not optimized, but waiting for their hour

;; X contents is substituted!
(defun trmv (A X &key (uplo :upper) (transA :notrans))
  (let ((dim (dim0 A)))
    (assert (= dim (dim1 A) (dim0 X)) nil "Improper dimesions for trmv")
    (sb-sys:with-pinned-objects (A X)
      (float-choice-funcall (array-element-type A) trmv %
	       'CBlasRowMajor uplo transA :nonunit dim (array-sap A) dim (array-sap X) 1)
      X)))

(defun symv (A X &key dest (alpha 1) (beta 0) (uplo :upper))
  (let ((dim (dim0 A)))
    (assert (= dim (dim1 A) (dim0 X)) nil "Improper dimensions for symv")
    (let ((type (array-element-type A)))
      (assert (not (subtypep type 'complex)) nil "Symmetric matrix should be real")
      (if dest
	  (assert (<= dim (dim0 dest)) nil "Improper dimensions for symv")
	  (setf dest (make-matrix dim :element-type type)))
      (sb-sys:with-pinned-objects (A X dest)
	(float-choice-funcall type symv %
	 'CBlasRowMajor uplo dim (coerce alpha type) (array-sap A) dim
	 (array-sap X) 1 (coerce beta type) (array-sap dest) 1)
	dest))))

(defun hemv (A X &key dest (alpha #C(1.0 0.0)) (beta #C(0.0 0.0)) (uplo :upper))
  (let ((dim (dim0 A)))
    (assert (= dim (dim1 A) (dim0 X)) nil "Improper dimensions for hemv")
    (let ((type (array-element-type A)))
      (assert (subtypep type 'complex) nil "Hermitian matrix should be complex")
      (if dest
	  (assert (<= dim (dim0 dest)) nil "Improper dimensions for hemv")
	  (setf dest (make-matrix dim :element-type type)))
      (sb-sys:with-pinned-objects (A X dest alpha beta)
	(float-choice-funcall type hemv %
	 'CBlasRowMajor uplo dim (complex-sap (coerce alpha type)) (array-sap A) dim
	 (array-sap X) 1 (complex-sap (coerce beta type)) (array-sap dest) 1)
	dest))))

(defun syr (X &key dest (alpha 1) (uplo :upper))
  (let ((dim (dim0 X)))
    (let ((type (array-element-type X)))
      (if dest
	  (assert (= (dim0 dest) (dim1 dest) dim) nil "Improper dimensions for syr")
	  (setf dest (make-matrix (list dim dim) :element-type type)))
      (assert (not (subtypep type 'complex)) nil "Symetric matrix should be real")
      (sb-sys:with-pinned-objects (dest X)
	(float-choice-funcall type syr %
		 'CBlasRowMajor uplo dim (coerce alpha type)
		 (array-sap X) 1 (array-sap dest) dim)
	dest))))

(defun her (X &key dest (alpha #C(1.0 0.0)) (uplo :upper))
  (let ((dim (dim0 X)))
    (let ((type (array-element-type X)))
      (if dest
	  (assert (= (dim0 dest) (dim1 dest) dim) nil "Improper dimensions for her")
	  (setf dest (make-matrix (list dim dim) :element-type type)))
      (assert (subtypep type 'complex) nil "Hermitian matrix should be complex")
      (sb-sys:with-pinned-objects (dest X alpha)
	(float-choice-funcall type her %
		 'CBlasRowMajor uplo dim (complex-sap (coerce alpha type))
		 (array-sap X) 1 (array-sap dest) dim)
	dest))))

;;; ==============================================================
;;; BLAS 3

(defun gemm (A B &key dest (alpha 1.0) (beta 0.0) (transa :notrans) (transB :notrans))
  (declare (optimize speed)
	   (type simple-array A B))
  (let ((type (array-element-type A))
	(M (the fixnum (if (eq transA :notrans) (dim0 A) (dim1 A))))
	(N (the fixnum (if (eq transB :notrans) (dim1 B) (dim0 B)))))
    (if dest
	(assert (and (= M (dim0 dest))
		     (= N (dim1 dest)))
		nil "Improper dimensions for gemm")
	(setf dest (the simple-array (make-matrix `(,M ,N) :element-type type))))
    (float-choice-funcall type gemm nil
      A B (the simple-array dest) alpha beta transa transb)))

(defun trmm (A B &key (alpha 1.0) (side :left) (uplo :upper) (transA :notrans))
  (declare (optimize speed)
	   (type simple-array A B))
  (assert
   (if (eq side :left)
       (= (dim0 A) (dim1 A) (dim0 B))
       (= (dim0 A) (dim1 A) (dim1 B)))
   nil "Improper dimensions for trmm")
  (float-choice-funcall (array-element-type A) trmm nil
    A B alpha side uplo transA))

#|
;;; packed functions -  to be deleted

;;; ==============================================================
;;; BLAS 2 packed

;; X contents is substituted!
(defun tpmv (Ap X &key (uplo :upper) (transA :notrans))
  (let ((dim (ldm Ap)))
    (assert (= dim (dim0 X)) nil "Improper dimesions for tpmv")
    (sb-sys:with-pinned-objects (Ap X)
      (float-choice-funcall (pmatrix-element-type Ap) tpmv %
	       'CBlasRowMajor uplo transA :nonunit dim (pmatrix-sap Ap) (array-sap X) 1)
      X)))

(defun spmv (Ap X &key dest (alpha 1) (beta 0) (uplo :upper))
  (let ((dim (ldm Ap)))
    (assert (= dim (dim0 X)) nil "Improper dimensions for spmv")
    (let ((type (pmatrix-element-type Ap)))
      (assert (not (subtypep type 'complex)) nil "Symmetric matrix should be real")
      (if dest
	  (assert (<= dim (dim0 dest)) nil "Improper dimensions for spmv")
	  (setf dest (make-matrix dim :element-type type)))
      (sb-sys:with-pinned-objects (Ap X dest)
	(float-choice-funcall type spmv %
	 'CBlasRowMajor uplo dim (coerce alpha type) (pmatrix-sap Ap)
	 (array-sap X) 1 (coerce beta type) (array-sap dest) 1)
	dest))))

(defun hpmv (Ap X &key dest (alpha #C(1.0 0.0)) (beta #C(0.0 0.0)) (uplo :upper))
  (let ((dim (ldm Ap)))
    (assert (= dim (dim0 X)) nil "Improper dimensions for hpmv")
    (let ((type (pmatrix-element-type Ap)))
      (assert (subtypep type 'complex) nil "Hermitian matrix should be complex")
      (if dest
	  (assert (<= dim (dim0 dest)) nil "Improper dimensions for hpmv")
	  (setf dest (make-matrix dim :element-type type)))
      (sb-sys:with-pinned-objects (Ap X dest alpha beta)
	(float-choice-funcall type hpmv %
	 'CBlasRowMajor uplo dim (complex-sap (coerce alpha type)) (pmatrix-sap Ap)
	 (array-sap X) 1 (complex-sap (coerce beta type)) (array-sap dest) 1)
	dest))))

(defun spr (X &key dest (alpha 1) (uplo :upper))
  (let ((dim (dim0 X)))
    (let ((type (array-element-type X)))
      (if dest
	  (assert (= (ldm dest) dim) nil "Improper dimensions for spr")
	  (setf dest (make-pmatrix (list dim dim)
				   :element-type type
				   :pack-type :symmetric
				   :upper? (eq uplo :upper))))
      (assert (not (subtypep type 'complex)) nil "Symetric matrix should be real")
      (sb-sys:with-pinned-objects (dest X)
	(float-choice-funcall type spr %
		 'CBlasRowMajor uplo dim (coerce alpha type)
		 (array-sap X) 1 (pmatrix-sap dest))
	dest))))

(defun hpr (X &key dest (alpha #C(1.0 0.0)) (uplo :upper))
  (let ((dim (dim0 X)))
    (let ((type (array-element-type X)))
      (if dest
	  (assert (= (ldm dest) dim) nil "Improper dimensions for hpr")
	  (setf dest (make-pmatrix (list dim dim)
				   :element-type type
				   :pack-type :hermitian
				   :upper? (eq uplo :upper))))
      (assert (subtypep type 'complex) nil "Hermitian matrix should be complex")
      (sb-sys:with-pinned-objects (dest X alpha)
	(float-choice-funcall type hpr %
		 'CBlasRowMajor uplo dim (complex-sap (coerce alpha type))
		 (array-sap X) 1 (pmatrix-sap dest))
	dest))))

|#

(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
