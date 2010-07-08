(in-package :sb-math)

;;; ==============================================================
;;; BLAS 1

(defun inner-prod (X Y &key (conjY :noconj))
  (sb-sys:with-pinned-objects (X Y)
    (assert (= (array-total-size X) (array-total-size Y)) nil "Improper dimensions for inner-prod")
    (funcall (with-float-type-choice (array-element-type X)
	       #'sdot #'ddot
	       (ecase conjY (:conj #'cdotc) (:noconj #'cdotu)) 
	       (ecase conjY (:conj #'zdotc) (:noconj #'zdotu)))
	     X Y)))

(defun e-norm (X)
  (sb-sys:with-pinned-objects (X)
    (funcall (with-float-type-choice (array-element-type X)
	       #'%snrm2 #'%dnrm2 #'%scnrm2 #'%dznrm2)
	     (array-total-size X) (array-sap X) 1)))

(defun normalize (X)
  (m*c X (/ (e-norm X))))

(defun amsum (X)
  (sb-sys:with-pinned-objects (X)
    (funcall (with-float-type-choice (array-element-type X)
	       #'%sasum #'%dasum #'%scasum #'%dzasum)
	     (array-total-size X) (array-sap X) 1)))

(defun iamax (X)
  (sb-sys:with-pinned-objects (X)
    (funcall (with-float-type-choice (array-element-type X)
	       #'%isamax #'%idamax #'%icamax #'%izamax)
	     (array-total-size X) (array-sap X) 1)))

(defun ammax (X)
  (row-major-aref X (iamax X)))

(defun swap (X Y)
  (sb-sys:with-pinned-objects (X Y)
    (assert (= (array-total-size X) (array-total-size Y)) nil "Improper dimensions for swap")
    (funcall (with-function-choice 'swap (array-element-type X))
	     (array-total-size X) (array-sap X) 1 (array-sap Y) 1) Y))

(defun copy (X &optional Y)
  (if Y
      (assert (<= (array-total-size X) (array-total-size Y)) nil "Improper dimensions for copy")
      (setf Y (make-array (array-dimensions X) :element-type (array-element-type X))))
  (sb-sys:with-pinned-objects (X Y)
    (funcall (with-function-choice 'copy (array-element-type X))
	     (array-total-size X) (array-sap X) 1 (array-sap Y) 1) Y))

(defun copy-with-offset (X Y offset)
  (assert (<= (array-total-size X) (- (array-total-size Y) offset)) nil
	  "Improper dimensions for copy-with-offset")
  (sb-sys:with-pinned-objects (X Y)
    (let ((type (array-element-type X)))
      (funcall
       (with-function-choice 'copy type)
       (array-total-size X) (array-sap X) 1
       (sb-sys:sap+ (array-sap Y)
		    (* offset
		       (float-sizeof type)))
       1)
      Y)))

(defun axpy (X Y alpha)
  (sb-sys:with-pinned-objects (X Y alpha)
    (assert (<= (array-total-size X) (array-total-size Y)) nil "Improper dimensions for axpy")
    (let ((type (array-element-type X)))
      (funcall (with-function-choice 'axpy type)
	       (array-total-size X) (maybe-complex (coerce alpha type)) (array-sap X) 1 (array-sap Y) 1))
    Y))

(defun m+ (m1 m2) (axpy m2 m1 1))
(defun m- (m1 m2) (axpy m2 m1 -1))

(defun m*c (X alpha)
  (sb-sys:with-pinned-objects (X)
    (let ((type (array-element-type X)))
      (funcall (with-function-choice 'scal type)
	       (array-total-size X) (maybe-complex (coerce alpha type)) (array-sap X) 1))
    X))

;;; ==============================================================
;;; BLAS 2

(defun gemv (A X &key dest (alpha 1) (beta 0) (transA :notrans))
  (let ((dim0 nil) (dim1 nil))
    (if (eq transA :notrans)
	(setf dim0 (dim0 A)
	      dim1 (dim1 A))
	(setf dim0 (dim1 A)
	      dim1 (dim0 A)))
    (assert (= dim1 (dim0 X)) nil "Improper dimensions for gemv")
    (let ((type (array-element-type A)))
      (if dest
	  (assert (<= dim0 (dim0 dest)) nil "Improper dimensions for gemv")
	  (setf dest (make-matrix dim0 :element-type type)))
      (sb-sys:with-pinned-objects (A X dest alpha beta)
	(funcall
	 (with-function-choice 'gemv type)
	 'CblasRowMajor transA (dim0 A) (dim1 A) (maybe-complex (coerce alpha type))
	 (array-sap A) (dim1 A) (array-sap X) 1 (maybe-complex (coerce beta type))
	 (array-sap dest) 1)
	dest))))

(defun ger (X Y &key dest (alpha 1) (conj :noconj))
  (let ((type (array-element-type X)))
    (if dest
	(assert (and (= (dim0 X) (dim0 dest))
		     (= (dim0 Y) (dim1 dest)))
		nil "Improper dimensions for ger")
	(setf dest (make-matrix (list (dim0 X) (dim0 Y)) :element-type type)))
    (sb-sys:with-pinned-objects (dest X Y alpha)
      (funcall (with-float-type-choice  type #'sger #'dger #'cger #'zger)
	       dest X Y (coerce alpha type) conj)
      dest)))

;; X contents is substituted!
(defun trmv (A X &key (uplo :upper) (transA :notrans))
  (let ((dim (dim0 A)))
    (assert (= dim (dim1 A) (dim0 X)) nil "Improper dimesions for trmv")
    (sb-sys:with-pinned-objects (A X)
      (funcall (with-function-choice 'trmv (array-element-type A))
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
	(funcall
	 (with-function-choice 'symv type)
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
	(funcall
	 (with-function-choice 'hemv type)
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
	(funcall (with-function-choice 'syr type)
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
	(funcall (with-function-choice 'her type)
		 'CBlasRowMajor uplo dim (complex-sap (coerce alpha type))
		 (array-sap X) 1 (array-sap dest) dim)
	dest))))

;;; ==============================================================
;;; BLAS 3

(defun gemm (A B &key dest (alpha 1) (beta 0) (transA :notrans) (transB :notrans))
  (let (M N K K1 LDA LDB LDC)
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
    (assert (= K K1) nil "Improper dimensions for gemm")
    (let ((type (array-element-type A)))
      (if dest
	  (assert (and (= M (dim0 dest))
		       (= N (dim1 dest)))
		  nil "Improper dimensions for gemm")
	  (setf dest (make-matrix (list M N) :element-type type)))
      (sb-sys:with-pinned-objects (A B dest alpha beta)
	(funcall
	 (with-function-choice 'gemm type)
	 'CblasRowMajor transA transB
	 M N K (maybe-complex (coerce alpha type)) (array-sap A) LDA
	 (array-sap B) LDB (maybe-complex (coerce beta type))
	 (array-sap dest) LDC)
	dest))))

;;; ==============================================================
;;; BLAS 2 packed

;; X contents is substituted!
(defun tpmv (Ap X &key (uplo :upper) (transA :notrans))
  (let ((dim (ldm Ap)))
    (assert (= dim (dim0 X)) nil "Improper dimesions for tpmv")
    (sb-sys:with-pinned-objects (Ap X)
      (funcall (with-function-choice 'tpmv (pmatrix-element-type Ap))
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
	(funcall
	 (with-function-choice 'spmv type)
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
	(funcall
	 (with-function-choice 'hpmv type)
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
	(funcall (with-function-choice 'spr type)
		 'CBlasRowMajor uplo dim (coerce alpha type)
		 (array-sap X) 1 (pmatrix-sap dest) dim)
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
	(funcall (with-function-choice 'hpr type)
		 'CBlasRowMajor uplo dim (complex-sap (coerce alpha type))
		 (array-sap X) 1 (pmatrix-sap dest) dim)
	dest))))
