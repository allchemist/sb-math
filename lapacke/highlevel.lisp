(in-package :sb-math)

(export '(transpose lu lu-inverse lu-solve svd))

(defun transpose (matrix &optional dest)
  (let ((element-type (array-element-type matrix))
	(dim0 (dim0 matrix))
	(dim1 (dim1 matrix))
	(info -1))
    (if dest
	(assert (and (= dim0 (dim1 dest))
		     (= dim1 (dim0 dest)))
		nil "Improper destination matrix dimensions")
	(setf dest (make-matrix (list dim1 dim0) :element-type element-type)))
    (sb-sys:with-pinned-objects (matrix dest)
      (setf info (float-choice-funcall element-type ge_trans %
		   101 dim0 dim1 (array-sap matrix) dim1 (array-sap dest) dim0))
      ;(assert (zerop info) nil "Transposition returned non-zero code: ~A" info)
      )
    dest))

(defun lu (A &key (pure t))
  (let ((dim (dim0 A))
	(element-type (array-element-type A))
	(info -1)
	(%A (if pure (copy A) A)))
    (assert (= dim (dim1 %A)) nil "Matrix should be sqhare")
    (let ((ipiv (make-array dim :element-type '(unsigned-byte 32))))
      (sb-sys:with-pinned-objects (%A ipiv)
	(setf info (float-choice-funcall element-type getrf %
		     101 dim dim (array-sap %A) dim (array-sap ipiv))))
      (assert (zerop info) nil "LU decomposition returned non-zero code: ~A" info)
      (values %A ipiv))))

(defun lu-inverse (A &key ipiv (pure t))
  (let ((dim (dim0 A))
	(element-type (array-element-type A))
	(info -1)
	(%A (if pure (copy A) A)))
    (assert (= dim (dim1 %A)) nil "Matrix should be sqhare")
    (if ipiv
	(sb-sys:with-pinned-objects (%A ipiv)
	  (setf info (float-choice-funcall element-type getri %
		       101 dim (array-sap %A) dim (array-sap ipiv)))
	  (assert (zerop info) nil "LU inversion returned non-zero code: ~A" info))
	(progn
	  (setf ipiv (make-array dim :element-type '(unsigned-byte 32)))
	  (sb-sys:with-pinned-objects (%A ipiv)
	    (setf info (float-choice-funcall element-type getrf %
			 101 dim dim (array-sap %A) dim (array-sap ipiv)))
	    (assert (zerop info) nil "LU decomposition returned non-zero code: ~A" info)
	    (setf info (float-choice-funcall element-type getri %
		         101 dim (array-sap %A) dim (array-sap ipiv)))
	    (assert (zerop info) nil "LU inversion returned non-zero code: ~A" info))))
    %A))

(defun lu-solve (A B &key (trans :notrans) (pure t))
  (let* ((dim (dim0 A))
	 (dim1 (if (vectorp B) 1 (dim1 B)))
	 (element-type (array-element-type A))
	 (info -1)
	 (%A (if pure (copy A) A))
	 (%B (if pure (copy B) B)))
    (assert (= dim (dim1 %A) (dim0 %B)) nil "Improper dimensions for equation")
    (let ((ipiv (make-array dim :element-type '(unsigned-byte 32))))
      (sb-sys:with-pinned-objects (%A %B ipiv)
	(setf info (float-choice-funcall element-type getrf %
		     101 dim dim (array-sap %A) dim (array-sap ipiv)))
	(assert (zerop info) nil "LU decomposition returned non-zero code: ~A" info)
	(setf info (float-choice-funcall element-type getrs %
		     101 (lapack-char-code trans) dim dim1 (array-sap %A) dim
		     (array-sap ipiv) (array-sap %B) dim1))
	(assert (zerop info) nil "Solving equation returned non-zero code: ~A" info)))
    %B))

(defun svd (A &key (left :none) (right :none) (values :vector) (pure t))
  (let* ((dim0 (dim0 A))
	 (dim1 (dim1 A))
	 (min-dim (min dim0 dim1))
	 (type (array-element-type A))
	 (real-type (if (subtypep type 'complex) (second type) type))
	 (info -1)
	 (%A (if pure (copy A) A))
	 (%s (make-matrix min-dim :element-type real-type))
	 (%u (make-matrix (ecase left
			    (:none '(1 1))
			    (:singular `(,min-dim ,dim0))
			    (:all `(,dim0 ,dim0)))
			 :element-type type))
	 (%vt (make-matrix (ecase right
			     (:none '(1 1))
			     (:singular `(,dim1 ,min-dim))
			     (:all `(,dim1 ,dim1)))
			   :element-type type))
	 (%work (make-matrix (- min-dim 1) :element-type type)))
    (sb-sys:with-pinned-objects (%A %s %u %vt)
      (setf info (float-choice-funcall type gesvd %
		   101 (lapack-char-code left) (lapack-char-code right) dim0 dim1 (array-sap %A)
		   dim1 (array-sap %s) (array-sap %u) dim0 (array-sap %vt) dim1 (array-sap %work)))
      (assert (zerop info) nil "SVD decomposition returned non-zero code: ~A" info)
      (let ((diag
	     (ecase values
	       (:vector %s)
	       (:matrix (setf (diag (make-matrix (array-dimensions %A) :element-type type)) %s)))))
	(values diag
		(if (eq left :none) nil %u)
		(if (eq right :none) nil %vt))))))
