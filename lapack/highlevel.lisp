(in-package :sb-math)

(defun lu (A)
  (let ((dim (dim0 A))
	(element-type (array-element-type A)))
    (assert (= dim (dim1 A)) nil "Matrix should be sqhare")
    (let ((copy-A (copy A))
	  (ipiv (make-matrix dim :element-type '(unsigned-byte 32))))
      (sb-sys:with-pinned-objects (copy-A ipiv)
	(funcall (with-function-choice 'getrf element-type t) copy-A ipiv))
      (values copy-A ipiv))))

(defun lu-inverse (A)
  (let* ((dim (dim0 A))
	 (element-type (array-element-type A))
	 (complex? (subtypep element-type 'complex)))
    (assert (= dim (dim1 A)) nil "Matrix should be sqhare")
    (multiple-value-bind (LU P)
	(lu A)
      (let ((work (make-matrix (* dim 2)
			       :element-type (if complex?
						 (second element-type)
						 element-type))))
	(sb-sys:with-pinned-objects (LU P work)
	  (funcall (with-function-choice 'getri element-type t) LU P work))
	LU))))

(defun lu-solve (A B &key (trans :notrans))
  (let* ((dim (dim0 A))
	 (X (if (vectorp B) (copy B) (transpose B)))
	 (real-A A)
	 (element-type (array-element-type A))
	 (real-trans (ecase trans
		       (:notrans :trans)
		       (:trans :notrans)
		       (:conjtrans (progn (setf real-A (transpose A))
					  :conjtrans)))))
    (assert (= dim (dim1 A) (dim1 X)) nil "MAtrix should be square")
    (multiple-value-bind (LU P)
	(lu real-A)
      (sb-sys:with-pinned-objects (LU X P)
	(funcall (with-function-choice 'getrs element-type t)
		 (lapack-char-code real-trans) LU X P))
      (if (vectorp B) (copy X) (transpose X)))))
