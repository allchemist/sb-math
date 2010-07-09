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
    (assert (= dim (dim1 A) (if (vectorp X) (dim0 X) (dim1 X))) nil "MAtrix should be square")
    (multiple-value-bind (LU P)
	(lu real-A)
      (sb-sys:with-pinned-objects (LU X P)
	(funcall (with-function-choice 'getrs element-type t)
		 (lapack-char-code real-trans) LU X P))
      (if (vectorp B) (copy X) (transpose X)))))

(defun svd (A &key (left :none) (right :none) (values :vector))
  (let* ((dim0 (dim1 A)) (dim1 (dim0 A))
	 (element-type (array-element-type A))
	 (complex? (subtypep element-type 'complex))
	 (S (make-matrix (min dim0 dim1)
			 :element-type (if complex? (second element-type) element-type)))
	 (U (make-matrix (list dim0 (ecase left
				      (:none 1)
				      (:singular (min dim0 dim1))
				      (:all dim0)))
			 :element-type element-type))
	 (VT (make-matrix (list (ecase right
				  (:none 1)
				  (:singular (min dim0 dim1))
				  (:all dim1))
				dim1)
			  :element-type element-type))
	 (work (make-matrix (* 2 (max (+ (* 3 (min dim0 dim1)) (max dim0 dim1))
				      (* 5 (min dim0 dim1))))
			    :element-type element-type))
	 (rwork (when complex? (make-matrix (* 10 (min dim0 dim1))
					    :element-type (second element-type))))
	 (copy-A (transpose A)))
    (let ((info
	   (sb-sys:with-pinned-objects (copy-A S U VT work)
	     (apply (with-function-choice 'gesvd element-type t)
		    (lapack-char-code left) (lapack-char-code right)
		    copy-A S U VT work (if complex? (list rwork) '())))))
      (cond ((zerop info)
	     (let ((diag nil))
	       (ecase values
		 (:vector (setf diag S))
		 (:matrix (progn
			    (setf diag (make-matrix (list dim1 dim0)
						    :element-type element-type))
			    (do-diag (diag i)
			      (setf (aref diag i i) (coerce (aref S i) element-type)))))
		 (:packed (progn
			    (setf diag (make-pmatrix (list dim1 dim0)
						     :element-type element-type
						     :pack-type :diagonal))
			    (let ((diag-vec (pmatrix-storage-vector diag)))
			      (do-matrix (diag-vec i)
				(setf (aref diag-vec i) (coerce (aref S i) element-type)))))))
	       (values diag
		       (if (eq left :none) nil (transpose U))
		       (if (eq right :none) nil (transpose VT)))))
	    ((minusp info)
	     (error "Illegal ~A'th parameter for %esvd" (- info)))
	    ((plusp info)
	     (error "~A superdiagonals did not converge" info))))))

(defun eigen (A &key (right :eval) (left :none) (values :vector) (real-values nil))
  (let* ((dim (dim0 A))
	 (type (array-element-type A))
	 (complex? (subtypep type 'complex))
	 (real-type (if complex? (second type) type))
	 (copy-A (transpose A))
	 (w (when complex? (make-matrix dim :element-type type)))
	 (wr (when (not complex?) (make-matrix dim :element-type real-type)))
	 (wi (when (not complex?) (make-matrix dim :element-type real-type)))
	 (vl (make-matrix (list (if (eq left :none) 1 dim) dim) :element-type type))
	 (vr (make-matrix (list (if (eq right :none) 1 dim) dim) :element-type type))
	 (work (make-matrix (* dim 4) :element-type type))
	 (rwork (when complex? (make-matrix (* dim 2) :element-type real-type))))
    (assert (= dim (dim1 A)) nil "Matrix should be square")
    (let ((info
	   (sb-sys:with-pinned-objects (A w wr wi vl vr work rwork)    
	     (apply (with-function-choice 'geev type t)
		    `(,(lapack-char-code left) ,(lapack-char-code right) ,copy-A
		       ,@(if complex? `(,w) `(,wr ,wi))
		       ,vl ,vr ,work ,@(if complex? `(,rwork) '()))))))
      (cond ((zerop info)
	     (values 
	       (let ((vec
		      (if complex?
			  w
			  (if real-values
			      wr
			      (let ((vals (make-array dim :element-type `(complex ,real-type))))
				(do-matrix (vals i)
				  (setf (aref vals i) (complex (aref wr i) (aref wi i))))
				vals)))))
		 (ecase values
		   (:vector vec)
		   (:matrix (diag vec))
		   (:packed (make-instance 'diagonal-matrix
					   :storage-vector vec
					   :ldm dim
					   :ncols dim))))
	       (if (eq right :none) nil (transpose vr))
	       (if (eq left :none) nil (transpose vl))))
	    ((minusp info)
	     (error "Illegal ~A'th parameter for geev" (- info)))
	    ((plusp info)
	     (error "QR failed"))))))
