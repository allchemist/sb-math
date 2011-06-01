(in-package :sb-math)

(defun double-list (elem)
  (list elem elem))

(let ((sm (make-random-matrix '(3 3)
			      :element-type 'single-float))
      (dm (make-random-matrix '(3 3)
			      :element-type 'double-float))
      (cm (make-random-matrix (double-list (round (plain-rng 5 10)))
			      :element-type '(complex single-float)))
     (zm (make-random-matrix (double-list (round (plain-rng 5 10)))
			      :element-type '(complex double-float))))
;; lu-invese
  (define-test "inv-single"
      (gemm (inv sm) sm)
    (setf (diag (make-matrix `(,(dim0 sm) ,(dim0 sm)) :element-type 'single-float))
	  (make-matrix (dim0 sm)
		       :element-type 'single-float
		       :initial-element 1.0))
    :eps *eps-single*)

  (define-test "inv-double"
      (gemm (inv dm) dm)
    (setf (diag (make-matrix `(,(dim0 sm) ,(dim0 sm)) :element-type 'double-float))
	  (make-matrix (dim0 dm)
		       :element-type 'double-float
		       :initial-element 1d0))
    :eps *eps-double*)

  (define-test "inv-complex-single"
      (gemm (inv cm) cm)
    (setf (diag (make-matrix `(,(dim1 sm) ,(dim1 sm)) :element-type '(complex single-float)))
	  (make-matrix (dim0 cm)
		       :element-type '(complex single-float)
		       :initial-element (complex 1.0)))
    :eps *eps-single*)

  (define-test "inv-complex-double"
      (gemm (inv zm) zm)
    (setf (diag (make-matrix `(,(dim0 sm) ,(dim0 sm)) :element-type '(complex double-float)))
	  (make-matrix (dim0 zm)
		       :element-type '(complex double-float)
		       :initial-element (complex 1d0)))
    :eps *eps-double*)

;; lu-solve
  (define-test "lin-solve-single"
      (gemv sm (lin-solve sm (col sm 0))) (col sm 0) :eps *eps-single*)

  (define-test "lin-solve-double"
      (gemv dm (lin-solve dm (col dm 0))) (col dm 0) :eps *eps-double*)

  (define-test "lin-solve-complex-single"
      (gemv cm (lin-solve cm (col cm 0))) (col cm 0) :eps *eps-single*)

  (define-test "lin-solve-complex-double"
      (gemv zm (lin-solve zm (col zm 0))) (col zm 0) :eps *eps-double*)

  (define-test "lin-solve-couple-single"
      (lin-solve sm sm)
    (setf (diag (make-matrix `(,(dim0 sm) ,(dim0 sm)) :element-type 'single-float))
	  (make-matrix (dim0 sm)
		       :element-type 'single-float
		       :initial-element 1.0))
    :eps *eps-single*)

  (define-test "lin-solve-couple-double"
      (lin-solve dm dm)
    (setf (diag (make-matrix `(,(dim0 sm) ,(dim0 sm)) :element-type 'double-float))
	  (make-matrix (dim0 dm)
		       :element-type 'double-float
		       :initial-element 1d0))
    :eps *eps-double*)

  (define-test "lin-solve-couple-complex-single"
      (lin-solve cm cm)
    (setf (diag (make-matrix `(,(dim0 sm) ,(dim0 sm)) :element-type '(complex single-float)))
	  (make-matrix (dim0 cm)
		       :element-type '(complex single-float)
		       :initial-element (complex 1.0)))
    :eps *eps-single*)

  (define-test "lin-solve-couple-complex-double"
      (lin-solve zm zm)
    (setf (diag (make-matrix `(,(dim0 sm) ,(dim0 sm)) :element-type '(complex double-float)))
	  (make-matrix (dim0 zm)
		       :element-type '(complex double-float)
		       :initial-element (complex 1d0)))
    :eps *eps-double*)
#|
;; eigen

  (defun check-cond (matrix)
    (let ((vals (eigen matrix :right :none))
	  (res t))
      (do-matrix (vals i)
	(when (not (zerop (imagpart (aref vals i))))
	  (setf res nil)))
      res))

  (if (check-cond sm)
      (define-test "eigen-single"
	  (multiple-value-bind (vals vecs)
	      (eigen sm :values :matrix :real-values t)
	    (gemm (gemm vecs vals) (lu-inverse vecs)))
	sm
	:eps *eps-single*)
      (warn "Randomly-generated single-float matrix seems to be unconditioned, skipping this test. Try again"))

  (if (check-cond dm)
      (define-test "eigen-double"
	  (multiple-value-bind (vals vecs)
	      (eigen dm :values :matrix :real-values t)
	    (gemm (gemm vecs vals) (lu-inverse vecs)))
	dm
	:eps *eps-double*)
      (warn "Randomly-generated double-float matrix seems to be unconditioned, skipping this test. Try again"))

  (define-test "eigen-complex-single"
      (multiple-value-bind (vals vecs)
	  (eigen cm :values :matrix)
	(gemm (gemm vecs vals) (lu-inverse vecs)))
    cm
    :eps *eps-single*)

  (define-test "eigen-complex-double"
      (multiple-value-bind (vals vecs)
	  (eigen zm :values :matrix)
	(gemm (gemm vecs vals) (lu-inverse vecs)))
    zm
    :eps *eps-double*)
|#
;; svd

  (define-test "svd-prod-single"
      (multiple-value-bind (S U VT)
	  (svd sm :left :all :right :all :values :matrix)
	(gemm (gemm U S) VT))
    sm
    :eps *eps-single*)

  (define-test "svd-prod-double"
      (multiple-value-bind (S U VT)
	  (svd dm :left :all :right :all :values :matrix)
	(gemm (gemm U S) VT))
    dm
    :eps *eps-double*)

  (define-test "svd-prod-complex-single"
      (multiple-value-bind (S U VT)
	  (svd cm :left :all :right :all :values :matrix)
	(gemm (gemm U S) VT))
    cm
    :eps *eps-single*)

  (define-test "svd-prod-complex-double"
      (multiple-value-bind (S U VT)
	  (svd zm :left :all :right :all :values :matrix)
	(gemm (gemm U S) VT))
    zm
    :eps *eps-double*)

  (multiple-value-bind (S U VT)
      (svd sm :left :all :right :all :values :matrix)
    (declare (ignore S))
    (define-test "svd-orth-single"
	(gemm U U :transa :trans)
      (gemm VT VT :transa :trans)
      :eps *eps-single*))

  (multiple-value-bind (S U VT)
      (svd dm :left :all :right :all :values :matrix)
    (declare (ignore S))
    (define-test "svd-orth-double"
	(gemm U U :transa :trans)
      (gemm VT VT :transa :trans)
      :eps *eps-double*))

  (multiple-value-bind (S U VT)
      (svd cm :left :all :right :all :values :matrix)
    (declare (ignore S))
    (define-test "svd-orth-complex-single"
	(gemm U U :transa :conjtrans)
      (gemm VT VT :transa :conjtrans)
      :eps *eps-single*))

  (multiple-value-bind (S U VT)
      (svd zm :left :all :right :all :values :matrix)
    (declare (ignore S))
    (define-test "svd-orth-complex-double"
	(gemm U U :transa :conjtrans)
      (gemm VT VT :transa :conjtrans)
      :eps *eps-double*))



)
