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
  (define-test "lu-inverse-single"
      (gemm (lu-inverse sm) sm)
    (diag (make-matrix (dim0 sm)
		       :element-type 'single-float
		       :initial-element 1.0))
    :eps *eps-single*)

  (define-test "lu-inverse-double"
      (gemm (lu-inverse dm) dm)
    (diag (make-matrix (dim0 dm)
		       :element-type 'double-float
		       :initial-element 1d0))
    :eps *eps-double*)

  (define-test "lu-inverse-complex-single"
      (gemm (lu-inverse cm) cm)
    (diag (make-matrix (dim0 cm)
		       :element-type '(complex single-float)
		       :initial-element (complex 1.0)))
    :eps *eps-single*)

  (define-test "lu-inverse-complex-double"
      (gemm (lu-inverse zm) zm)
    (diag (make-matrix (dim0 zm)
		       :element-type '(complex double-float)
		       :initial-element (complex 1d0)))
    :eps *eps-double*)

;; lu-solve
  (define-test "lu-solve-single"
      (gemv sm (lu-solve sm (col sm 0))) (col sm 0) :eps *eps-single*)

  (define-test "lu-solve-double"
      (gemv dm (lu-solve dm (col dm 0))) (col dm 0) :eps *eps-double*)

  (define-test "lu-solve-complex-single"
      (gemv cm (lu-solve cm (col cm 0))) (col cm 0) :eps *eps-single*)

  (define-test "lu-solve-complex-double"
      (gemv zm (lu-solve zm (col zm 0))) (col zm 0) :eps *eps-double*)

  (define-test "lu-solve-couple-single"
      (lu-solve sm sm)
    (diag (make-matrix (dim0 sm)
		       :element-type 'single-float
		       :initial-element 1.0))
    :eps *eps-single*)
  
  (define-test "lu-solve-couple-double"
      (lu-solve dm dm)
    (diag (make-matrix (dim0 dm)
		       :element-type 'double-float
		       :initial-element 1d0))
    :eps *eps-double*)

  (define-test "lu-solve-couple-complex-single"
      (lu-solve cm cm)
    (diag (make-matrix (dim0 cm)
		       :element-type '(complex single-float)
		       :initial-element (complex 1.0)))
    :eps *eps-single*)

  (define-test "lu-solve-couple-complex-double"
      (lu-solve zm zm)
    (diag (make-matrix (dim0 zm)
		       :element-type '(complex double-float)
		       :initial-element (complex 1d0)))
    :eps *eps-double*)

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
