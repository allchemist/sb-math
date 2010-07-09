(in-package :sb-math)

(defun double-list (elem)
  (list elem elem))

(let ((sm (make-random-matrix (double-list (round (plain-rng 5 10)))
			      :element-type 'single-float
			      :rng #'(lambda (x)
				       (declare (ignore x))
				       (plain-rng -1 1 :element-type 'single-float))))
      (dm (make-random-matrix (double-list (round (plain-rng 5 10)))
			      :element-type 'double-float
			      :rng #'(lambda (x)
				       (declare (ignore x))
				       (plain-rng -1 1 :element-type 'double-float))))
      (cm (make-random-matrix (double-list (round (plain-rng 5 10)))
			      :element-type '(complex single-float)
			      :rng #'(lambda (x)
				       (declare (ignore x))
				       (plain-rng -1 1 :element-type '(complex single-float)))))
      (zm (make-random-matrix (double-list (round (plain-rng 5 10)))
			      :element-type '(complex double-float)
			      :rng #'(lambda (x)
				       (declare (ignore x))
				       (plain-rng -1 1 :element-type '(complex double-float))))))
;; lu-invese
  (define-test "lu-inverse-single"
      (gemm (lu-inverse sm) sm)
    (make-matrix (double-list (dim0 sm))
		 :element-type 'single-float
		 :initial-element 1.0)
    :eps *eps-single*)

  (define-test "lu-inverse-double"
      (gemm (lu-inverse dm) dm)
    (make-matrix (double-list (dim0 dm))
		 :element-type 'double-float
		 :initial-element 1d0)
    :eps *eps-double*)

  (define-test "lu-inverse-complex-single"
      (gemm (lu-inverse cm) cm)
    (make-matrix (double-list (dim0 cm))
		 :element-type '(complex single-float)
		 :initial-element (complex 1.0))
    :eps *eps-single*)

  (define-test "lu-inverse-complex-double"
      (gemm (lu-inverse zm) zm)
    (make-matrix (double-list (dim0 zm))
		 :element-type '(complex double-float)
		 :initial-element (complex 1d0))
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
    (make-matrix (double-list (dim0 sm))
		 :element-type 'single-float
		 :initial-element 1.0)
    :eps *eps-single*)
  
  (define-test "lu-solve-couple-double"
      (lu-solve dm dm)
    (make-matrix (double-list (dim0 dm))
		 :element-type 'double-float
		 :initial-element 1d0)
    :eps *eps-double*)

  (define-test "lu-solve-couple-complex-single"
      (lu-solve cm cm)
    (make-matrix (double-list (dim0 cm))
		 :element-type '(complex single-float)
		 :initial-element (complex 1.0))
    :eps *eps-single*)

  (define-test "lu-solve-couple-complex-double"
      (lu-solve zm zm)
    (make-matrix (double-list (dim0 zm))
		 :element-type '(complex double-float)
		 :initial-element (complex 1d0))
    :eps *eps-double*)

;; eigen
#|
  (define-test "eigen-single"
      (multiple-value-bind (vals vecs)
	  (eigen sm :values :matrix :real-values t)
	(gemm (gemm vecs vals) vecs :transb :trans))
    sm
    :eps *eps-single*)

  (define-test "eigen-double"
      (multiple-value-bind (vals vecs)
	  (eigen dm :values :matrix :real-values t)
	(gemm (gemm vecs vals) vecs :transb :trans))
    dm
    :eps *eps-double*)

  (define-test "eigen-complex-single"
      (multiple-value-bind (vals vecs)
	  (eigen cm :values :matrix)
	(gemm (gemm vecs vals) vecs :transb :trans))
    cm
    :eps *eps-single*)

  (define-test "eigen-complex-double"
      (multiple-value-bind (vals vecs)
	  (eigen zm :values :matrix)
	(gemm (gemm vecs vals) vecs :transb :trans))
    zm
    :eps *eps-double*)
|#
)
