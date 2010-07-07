(in-package :sb-math)

(let ((sm (make-random-matrix (list (round (plain-rng 5 10))
				    (round (plain-rng 5 10)))
			      :element-type 'single-float))
      (dm (make-random-matrix (list (round (plain-rng 5 10))
				    (round (plain-rng 5 10)))
			      :element-type 'double-float))
      (cm (make-random-matrix (list (round (plain-rng 5 10))
				    (round (plain-rng 5 10)))
			      :element-type '(complex single-float)))
      (zm (make-random-matrix (list (round (plain-rng 5 10))
				    (round (plain-rng 5 10)))
			      :element-type '(complex double-float))))

;; transpose, row, col
  (define-test "transpose,row,col-single"
      (row sm 4) (col (transpose sm) 4) :eps *eps-single*)

  (define-test "transpose,row,col-double"
      (row dm 4) (col (transpose dm) 4) :eps *eps-double*)
  
  (define-test "transpose,row,col-complex-single"
      (row cm 4) (col (transpose cm) 4) :eps *eps-single*)
  
  (define-test "transpose,row,col-complex-double"
      (row zm 4) (col (transpose zm) 4) :eps *eps-double*)
  

;; set-col  
  (define-test "set-col-single"
      (progn (setf (col sm 2) (col sm 0))
	     (col sm 2))
    (col sm 0)
    :eps *eps-single*)

  (define-test "set-col-double"
      (progn (setf (col dm 2) (col dm 0))
	     (col dm 2))
    (col dm 0)
    :eps *eps-double*)

  (define-test "set-col-complex-single"
      (progn (setf (col cm 2) (col cm 0))
	     (col cm 2))
    (col cm 0)
    :eps *eps-single*)

  (define-test "set-col-complex-double"
      (progn (setf (col zm 2) (col zm 0))
	     (col zm 2))
    (col zm 0)
    :eps *eps-double*)

;; set-row
  (define-test "set-row-single"
      (progn (setf (row sm 2) (row sm 0))
	     (row sm 2))
    (row sm 0)
    :eps *eps-single*)

  (define-test "set-row-double"
      (progn (setf (row dm 2) (row dm 0))
	     (row dm 2))
    (row dm 0)
    :eps *eps-double*)

  (define-test "set-row-complex-single"
      (progn (setf (row cm 2) (row cm 0))
	     (row cm 2))
    (row cm 0)
    :eps *eps-single*)

  (define-test "set-row-complex-double"
      (progn (setf (row zm 2) (row zm 0))
	     (row zm 2))
    (row zm 0)
    :eps *eps-double*)

;; permute-rows
)
