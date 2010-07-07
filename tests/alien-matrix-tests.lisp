(in-package :sb-math)

(let ((sm (make-random-matrix (list (round (plain-rng 5 10))
				    (round (plain-rng 5 10)))
			      :element-type 'single-float
			      :rng #'(lambda (x)
				       (declare (ignore x))
				       (plain-rng -1 1 :element-type 'single-float))))
      (dm (make-random-matrix (list (round (plain-rng 5 10))
				    (round (plain-rng 5 10)))
			      :element-type 'double-float
			      :rng #'(lambda (x)
				       (declare (ignore x))
				       (plain-rng -1 1 :element-type 'double-float))))
      (cm (make-random-matrix (list (round (plain-rng 5 10))
				    (round (plain-rng 5 10)))
			      :element-type '(complex single-float)
			      :rng #'(lambda (x)
				       (declare (ignore x))
				       (plain-rng -1 1 :element-type '(complex single-float)))))
      (zm (make-random-matrix (list (round (plain-rng 5 10))
				    (round (plain-rng 5 10)))
			      :element-type '(complex double-float)
			      :rng #'(lambda (x)
				       (declare (ignore x))
				       (plain-rng -1 1 :element-type '(complex double-float))))))

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

  (let ((perm (random-permutation (dim0 sm))))  
    (define-test "permute-rows-single"
	(let ((copy (copy sm)))
	  (do-matrix (perm i)
	    (setf (row copy (aref perm i)) (row sm i)))
	  copy)
      (permute-rows sm perm)
      :eps *eps-single*))

  (let ((perm (random-permutation (dim0 dm))))
    (define-test "permute-rows-double"
	(let ((copy (copy dm)))
	  (do-matrix (perm i)
	    (setf (row copy (aref perm i)) (row dm i)))
	  copy)
      (permute-rows dm perm)
      :eps *eps-double*))

  (let ((perm (random-permutation (dim0 cm))))  
    (define-test "permute-rows-complex-single"
	(let ((copy (copy cm)))
	  (do-matrix (perm i)
	    (setf (row copy (aref perm i)) (row cm i)))
	  copy)
      (permute-rows cm perm)
      :eps *eps-single*))

  (let ((perm (random-permutation (dim0 zm))))
    (define-test "permute-rows-complex-double"
	(let ((copy (copy zm)))
	  (do-matrix (perm i)
	    (setf (row copy (aref perm i)) (row zm i)))
	  copy)
      (permute-rows zm perm)
      :eps *eps-double*))

;; submatrix
  (let ((sub (make-matrix '(3 2) :element-type 'single-float)))
    (define-test "submatrix+set-single"
	(let ((copy (copy sm)))
	  (setf (submatrix copy '(1 2)) sub)
	  (submatrix copy '(3 2) '(1 2)))
      sub
      :eps *eps-single*))

  (let ((sub (make-matrix '(3 2) :element-type 'double-float)))
    (define-test "submatrix+set-double"
	(let ((copy (copy dm)))
	  (setf (submatrix copy '(1 2)) sub)
	  (submatrix copy '(3 2) '(1 2)))
      sub
      :eps *eps-double*))

  (let ((sub (make-matrix '(3 2) :element-type '(complex single-float))))
    (define-test "submatrix+set-complex-single"
	(let ((copy (copy cm)))
	  (setf (submatrix copy '(1 2)) sub)
	  (submatrix copy '(3 2) '(1 2)))
      sub
      :eps *eps-single*))

  (let ((sub (make-matrix '(3 2) :element-type '(complex double-float))))
    (define-test "submatrix+set-complex-double"
	(let ((copy (copy zm)))
	  (setf (submatrix copy '(1 2)) sub)
	  (submatrix copy '(3 2) '(1 2)))
      sub
      :eps *eps-double*))

;; m+/-c
  (define-test "m+/-c-single"
      (m-c (m+c (copy sm) 1) 1)
    sm
    :eps *eps-single*)
  
  (define-test "m+/-c-double"
      (m-c (m+c (copy dm) 1) 1)
    dm
    :eps *eps-double*)
#|  
  (define-test "m+/-c-complex-single"
      (m-c (m+c (copy cm) 1) 1)
    sm
    :eps *eps-single*)
  
  (define-test "m+/-c-complex-double"
      (m-c (m+c (copy zm) 1) 1)
    sm
    :eps *eps-double*)
|#
;; m*c

  (define-test "m*-single"
      (row (m* (copy sm) sm) 2)
    (map '(vector single-float) #'square (row sm 2))
    :eps *eps-single*)

  (define-test "m*-double"
      (row (m* (copy dm) dm) 2)
    (map '(vector double-float) #'square (row dm 2))
    :eps *eps-double*)

  (define-test "m*-complex-single"
      (row (m* (copy cm) cm) 2)
    (map '(vector (complex single-float)) #'square (row cm 2))
    :eps *eps-single*)

  (define-test "m*-complex-double"
      (row (m* (copy zm) zm) 2)
    (map '(vector (complex double-float)) #'square (row zm 2))
    :eps *eps-double*)
  
;; min
  (define-test "mmin-single"
      (mmin sm)
    (apply #'min (coerce (sb-ext:array-storage-vector sm) 'list))
    :eps *eps-single*)

  (define-test "mmin-double"
      (mmin dm)
    (apply #'min (coerce (sb-ext:array-storage-vector dm) 'list))
    :eps *eps-double*)

;; min abs
  (define-test "ammin-single"
      (abs (ammin sm))
    (apply #'min (map 'list #'abs (sb-ext:array-storage-vector sm)))
    :eps *eps-single*)

  (define-test "ammin-double"
      (abs (ammin dm))
    (apply #'min (map 'list #'abs (sb-ext:array-storage-vector dm)))
    :eps *eps-double*)
#|
  (define-test "ammin-complex-single"
      (abs (ammin cm))
    (apply #'min (map 'list #'abs (sb-ext:array-storage-vector cm)))
    :eps *eps-single*)

  (define-test "ammin-complex-double"
      (abs (ammin zm))
    (apply #'min (map 'list #'abs (sb-ext:array-storage-vector zm)))
    :eps *eps-double*)
|#

)
