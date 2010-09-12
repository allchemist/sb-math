(in-package :sb-math)

(defun transpose (matrix &optional dest)
  (let ((element-type (array-element-type matrix))
	(dim0 (dim0 matrix))
	(dim1 (dim1 matrix)))
    (if dest
	(assert (and (= (dim1 dest) dim0) (= (dim0 dest) dim1))
		nil "Improper destination dimensions for transposition")
	(setf dest (make-matrix (list dim1 dim0) :element-type element-type)))
    (sb-sys:with-pinned-objects (matrix dest)
      (float-choice-funcall element-type trans %
	       (array-sap matrix) (array-sap dest) dim0 dim1))
    dest))

(defun col (matrix col &optional dest)
  (let ((element-type (array-element-type matrix))
	(dim0 (dim0 matrix))
	(dim1 (dim1 matrix)))
    (assert (< col dim1) nil "Column index is larger than maximum")
    (if dest
	(assert (>= (dim0 dest) dim0) nil "Column destination length is too little")
	(setf dest (make-matrix dim0 :element-type element-type)))
    (sb-sys:with-pinned-objects (matrix dest)
      (float-choice-funcall element-type col %
	       (array-sap matrix) (array-sap dest) dim0 dim1 col))
    dest))

(defun (setf col) (source matrix col)
  (let ((element-type (array-element-type matrix))
	(dim0 (array-dimension matrix 0))
	(dim1 (array-dimension matrix 1)))
    (assert (< col dim1) nil "Column index is larger than maximum")
    (assert (= (dim0 source) dim0) nil "Improper column length")
    (sb-sys:with-pinned-objects (matrix source)
      (float-choice-funcall element-type setcol %
	       (array-sap matrix) (array-sap source) dim0 dim1 col))
    matrix))

(defun row (matrix row &optional dest)
  (let ((element-type (array-element-type matrix))
	(dim0 (array-dimension matrix 0))
	(dim1 (array-dimension matrix 1)))
    (assert (< row dim0) nil "Row index is larger than maximum")
    (if dest
	(assert (>= (dim0 dest) dim1) nil "Row destination length is too little")
	(setf dest (make-matrix dim1 :element-type element-type)))
    (sb-sys:with-pinned-objects (matrix dest)
      (float-choice-funcall element-type row %
	       (array-sap matrix) (array-sap dest) dim0 dim1 row))
    dest))

(defun (setf row) (source matrix row)
  (let ((element-type (array-element-type matrix))
	(dim0 (array-dimension matrix 0))
	(dim1 (array-dimension matrix 1)))
    (assert (< row dim0) nil "Row index is larger than maximum")
    (assert (= (dim0 source) dim1) nil "Improper row length")
    (sb-sys:with-pinned-objects (matrix source)
      (float-choice-funcall element-type setrow %
	       (array-sap matrix) (array-sap source) dim0 dim1 row))
    matrix))

(defun permute-rows (matrix perm &optional dest)
  (let ((element-type (array-element-type matrix))
	(dim0 (array-dimension matrix 0))
	(dim1 (array-dimension matrix 1)))
    (assert (= dim0 (dim0 perm)) nil "Permutation vector size is not equal to number of rows")
    (if dest
	(assert (and (= dim0 (array-dimension dest 0))
		     (= dim1 (array-dimension dest 1)))
		nil "Improper destination matrix dimensions")
	(setf dest (make-matrix (list dim0 dim1) :element-type element-type)))
    (sb-sys:with-pinned-objects (matrix perm dest)
      (float-choice-funcall element-type rowperm %
	       (array-sap matrix) (array-sap perm) (array-sap dest) dim0 dim1))
    dest))

(defun random-permutation (size)
  (let ((perm (make-array size :element-type '(unsigned-byte 32))))
    (do-matrix (perm i)
      (setf (aref perm i) i))
    (dotimes (i (1+ (random size)))
      (rotatef (aref perm (random size))
	       (aref perm (random size))))
    perm))

(defun submatrix (matrix dimensions offset &optional dest)
  (let ((element-type (array-element-type matrix))
	(dim0 (dim0 matrix))
	(dim1 (dim1 matrix))
	(sub-dim0 (first dimensions))
	(sub-dim1 (second dimensions))
	(x-offset (first offset))
	(y-offset (second offset)))
    (assert (and (<= (+ sub-dim0 x-offset) dim0)
		 (<= (+ sub-dim1 y-offset) dim1))
	    nil "Attempt to take submatrix larger than possible")
    (assert (and (>= x-offset 0) (>= y-offset 0))
	    nil "Start position must be positive")
    (if dest
	(assert (equal dimensions (array-dimensions dest))
		nil "Improper destination matrix dimensions")
	(setf dest (make-matrix dimensions :element-type element-type)))
    (sb-sys:with-pinned-objects (matrix dest)
      (float-choice-funcall element-type submatrix %
	       (array-sap matrix) (array-sap dest) dim0 dim1
	       x-offset y-offset sub-dim0 sub-dim1))
    dest))

(defun (setf submatrix) (source matrix offset)
  (let ((element-type (array-element-type matrix))
	(dim0 (dim0 matrix))
	(dim1 (dim1 matrix))
	(sub-dim0 (array-dimension source 0))
	(sub-dim1 (array-dimension source 1))
	(x-offset (first offset))
	(y-offset (second offset)))
    (assert (and (<= (+ sub-dim0 x-offset) dim0)
		 (<= (+ sub-dim1 y-offset) dim1))
	    nil "Attempt to take submatrix larger than possible")
    (assert (and (>= x-offset 0) (>= y-offset 0))
	    nil "Start position must be positive")
    (sb-sys:with-pinned-objects (matrix source)
      (float-choice-funcall element-type setsubmatrix %
	       (array-sap matrix) (array-sap source) dim0 dim1
	       x-offset y-offset sub-dim0 sub-dim1))
    matrix))

(defun m+c (matrix const)
  (let ((type (array-element-type matrix)))
    (sb-sys:with-pinned-objects (matrix const)
      (float-choice-funcall type mplusc %
	       (array-sap matrix) (maybe-complex (coerce const type)) (array-total-size matrix)))
    matrix))

(defun m-c (matrix const)
  (declare (inline m+c))
  (m+c matrix (- const)))

(defun m* (m1 m2)
  (assert (equal (array-dimensions m1) (array-dimensions m2))
	  nil "Matrices has unequal dimensions")
  (sb-sys:with-pinned-objects (m1 m2)
    (float-choice-funcall (array-element-type m1) mmult %
			  (array-sap m1) (array-sap m2) (array-total-size m1)))
  m1)

(defun imin (matrix)
  (let ((element-type (array-element-type matrix)))
    (assert (not (subtypep element-type 'complex)) nil "This function is not applicable for complex data")
    (sb-sys:with-pinned-objects (matrix)
      (ecase element-type
	(single-float (%simin (array-sap matrix) (array-total-size matrix)))
	(double-float (%dimin (array-sap matrix) (array-total-size matrix)))))))

(defun mmin (matrix)
  (declare (inline imin))
  (row-major-aref matrix (imin matrix)))

(defun imax (matrix)
  (let ((element-type (array-element-type matrix)))
    (assert (not (subtypep element-type 'complex)) nil "This function is not applicable for complex data")
    (sb-sys:with-pinned-objects (matrix)
      (ecase element-type
	(single-float (%simax (array-sap matrix) (array-total-size matrix)))
	(double-float (%dimax (array-sap matrix) (array-total-size matrix)))))))

(defun mmax (matrix)
  (declare (inline imax))
  (row-major-aref matrix (imax matrix)))

(defun iamin (matrix)
  (sb-sys:with-pinned-objects (matrix)
    (float-choice-funcall (array-element-type matrix) iamin %
	     (array-sap matrix) (array-total-size matrix))))

(defun ammin (matrix)
  (declare (inline iamin))
  (row-major-aref matrix (iamin matrix)))

(defun msum (matrix)
  (let ((element-type (array-element-type matrix)))
    (assert (not (subtypep element-type 'complex)) nil "This function is not applicable for complex data")
    (sb-sys:with-pinned-objects (matrix)
       (ecase element-type
	(single-float (%smsum (array-sap matrix) (array-total-size matrix)))
	(double-float (%dmsum (array-sap matrix) (array-total-size matrix)))))))

(defun mean (matrix)
  (declare (inline msum))
  (/ (msum matrix) (array-total-size matrix)))

(defmacro map-matrix-cb (func matrix)
  `(let ((element-type (array-element-type ,matrix)))
     (sb-sys:with-pinned-objects (,matrix)
       (float-type-choice
	element-type
	(%smapmatrix (array-sap ,matrix) (array-total-size ,matrix) (function-sap ,func single-float))
	(%dmapmatrix (array-sap ,matrix) (array-total-size ,matrix) (function-sap ,func double-float))
	(%cmapmatrix (array-sap ,matrix) (array-total-size ,matrix) (function-sap ,func system-area-pointer))
	(%zmapmatrix (array-sap ,matrix) (array-total-size ,matrix) (function-sap ,func system-area-pointer))))
     ,matrix))

(defun sq-matrix (matrix)
  (sb-sys:with-pinned-objects (matrix)
    (float-choice-funcall (array-element-type matrix) sqmatrix %
			  (array-sap matrix) (array-total-size matrix)))
  matrix)
