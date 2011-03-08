(in-package :sb-math)

(export
 '(col row diag))

(define-with-types col (:matrix-args (matrix dest) :rest-args (idx dim)
			:return (the (simple-array float-type) dest))
  (declare (type fixnum idx dim))
  (dotimes (i dim)
    (setf (aref dest i) (aref matrix i idx))))

(define-with-types set-col (:matrix-args (matrix source) :rest-args (idx dim))
  (declare (type fixnum idx dim))
  (dotimes (i dim)
    (setf (aref matrix i idx) (aref source i))))

(define-with-types row (:matrix-args (matrix dest) :rest-args (idx dim)
			:return (the (simple-array float-type) dest))
  (declare (type fixnum idx dim))
  (dotimes (i dim)
    (setf (aref dest i) (aref matrix idx i))))

(define-with-types set-row (:matrix-args (matrix source) :rest-args (idx dim))
  (declare (type fixnum idx dim))
  (dotimes (i dim)
    (setf (aref matrix idx i) (aref source i))))

(define-with-types diag (:matrix-args (matrix dest) :rest-args dim
			 :return (the (simple-array float-type) dest))
  (declare (type fixnum dim))
  (dotimes (i dim)
    (setf (aref dest i) (aref matrix i i))))

(define-with-types set-diag (:matrix-args (matrix source) :rest-args dim)
  (declare (type fixnum dim))
  (dotimes (i dim)
    (setf (aref matrix i i) (aref source i))))

(define-with-types sub-matrix (:matrix-args (matrix dest) :rest-args (dimensions offset))
  (let ((row-offset (first offset))
	(col-offset (second offset))
	(sub-dim0 (first dimensions))
	(sub-dim1 (second dimensions)))
    (declare (type fixnum row-offset col-offset sub-dim0 sub-dim1))
    (dotimes (i sub-dim0)
      (dotimes (j sub-dim1)
	(setf (aref dest i j) (aref matrix (+ row-offset i) (+ col-offset j))))))
  dest)

(define-with-types set-sub-matrix (:matrix-args (matrix source) :rest-args (dimensions offset))
  (let ((row-offset (first offset))
	(col-offset (second offset))
	(sub-dim0 (first dimensions))
	(sub-dim1 (second dimensions)))
    (declare (type fixnum row-offset col-offset sub-dim0 sub-dim1))
    (dotimes (i sub-dim0)
      (dotimes (j sub-dim1)
	(setf (aref matrix (+ row-offset i) (+ col-offset j)) (aref source i j)))))
  matrix)

;; highlevel

(defun col (matrix idx &optional dest)
  (declare (optimize speed)
	   (type fixnum idx)
	   (type simple-array matrix))
  (let ((element-type (array-element-type matrix))
	(dim0 (dim0 matrix)))
    (assert (< idx (dim1 matrix)) nil "Column index is larger than maximum")
    (if dest
	(assert (>= (dim0 dest) dim0) nil "Column destination is too small")
	(setf dest (the simple-array (make-matrix dim0 :element-type element-type))))
    (float-choice-funcall element-type col nil
      matrix dest idx dim0)))

(defun (setf col) (source matrix idx)
  (declare (optimize speed)
	   (type fixnum idx)
	   (type simple-array matrix source))
  (let ((element-type (array-element-type matrix))
	(dim0 (dim0 matrix)))
    (assert (< idx (dim1 matrix)) nil "Column index is larger than maximum")
    (assert (<= (dim0 source) dim0) nil "Destination matrix is too small")
    (float-choice-funcall element-type set-col nil
      matrix source idx dim0)))

(defun row (matrix idx &optional dest)
  (declare (optimize speed)
	   (type fixnum idx)
	   (type simple-array matrix))
  (let ((element-type (array-element-type matrix))
	(dim1 (dim1 matrix)))
    (assert (< idx (dim0 matrix)) nil "Row index is larger than maximum")
    (if dest
	(assert (>= (dim0 dest) dim1) nil "Row destination is too small")
	(setf dest (the simple-array (make-matrix dim1 :element-type element-type))))
    (float-choice-funcall element-type row nil
      matrix dest idx dim1)))

(defun (setf row) (source matrix idx)
  (declare (optimize speed)
	   (type fixnum idx)
	   (type simple-array matrix source))
  (let ((element-type (array-element-type matrix))
	(dim1 (dim1 matrix)))
    (assert (< idx (dim0 matrix)) nil "Column index is larger than maximum")
    (assert (<= (dim0 source) dim1) nil "Destination matrix is too small")
    (float-choice-funcall element-type set-row nil
      matrix source idx dim1)))

(defun sub-matrix (matrix dimensions offset &optional dest)
  (declare (optimize speed)
	   (type simple-array matrix))
  (float-choice-funcall (array-element-type matrix) sub-matrix nil
			matrix dest dimensions offset))

(defun (setf sub-matrix) (source matrix dimensions offset)
  (declare (optimize speed)
	   (type simple-array matrix))
  (float-choice-funcall (array-element-type matrix) set-sub-matrix nil
			matrix source dimensions offset))

(defun diag (matrix &optional dest)
  (declare (optimize speed)
	   (type simple-array matrix))
  (let ((element-type (array-element-type matrix))
	(dim (the fixnum (min (dim0 matrix) (dim1 matrix)))))
    (if dest
	(assert (>= (dim0 dest) dim) nil "Diag destination is too small")
	(setf dest (the simple-array (make-matrix dim :element-type element-type))))
    (float-choice-funcall element-type diag nil
			  matrix dest dim)))

(defun (setf diag) (source matrix)
  (declare (optimize speed)
	   (type simple-array matrix source))
  (let ((element-type (array-element-type matrix))
	(dim (the fixnum (min (dim0 matrix) (dim1 matrix)))))
    (assert (<= (dim0 source) dim) nil "Destination matrix is too small")
    (float-choice-funcall element-type set-diag nil
      matrix source dim)))
