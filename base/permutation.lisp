(in-package :sb-math)

(export
 '(row-perm random-permutation))

(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

(define-with-types row-perm (:matrix-args (matrix dest) :rest-args perm
			     :return (the (simple-array float-type) dest))
  (let ((dest-pos 0) (source-pos 0) (dim0 (dim0 matrix)) (dim1 (dim1 matrix)))
    (declare (type fixnum dest-pos source-pos dim0 dim1)
	     (type (simple-array fixnum) perm))
    (dotimes (j dim0)
      (setf dest-pos (* dim1 (aref perm j)))
      (setf source-pos (* dim1 j))
      (dotimes (i dim1)
	(setf (row-major-aref dest dest-pos)
	      (row-major-aref matrix source-pos))
	(incf dest-pos)
	(incf source-pos)))))

(defun row-perm (matrix perm &optional dest)
  (declare (type simple-array matrix)
	   (type (simple-array fixnum) perm)
	   (optimize speed))
  (let ((element-type (array-element-type matrix))
	(dim0 (dim0 matrix))
	(dim1 (dim1 matrix)))
    (assert (= dim0 (dim0 perm)) nil "Permutation vector size is not equal to number of rows")
    (if dest
	(assert (and (= dim0 (dim0 dest))
		     (= dim1 (dim1 dest)))
		nil "Improper destination matrix dimensions")
	(setf dest (the simple-array (make-matrix (list dim0 dim1) :element-type element-type))))
    (float-choice-funcall element-type row-perm nil
			  matrix dest perm)))

;; tmp
(defun random-permutation (size)
  (let ((perm (the (simple-array fixnum)
		(make-array size :element-type 'fixnum))))
    (dotimes (i size)
      (setf (aref perm i) i))
    (dotimes (i (1+ (random size)))
      (rotatef (aref perm (random size))
	       (aref perm (random size))))
    perm))


#|(define-with-types transpose (:matrix-args (matrix dest) :return (the (simple-array float-type) dest))
  (dotimes (i (dim0 matrix))
    (dotimes (j (dim1 matrix))
      (setf (aref dest j i) (aref matrix i j)))))

(defun transpose (matrix &optional dest)
  (declare (type simple-array matrix)
	   (optimize speed))
  (let ((element-type (array-element-type matrix))
	(dim0 (dim0 matrix))
	(dim1 (dim1 matrix)))
    (if dest
	(assert (and (= dim0 (dim1 dest))
		     (= dim1 (dim0 dest)))
		nil "Improper destination matrix dimensions")
	(setf dest (the simple-array (make-matrix (list dim1 dim0) :element-type element-type))))
    (float-choice-funcall element-type transpose nil
			  matrix dest)))|#

(declaim (sb-ext:unmuffle-conditions sb-ext:compiler-note))
