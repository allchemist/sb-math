(in-package :sb-math)

(export
 '(imin imax mmin mmax
   iamin iamax ammin ammax
   msum amsum mean))

(declaim (inline imin imax iamin iamax))

(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
;; shut up compiler comments "can't tell the rank at compile time"

(define-with-types imin (:matrix-args matrix :rest-args (start end) :return :body :only-real t)
  (let ((min-val (the float-type (row-major-aref matrix 0)))
	(cur-val (the float-type (coerce 0.0 'float-type)))
	(min-pos (the fixnum 0)))
    (loop for i of-type fixnum from start below end do
      (progn
	(setf cur-val (row-major-aref matrix i))
	(when (< cur-val min-val)
	  (setf min-val cur-val
		min-pos i)))
      finally (return min-pos))))

(define-with-types imax (:matrix-args matrix :rest-args (start end) :return :body :only-real t)
  (let ((max-val (the float-type (row-major-aref matrix 0)))
	(cur-val (the float-type (coerce 0.0 'float-type)))
	(max-pos (the fixnum 0)))
    (loop for i of-type fixnum from start below end do
      (progn
	(setf cur-val (row-major-aref matrix i))
	(when (> cur-val max-val)
	  (setf max-val cur-val
		max-pos i)))
      finally (return max-pos))))

(defun imin (matrix &key (start 0) end)
  (declare (type simple-array matrix)
	   (optimize speed))
  (let ((element-type (array-element-type matrix)))
    (unless end (setf end (array-total-size matrix)))
    (assert (<= start end) nil "Given start position is later, than end position")
    (assert (not (subtypep element-type 'complex)) nil "This function is not applicable for complex data")
    (ecase element-type
      (single-float (simin matrix start end))
      (double-float (dimin matrix start end)))))

(defun imax (matrix &key (start 0) end)
  (declare (type simple-array matrix)
	   (optimize speed))
  (let ((element-type (array-element-type matrix)))
    (unless end (setf end (array-total-size matrix)))
    (assert (<= start end) nil "Given start position is later, than end position")
    (assert (not (subtypep element-type 'complex)) nil "This function is not applicable for complex data")
    (ecase element-type
	(single-float (simax matrix start end))
	(double-float (dimax matrix start end)))))

(defun mmin (matrix &key (start 0) end)
  (row-major-aref matrix (imin matrix :start start :end end)))
(defun mmax (matrix &key (start 0) end)
  (row-major-aref matrix (imax matrix :start start :end end)))

(define-with-types iamin (:matrix-args matrix :rest-args (start end) :return :body)
  (let ((min-val (abs (the float-type (row-major-aref matrix 0))))
	(cur-val (the float-type (coerce 0.0 'float-type)))
	(min-pos (the fixnum 0)))
    (loop for i of-type fixnum from start below end do
      (progn
	(setf cur-val (abs (row-major-aref matrix i)))
	(when (< cur-val min-val)
	  (setf min-val cur-val
		min-pos i)))
      finally (return min-pos))))

(define-with-types iamax (:matrix-args matrix :rest-args (start end) :return :body)
  (let ((max-val (abs (the float-type (row-major-aref matrix 0))))
	(cur-val (the float-type (coerce 0.0 'float-type)))
	(max-pos (the fixnum 0)))
    (loop for i of-type fixnum from start below end do
      (progn
	(setf cur-val (abs (row-major-aref matrix i)))
	(when (> cur-val max-val)
	  (setf max-val cur-val
		max-pos i)))
      finally (return max-pos))))

(defun iamin (matrix &key (start 0) end)
  (declare (sb-ext:muffle-conditions warning))
  (unless end (setf end (array-total-size matrix)))
  (assert (<= start end) nil "Given start position is later, than end position")
  (float-choice-funcall (array-element-type matrix) iamin nil matrix start end))
(defun iamax (matrix &key (start 0) end)
  (declare (sb-ext:muffle-conditions warning))
  (unless end (setf end (array-total-size matrix)))
  (assert (<= start end) nil "Given start position is later, than end position")
  (float-choice-funcall (array-element-type matrix) iamax nil matrix start end))

(defun ammin (matrix &key (start 0) end) (row-major-aref matrix (iamin matrix :start start :end end)))
(defun ammax (matrix &key (start 0) end) (row-major-aref matrix (iamax matrix :start start :end end)))

(define-with-types msum (:matrix-args matrix :rest-args (start end) :return :body)
  (let ((sum (the float-type (coerce 0.0 'float-type))))
    (loop for i of-type fixnum from start below end do
      (incf sum (row-major-aref matrix i)))
    (the float-type sum)))

(defun msum (matrix &key (start 0) end)
  (declare (type simple-array matrix)
	   (optimize speed))
  (unless end (setf end (array-total-size matrix)))
  (assert (<= start end) nil "Given start position is later, than end position")
  (float-choice-funcall (array-element-type matrix) msum nil matrix start end))

(define-with-types amsum (:matrix-args matrix :rest-args (start end) :return :body)
  (let ((sum (the float-type (coerce 0.0 'float-type))))
    (loop for i of-type fixnum from start below end do
      (incf sum (abs (row-major-aref matrix i))))
    (the float-type sum)))

(defun amsum (matrix &key (start 0) end)
  (declare (type simple-array matrix)
	   (optimize speed))
  (unless end (setf end (array-total-size matrix)))
  (assert (<= start end) nil "Given start position is later, than end position")
  (float-choice-funcall (array-element-type matrix) amsum nil matrix start end))

(defun mean (matrix &key (start 0) end)
  (/ (msum matrix :start start :end end) (array-total-size matrix)))

(declaim (sb-ext:unmuffle-conditions sb-ext:compiler-note))
