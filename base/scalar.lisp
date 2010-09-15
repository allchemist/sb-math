(in-package :sb-math2)

(declaim (inline imin imax iamin iamax))

(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
;; shut up compiler comments "can't tell the rank at compile time"

(define-with-types imin (:matrix-args matrix :return-type fixnum :only-real t)
  (let ((min-val (the float-type (row-major-aref matrix 0)))
	(min-pos (the fixnum 0)))
    (dotimes (i (the fixnum (array-total-size matrix)) min-pos)
      (let ((cur-val (the float-type (row-major-aref matrix i))))
	(when (< cur-val min-val)
	  (setf min-val cur-val
		min-pos i))))))

(define-with-types imax (:matrix-args matrix :return-type fixnum :only-real t)
  (let ((min-val (the float-type (row-major-aref matrix 0)))
	(min-pos (the fixnum 0)))
    (dotimes (i (the fixnum (array-total-size matrix)) min-pos)
      (let ((cur-val (the float-type (row-major-aref matrix i))))
	(when (> cur-val min-val)
	  (setf min-val cur-val
		min-pos i))))))

(defun imin (matrix)
  (declare (type simple-array matrix)
	   (optimize speed))
  (let ((element-type (array-element-type matrix)))
    (assert (not (subtypep element-type 'complex)) nil "This function is not applicable for complex data")
    (ecase element-type
      (single-float (simin matrix))
      (double-float (dimin matrix)))))
			  
(defun imax (matrix)
  (declare (type simple-array matrix)
	   (optimize speed))
  (let ((element-type (array-element-type matrix)))
    (assert (not (subtypep element-type 'complex)) nil "This function is not applicable for complex data")
    (ecase element-type
	(single-float (simax matrix))
	(double-float (dimax matrix)))))

(defun mmin (matrix) (row-major-aref matrix (imin matrix)))
(defun mmax (matrix) (row-major-aref matrix (imax matrix)))

(define-with-types iamin (:matrix-args matrix :return-type fixnum)
  (let ((min-val (abs (the float-type (row-major-aref matrix 0))))
	(min-pos (the fixnum 0)))
    (dotimes (i (the fixnum (array-total-size matrix)) min-pos)
      (let ((cur-val (abs (the float-type (row-major-aref matrix i)))))
	(when (< cur-val min-val)
	  (setf min-val cur-val
		min-pos i))))))

(define-with-types iamax (:matrix-args matrix :return-type fixnum)
  (let ((min-val (abs (the float-type (row-major-aref matrix 0))))
	(min-pos (the fixnum 0)))
    (dotimes (i (the fixnum (array-total-size matrix)) min-pos)
      (let ((cur-val (abs (the float-type (row-major-aref matrix i)))))
	(when (> cur-val min-val)
	  (setf min-val cur-val
		min-pos i))))))

(defun iamin (matrix)
  (declare (type simple-array matrix)
	   (optimize speed))
  (float-choice-funcall (array-element-type matrix) iamin nil matrix))

(defun iamax (matrix)
  (declare (type simple-array matrix)
	   (optimize speed))
  (float-choice-funcall (array-element-type matrix) iamax nil matrix))
			  
(defun ammin (matrix) (row-major-aref matrix (iamin matrix)))
(defun ammax (matrix) (row-major-aref matrix (iamax matrix)))

(define-with-types msum (:matrix-args matrix :return-type t)
  (let ((sum (the float-type (coerce 0.0 'float-type))))
    (dotimes (i (the fixnum (array-total-size matrix)))
      (incf sum (row-major-aref matrix i)))
    (the float-type sum)))

(defun msum (matrix)
  (declare (type simple-array matrix)
	   (optimize speed))
  (float-choice-funcall (array-element-type matrix) msum nil matrix))

(define-with-types amsum (:matrix-args matrix :return-type t)
  (let ((sum 0))
    (dotimes (i (the fixnum (array-total-size matrix)))
      (incf sum (abs (row-major-aref matrix i))))
    sum))

(defun amsum (matrix)
  (declare (type simple-array matrix)
	   (optimize speed))
  (float-choice-funcall (array-element-type matrix) amsum nil matrix))

(declaim (sb-ext:unmuffle-conditions sb-ext:compiler-note))
