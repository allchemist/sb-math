(in-package :sb-math)


;; general matrices, assumed as typed

(defun square-matrix-p (matrix)
  (= (dim0 matrix) (dim1 matrix)))

(defmacro do-upper-triangle ((matrix &optional i j) &body body)
  (let ((m (or i (gensym)))
	(n (or j (gensym))))
    `(assert (square-matrix-p ,matrix) nil "Matrix should be square")
    `(dotimes (,m (dim0 ,matrix))
       (loop for ,n from ,m to (1- (dim1 ,matrix))
	     do
	  (progn
	    ,@body)))))

(defmacro do-lower-triangle ((matrix &optional i j) &body body)
  `(do-upper-triangle (,matrix ,j ,i)
     ,@body))

(defun mirror-upper-triangle (matrix)
  (do-upper-triangle (matrix i j)
    (setf (aref matrix j i)
	  (aref matrix i j)))
  matrix)

(defun mirror-lower-triangle (matrix)
  (do-upper-triangle (matrix i j)
    (setf (aref matrix i j)
	  (aref matrix j i)))
  matrix)

(defun mirror-upper-hermitian-triangle (matrix)
  (do-upper-triangle (matrix i j)
    (setf (aref matrix j i)
	  (conjugate (aref matrix i j))))
  matrix)

(defun mirror-lower-hermitian-triangle (matrix)
  (do-upper-triangle (matrix i j)
    (setf (aref matrix i j)
	  (conjugate (aref matrix j i))))
  matrix)

(defmacro do-diag ((matrix &optional i) &body body)
  (let ((n (or i (gensym))))
    `(dotimes (,n (min (dim0 ,matrix) (dim1 ,matrix)))
       ,@body)))

(defun diag (vec &key dest)
  (let ((dim (dim0 vec)))
    (if dest
	(assert (and (>= (dim0 dest) dim)
		     (>= (dim1 dest) dim))
		nil "Destination matrix is smaller than provided vector")
	(setf dest
	      (make-matrix (list dim dim)
			   :element-type (array-element-type vec))))
    (dotimes (i dim)
      (setf (aref dest i i)
	    (aref vec i)))
    dest))
