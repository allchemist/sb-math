(in-package :sb-math)

(defmacro define-mapping-function (func)
  (let ((defs nil)
	(result-name (intern (string-upcase
			      (concat-as-strings 'map-matrix- func))
			     :sb-math)))
    (dolist (type '((s single-float)
		    (d double-float)
		    (c (complex single-float))
		    (z (complex double-float))))
      (let ((array-type `(simple-array ,(second type)))
	    (func-name (intern (string-upcase
				(concat-as-strings (first type) 'map-matrix- func))
			       :sb-math)))
	(push `(declaim (ftype (function (,array-type) ,array-type) ,func-name)) defs)
	(push
	 `(defun ,func-name (matrix)
	    (declare (type ,array-type matrix)
		     (optimize speed (safety 0) (space 0))
		     (inline ,func)
		     (sb-ext:muffle-conditions sb-ext:compiler-note))
	    (dotimes (i (the fixnum (array-total-size matrix)))
	      (declare (type fixnum i))
	      (let ((val (the ,(second type) (row-major-aref matrix i))))
		(declare (type ,(second type) val))
		(setf (the ,(second type) (row-major-aref matrix i))
		      (,func val))))
	    matrix)
	 defs)))
    (push 
     `(defun ,result-name (matrix)
	(declare (sb-ext:unmuffle-conditions sb-ext:compiler-note))
	(float-choice-funcall (array-element-type matrix) ,result-name nil matrix))
     defs)
    (push `(export ',result-name) defs)
    `(progn ,@(nreverse defs))))

(define-mapping-function square)
(define-mapping-function tanh)
(define-mapping-function sqrt)
(define-mapping-function 1+)
(define-mapping-function 1-)
(define-mapping-function simple-rng)

;; + any other functions

(defmacro define-pair-mapping-function (func)
  (let ((defs nil)
	(result-name (intern (string-upcase
			      (concat-as-strings 'map-two-matrices- func))
			     :sb-math)))
    (dolist (type '((s single-float)
		    (d double-float)
		    (c (complex single-float))
		    (z (complex double-float))))
      (let ((array-type `(simple-array ,(second type)))
	    (func-name (intern (string-upcase
				(concat-as-strings (first type) 'map-two-matrices- func))
			       :sb-math)))
	(push
	 `(declaim (ftype (function (,array-type ,array-type fixnum)
				    ,array-type)
			  ,func-name))
	 defs)
	(push
	 `(defun ,func-name (m1 m2 size)
	    (declare (type ,array-type m1 m2)
		     (type fixnum size)
		     (optimize speed (safety 0) (space 0))
		     (inline ,func))
	    (dotimes (i size)
	      (declare (type fixnum i))
	      (let ((val1 (the ,(second type) (row-major-aref m1 i)))
		    (val2 (the ,(second type) (row-major-aref m2 i))))
		(declare (type ,(second type) val1 val2))
		(setf (the ,(second type) (row-major-aref m1 i))
		      (,func val1 val2))))
	    (the ,array-type m1))
	 defs)))
    (push 
     `(defun ,result-name (m1 m2)
	(let ((type (array-element-type m1))
	      (dim (the fixnum (array-total-size m1))))
	  (declare (type fixnum dim))
	  (assert (= dim (the fixnum (array-total-size m2))) nil "Matrix sizes not equal")
	  (assert (equal type (array-element-type m2)) nil "Matrix types not equal")
	  (float-choice-funcall type ,result-name nil
			      m1 m2 size)))
     defs)
    (push `(export ',result-name) defs)
    `(progn ,@(nreverse defs))))

(defun make-simple-random-matrix (dimensions &key
				  (element-type *default-type*))
  (map-matrix-simple-rng (make-matrix dimensions :element-type element-type)))
