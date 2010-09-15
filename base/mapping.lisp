(in-package :sb-math2)

(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

;; general mapping

;; map single matrix
(define-with-types map-matrix (:matrix-args matrix :rest-args func)
  (dotimes (i (the fixnum (array-total-size matrix)))
    (setf (the float-type (row-major-aref matrix i))
	  (the float-type (funcall func (the float-type (row-major-aref matrix i))))))
  (the (simple-array float-type) matrix))

(defun map-matrix (matrix func)
  (float-choice-funcall (array-element-type matrix) map-matrix nil
			matrix func))

;; map two matrices
(define-with-types map-two-matrices (:matrix-args (m1 m2) :rest-args func)
  (dotimes (i (the fixnum (array-total-size m1)))
    (setf (the float-type (row-major-aref m1 i))
	  (the float-type (funcall func (the float-type (row-major-aref m1 i))
				        (the float-type (row-major-aref m2 i))))))
  (the (simple-array float-type) m1))

(defun map-two-matrices (m1 m2 func)
  (float-choice-funcall (array-element-type m1) map-two-matrices nil
			m1 m2 func))


;; mapping with specified functions

(defmacro define-mapping-function (func)
  (let ((defs nil)
	(name (intern (string-upcase (concat-as-strings 'map-matrix- func)) :sb-math2)))
    (push
     `(define-with-types ,name (:matrix-args matrix)
	(dotimes (i (the fixnum (array-total-size matrix)))
	  (setf (the float-type (row-major-aref matrix i))
		(the float-type (,func (the float-type (row-major-aref matrix i))))))
	(the (simple-array float-type) matrix))
     defs)
    (push
     `(defun ,name (matrix)
	(float-choice-funcall (array-element-type matrix) ,name nil matrix))
     defs)
    `(progn ,@(nreverse defs))))

(defmacro define-pair-mapping-function (func)
  (let ((defs nil)
	(name (intern (string-upcase (concat-as-strings 'map-two-matrices- func)) :sb-math2)))
    (push
     `(define-with-types ,name (:matrix-args (m1 m2))
	(dotimes (i (the fixnum (array-total-size m1)))
	  (setf (the float-type (row-major-aref m1 i))
		(the float-type
		  (,func (the float-type (row-major-aref m1 i))
			 (the float-type (row-major-aref m2 i))))))
	(the (simple-array float-type) m1))
     defs)
    (push
     `(defun ,name (m1 m2)
	(float-choice-funcall (array-element-type m1) ,name nil m1 m2))
     defs)
    `(progn ,@(nreverse defs))))


(define-mapping-function square)
(define-mapping-function 1+)
(define-mapping-function 1-)
(define-mapping-function tanh)

;; and any other funtions

(declaim (sb-ext:unmuffle-conditions sb-ext:compiler-note))
