(in-package :sb-math)

(define-alien-type CBLAS_ORDER (enum nil (CblasRowMajor 101) (CblasColMajor 102)))
(define-alien-type CBLAS_TRANSPOSE (enum nil (:notrans 111) (:trans 112) (:conjtrans 113)))
(define-alien-type CBLAS_UPLO (enum nil (:upper 121) (:lower 122)))
(define-alien-type CBLAS_DIAG (enum nil (:nonunit 131) (:unit 132)))
(define-alien-type CBLAS_SIDE (enum nil (:left 141) (:right 142)))

(defmacro define-alien-wrapper (name &key matrix-args float-args rest-args lets pre alien-args return return-type)
  (when (not (listp matrix-args)) (setf matrix-args (list matrix-args)))
  (when (not (listp float-args)) (setf float-args (list float-args)))
  (when (not (listp rest-args)) (setf rest-args (list rest-args)))
  (when (not return) (setf return (car (last matrix-args))))
  (flet ((parse-alien-arg (arg type)
	   (cond ((member arg matrix-args)
		  `(array-sap ,arg))
		 ((member arg float-args)
		  (cond ((subtypep type 'real) `(coerce ,arg ',type))
			((subtypep type 'complex) `(complex-sap (coerce ,arg ',type)))))
		 ((member arg rest-args) arg)
		 (t arg))))
    (let ((defs nil))
      (dolist (type '((s single-float)
		      (d double-float)
		      (c (complex single-float))
		      (z (complex double-float))))
	(let ((typed-name (intern (string-upcase (concat-as-strings (first type) name)) :sb-math))
	      (alien-name (intern (string-upcase (concat-as-strings '% (first type) name)) :sb-math))
	      (array-type `(simple-array ,(second type))))
	  (push
	   `(declaim (ftype (function
			     (,@(make-list (length matrix-args) :initial-element array-type)
			      ,@(make-list (length float-args) :initial-element (second type))
			      ,@(make-list (length rest-args) :initial-element t))
			     ,(if return-type return-type array-type))
			    ,typed-name))
	   defs)
	  (push `(declaim (inline ,typed-name)) defs)
	  (push `(export ',typed-name) defs)
	  (push 
	    `(defun ,typed-name (,@matrix-args ,@float-args ,@rest-args)
	       (declare (type ,array-type ,@matrix-args)
			(optimize speed (safety 0) (space 0))
			(type ,(second type) ,@float-args)
					;(sb-ext:muffle-conditions sb-ext:compiler-note)
			)
	       (let ,lets
		 ,@pre
		 (sb-sys:with-pinned-objects (,@(if (subtypep (second type) 'complex)
						    (append matrix-args float-args)
						    matrix-args))
		   (,alien-name
		    ,@(mapcar #'(lambda (arg)
				  (parse-alien-arg arg (second type)))
			      alien-args)))
		 ,(if return-type `(the ,return-type ,return) `(the ,array-type ,return)))) 
	    ;(second type))
	   defs)))
      `(progn ,@(nreverse defs)))))
