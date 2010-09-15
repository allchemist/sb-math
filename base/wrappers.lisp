(in-package :sb-math2)

(defmacro float-type-choice (type single double complex-single complex-double)
  `(cond ((eq ,type 'single-float) ,single)
	 ((eq ,type 'double-float) ,double)
	 ((equal ,type '(complex single-float)) ,complex-single)
	 ((equal ,type '(complex double-float)) ,complex-double)))

(defmacro float-choice-funcall (type func prefix &rest args)
  `(float-type-choice ,type
    (,(intern (string-upcase (concat-as-strings prefix 's func))) ,@args)
    (,(intern (string-upcase (concat-as-strings prefix 'd func))) ,@args)
    (,(intern (string-upcase (concat-as-strings prefix 'c func))) ,@args)
    (,(intern (string-upcase (concat-as-strings prefix 'z func))) ,@args)))

(defun parse-type-defs (code type)
  (let ((str (write-to-string code)))
    (let ((pos (search "FLOAT-TYPE" str)))
      (read-from-string
       (if pos
	   (parse-type-defs
	    (concatenate 'string (subseq str 0 pos) (write-to-string type) (subseq str (+ pos 10)))
	    type)
	   str)))))

(defmacro define-with-types (name (&key prefix matrix-args float-args rest-args only-real return-type) &body body)
  (let ((defs nil))
    (when (not (listp matrix-args)) (setf matrix-args `(,matrix-args)))
    (when (not (listp float-args)) (setf float-args `(,float-args)))
    (when (not (listp rest-args)) (setf rest-args `(,rest-args)))
    (dolist (type (if only-real
		      '((s single-float)
			(d double-float))
		      '((s single-float)
			(d double-float)
			(c (complex single-float))
			(z (complex double-float)))))
      (let* ((pre (first type))
	     (float-type (second type))
	     (array-type `(simple-array ,float-type))
	     (typed-name (intern (string-upcase (concat-as-strings prefix pre name)) :sb-math2)))
	(push
	 `(declaim (ftype (function (,@(make-list (length matrix-args) :initial-element array-type)
				     ,@(make-list (length float-args) :initial-element float-type)
				     ,@(make-list (length rest-args) :initial-element t))
				    ,(if return-type return-type array-type))
			  ,typed-name)
		   (inline ,typed-name))
	 defs)
	(push `(export ',typed-name) defs)
	(push
	 (parse-type-defs
	  `(defun ,typed-name (,@matrix-args ,@float-args ,@rest-args)
	     (declare (type ,array-type ,@matrix-args)
		      (type ,float-type ,@float-args)
		      (optimize speed (safety 0) (space 0)))
	     ,@body)
	  float-type)
	 defs)))
    `(progn ,@(nreverse defs))))