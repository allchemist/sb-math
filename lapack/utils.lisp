(in-package :sb-math)

(defun lapack-char-code (char)
  (char-code
   (ecase char
     (:none #\N)
     (:eval #\V)
     (:notrans #\N)
     (:trans #\T)
     (:conjtrans #\C)
     (:all #\A)
     (:singular #\S))))

(defun lapack-float-type (type-char)
  (ecase type-char
    (s 'single-float)
    (d 'double-float)
    (c 't)
    (z 't)))

(defun lapack-real-float-type (type-char)
  (ecase type-char
    ((s c) 'single-float)
    ((d z) 'double-float)))

(defun lapack-parse-alien-arg (arg type)
  (list (first arg)
	(list '*
	      (case (second arg)
		(integer 'integer)
		(char 'char)
		(real-float (lapack-real-float-type type))
		(float (lapack-float-type type))
		(t 't)))))

(defun lapack-parse-wrapper-arg (arg)
  (if (eq (second arg) 'array)
      `(array-sap ,(first arg))
      `(alien-sap (addr ,(first arg)))))

(defmacro define-lapack-routine (name result args)
  (let ((defs nil))
    (dolist (type '(s d c z))
      (let ((fullname (intern (concat-as-strings '% type name) :sb-math))
	    (alien-name (concat-as-strings type name '_)))
	(push `(define-alien-routine (,(string-downcase alien-name) ,fullname) ,result
		 ,@(let ((arglst nil))
		     (dolist (arg args (nreverse arglst))
		       (case (first arg)
			 (complex-only
			    (when (or (eq type 'c) (eq type 'z))
			      (push (lapack-parse-alien-arg (second arg) type) arglst)))
			 (real-only 
			    (when (or (eq type 's) (eq type 'd))
			      (push (lapack-parse-alien-arg (second arg) type) arglst)))
			 (t (push (lapack-parse-alien-arg arg type) arglst))))))
	      defs)))
    `(progn ,@(nreverse defs))))

(defmacro define-lapack-wrapper (name &key return lets array-args rest-args aliens)
  (let ((ext-args (append rest-args array-args))
	(defs nil))
    (dolist (type '(s d c z))
      (let ((alien-name (intern (concat-as-strings '% type name) :sb-math))
	    (fullname (intern (concat-as-strings type name) :sb-math)))
	(push `(defun ,fullname
		   ,(remove nil
		     (mapcar #'(lambda (arg)
				 (if (not (listp arg))
				     arg
				     (ecase (first arg)
				       (real-only
					  (when (or (eq type 's) (eq type 'd))
					    (second arg)))
				       (complex-only
					  (when (or (eq type 'c) (eq type 'z))
					    (second arg))))))
			     ext-args))
		 (let* ,lets
		   (with-alien ,(remove-if
				 #'(lambda (x)
				     (or (member (first x) array-args)
					 (and
					  (eq (first x) 'complex-only)
					  (member (list 'complex-only
							(first (second x)))
						  array-args :test #'equal))
					 (and
					  (eq (first x) 'real-only)
					  (member (list 'real-only
							(first (second x)))
						  array-args :test #'equal))))
				 aliens)
		     (,alien-name ,@(let ((arglst nil))
				      (dolist (a aliens (nreverse arglst))
					(case (first a)
					  (complex-only
					     (when (or (eq type 'c) (eq type 'z))
					       (push (lapack-parse-wrapper-arg (second a)) arglst)))
					  (real-only 
					     (when (or (eq type 's) (eq type 'd))
					       (push (lapack-parse-wrapper-arg (second a)) arglst)))
					  (t (push (lapack-parse-wrapper-arg a) arglst))))))
		     ,return)))
	      defs)))
    `(progn ,@(nreverse defs))))

#|
(defun read-defs (file)
  (with-open-file (s file)
    (let ((defs nil))
      (loop
	(let ((def (read s nil)))
	  (if (null def)
	      (return)
	      (push def defs))))
      (nreverse defs))))

(defun write-alien-defs ()
  (with-open-file (s "general/lapack/aliens.lisp"
		     :direction :output
		     :if-exists :append
		     :if-does-not-exist :create)
    (map nil #'(lambda (def)
		 (format s "~%~%;; ~A" (second def)) 
		 (print (macroexpand-1 def) s))
	 (read-defs "general/lapack/alien-defs.lisp"))))

(defun write-lowlevel-defs ()
  (with-open-file (s "general/lapack/lowlevel.lisp"
		     :direction :output
		     :if-exists :append
		     :if-does-not-exist :create)
    (map nil #'(lambda (def)
		 (format s "~%~%;; ~A" (second def)) 
		 (print (macroexpand-1 def) s))
	 (read-defs "general/lapack/lowlevel-defs.lisp"))))
|#
