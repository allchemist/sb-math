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
  (cond ((eq (second arg) 'array)
	 `(array-sap ,(first arg)))
	((eq (second arg) 'array-integer)
	 `(array-sap ,(first arg)))
	(t `(alien-sap (addr ,(first arg))))))

(defmacro define-lapack-routine (name result args)
  (let ((defs nil))
    (dolist (type '(s d c z))
      (let ((fullname (intern (concat-as-strings (list '% type name)) :sb-math))
	    (alien-name (concat-as-strings (list type name '_))))
	(push `(define-alien-routine (,(string-downcase alien-name) ,fullname) ,result
		 ,@(let ((arglst nil))
		     (dolist (arg args (nreverse arglst))
		       (if (not (eq (first arg) 'complex-only))
			   (push (lapack-parse-alien-arg arg type) arglst)
			   (when (or (eq type 'c) (eq type 'z))
			     (push (lapack-parse-alien-arg (second arg) type) arglst))))))
	      defs)))
    `(progn ,@(nreverse defs))))

(defmacro define-lapack-wrapper (name &key return lets array-args rest-args aliens)
  (let ((ext-args (append rest-args array-args))
	(defs nil))
    (dolist (type '(s d c z))
      (let ((alien-name (intern (concat-as-strings (list '% type name)) :sb-math))
	    (fullname (intern (concat-as-strings (list type name)) :sb-math)))
	(push `(defun ,fullname
		   ,(remove nil
		     (mapcar #'(lambda (arg)
				 (if (not (and (listp arg)
					       (eq (first arg) 'complex-only)))
				     arg
				     (when (or (eq type 'c) (eq type 'z))
				       (second arg))))
			     ext-args))
		 (let* ,lets
		   (with-alien ,(remove-if #'(lambda (x)
					       (or (member (first x) array-args)
						   (eq (second x) 'array-integer)
						   (and
						    (eq (first x) 'complex-only)
						    (or (member (list 'complex-only
								      (first (second x)))
								array-args :test #'equal)
							(eq (second (second x)) 'array-integer)))))
					   aliens)
		     (,alien-name ,@(let ((arglst nil))
				      (dolist (a aliens (nreverse arglst))
					(if (not (eq (first a) 'complex-only))
					    (push (lapack-parse-wrapper-arg a) arglst)
					    (when (or (eq type 'c) (eq type 'z))
					      (push (lapack-parse-wrapper-arg (second a)) arglst))))))
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
