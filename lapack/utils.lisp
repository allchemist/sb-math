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

(defmacro define-lapack-routine (name result args)
  (let ((defs nil))
    (dolist (type '(s d c z))
      (let ((fullname (intern (concat-as-strings (list '% type name)) :sb-math))
	    (alien-name (concat-as-strings (list type name '_))))
	(push `(define-alien-routine (,(string-downcase alien-name) ,fullname) ,result
		 ,@(let ((arglst nil))
		     (dolist (arg args (nreverse arglst))
		       (push 
			(list (first arg)
			      (list '*
				    (case (second arg)
				      (integer 'integer)
				      (char 'char)
				      (float (lapack-float-type type))
				      (t 't))))
			arglst))))
	      defs)))
    `(progn ,@(nreverse defs))))

(defmacro define-lapack-wrapper (name &key return lets array-args rest-args aliens)
  (let ((ext-args (append rest-args array-args))
	(defs nil))
    (dolist (type '(s d c z))
      (let ((alien-name (intern (concat-as-strings (list '% type name)) :sb-math))
	    (fullname (intern (concat-as-strings (list type name)) :sb-math)))
	(push `(defun ,fullname ,ext-args
		 (let* ,lets
		   (with-alien ,(remove-if #'(lambda (x) (or (member (first x) array-args)
							     (eq (second x) 'array-integer)))
					   aliens) 
		     (,alien-name ,@(let ((arglst nil))
				      (dolist (a aliens (nreverse arglst))
					(push
					 (cond ((eq (second a) 'array)
						`(array-sap ,(first a)))
					       ((eq (second a) 'array-integer)
						`(array-sap ,(first a)))
					       (t `(alien-sap (addr ,(first a)))))
					 arglst))))
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
