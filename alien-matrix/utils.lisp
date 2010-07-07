(in-package :sb-math)

(defun float-result-type (type-char)
  (ecase type-char
    (s 'single-float)
    (d 'double-float)
    (c 'void)
    (z 'void)))

(defun float-array-type (type-char)
  (ecase type-char
    (s '(* single-float))
    (d '(* double-float))
    (c 'system-area-pointer)
    (z 'system-area-pointer)))

(defun float-number-type (type-char)
  (ecase type-char
    (s 'single-float)
    (d 'double-float)
    (c 'system-area-pointer)
    (z 'system-area-pointer)))

(defun same-alien-name (type name)
  (concat-as-strings (list type name)))

(defmacro %define-foreign-routine (name result alien-name-fn args)
  (let ((defs nil))
    (dolist (type '(s d c z))
      (let ((fullname (intern (concat-as-strings (list '% type name)) :sb-math))
	    (alien-name (funcall alien-name-fn type name)))
	(push `(define-alien-routine (,(string-downcase alien-name) ,fullname)
		   ,(if (eq result 'float) (float-result-type type) result)
		 ,@(let ((arglst nil))
		     (dolist (arg args (nreverse arglst))
		       (push 
			(list (first arg)
			      (cond ((and (listp (second arg))
					  (eq (first (second arg)) '*))
				     (float-array-type type))
				    ((eq (second arg) 'float)
				     (float-number-type type))
				    (t (second arg))))
			arglst))))
	      defs)))
    `(progn ,@(nreverse defs))))

(defmacro define-foreign-routine (name result &rest args)
  `(%define-foreign-routine ,name ,result ,#'same-alien-name ,args))
