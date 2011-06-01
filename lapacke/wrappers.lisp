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

(defun float-number-type (type-char)
  (ecase type-char
    (s '(* single-float))
    (d '(* double-float))
    (c 'system-area-pointer)
    (z 'system-area-pointer)))

(defmacro define-lapack-alien (name &rest args)
  (let (defs)
    (dolist (type '(s d c z))
      (let ((fullname (intern (concat-as-strings '% type name) :sb-math))
	    (alien-name (concat-as-strings "LAPACKE_"
				     (string-downcase (concat-as-strings type name)))))
	(push `(declaim (inline ,fullname)) defs)
	(push `(define-alien-routine (,alien-name ,fullname) int
		 ,@(let (arglst)
		     (push `(matrix_order int) arglst)
		     (dolist (arg args (nreverse arglst))
		       (let ((argtyp (second arg)))
			 (push (list (first arg)
				     (cond ((and (listp argtyp)
						 (eq (first argtyp) '*)
						 (eq (second argtyp) 'float))
					    (float-number-type type))
					   (t argtyp)))
			       arglst)))))
	      defs)))
    `(progn ,@(nreverse defs))))
