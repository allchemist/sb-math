;(defpackage :sb-math-tests
;    (:use :common-lisp :sb-math)
;  (:export :run-tests :run-all-tests))

(in-package :sb-math)

(export '(run-tests run-all-tests))

(defparameter *path-to-tests*
  (concatenate 'string
	       *root-path*
	       "tests/"))

(defparameter *passed* nil)
(defparameter *asserted* nil)
(defparameter *eps-single* 1.e-5)
(defparameter *eps-double* 1.e-13)

(defun define-test (name test compare &key (predicate #'~=) eps)
  (let ((exit (funcall predicate test compare eps)))
    (if exit
	(progn
	  (push name  *passed*)
	  (format *query-io* "; Test ~A passed~%" name))
	(progn
	  (push name *asserted*)
	  (format *query-io* "; Test ~A asserted~%" name)
	  (format *query-io* "; Difference between~%")
	  (print-matrix test
			:prec (float-type-choice (array-element-type test) 7 16 7 16)
			:dest *query-io*)
	  (format *query-io* "~%and~%")
	  (print-matrix compare
			:prec (float-type-choice (array-element-type compare) 7 16 7 16)
			:dest *query-io*)
	  (format *query-io* "~% is more than epsilon: ~A~%" eps)))))	  

(defun run-tests (&rest tests)
  (flet ((test-file (test)
	   (let ((file (concatenate 'string
				    *path-to-tests* 
				    (string-downcase (string test))
				    "-tests.lisp")))
	     (if (probe-file file) file (error "File ~A not found" file)))))
    (setf *passed* nil
	  *asserted* nil)
    (map nil #'load (mapcar #'test-file tests))
    (format *query-io* "; ~A units passed~%" (length *passed*))
    (format *query-io* "; ~A units asserted~%" (length *asserted*))
    (format *query-io* "; These units asserted: ~A~%" (or *asserted* "none"))))

(defun run-all-tests ()
  (run-tests :alien-matrix :blas :lapack))
