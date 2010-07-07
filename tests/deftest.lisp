;(defpackage :sb-math-tests
;    (:use :common-lisp :sb-math)
;  (:export :run-tests :run-all-tests))

(in-package :sb-math)

(defparameter *path-to-tests*
  (concatenate 'string
	       (namestring (asdf:component-relative-pathname
			    (asdf:find-system :sb-math)))
	       "tests/"))

(defparameter *passed* nil)
(defparameter *asserted* nil)
(defparameter *eps-single* 1.e-6)
(defparameter *eps-double* 1.e-14)

(defun define-test (name test compare &key (predicate #'~=) eps)
  (let ((exit (funcall predicate test compare eps)))
    (if exit
	(progn
	  (push name  *passed*)
	  (format *query-io* "; Test ~A passed~%" name))
	(progn
	  (push name *asserted*)
	  (format *query-io* "; Test ~A asserted~%" name)
	  (format *query-io* "; Difference between ~%~A~%and~%~A~% is more than epsilon: ~A~%"
		  test compare eps)))))

(defun run-tests (&rest tests)
  (flet ((test-file (test)
	   (let ((file (concatenate 'string *path-to-tests* test)))
	     (if (probe-file file) file (error "File ~A not found" file)))))
    (setf *passed* nil
	  *asserted* nil)
    (map nil #'load (mapcar #'test-file tests))
    (format *query-io* "; ~A units passed~%" (length *passed*))
    (format *query-io* "; ~A units asserted~%" (length *asserted*))
    (format *query-io* "; These units asserted: ~A~%" (or *asserted* "none"))))

(defun run-all-tests ()
  (run-tests "tests-single.lisp" "tests-double.lisp"
	     "tests-complex-single.lisp" "tests-complex-double.lisp"))
