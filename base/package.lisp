(in-package :sb-math-system)

(macrolet
    ((without-package-variance-warnings (&body body)
       `(eval-when (:compile-toplevel :load-toplevel :execute)
	  (handler-bind (#+sbcl(sb-int:package-at-variance #'muffle-warning))
	    ,@body))))
  (without-package-variance-warnings
   (defpackage :sb-math
       (:use :sb-math-system :common-lisp :sb-alien)
     (:nicknames :mth))))
