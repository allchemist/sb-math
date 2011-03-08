(defpackage :sb-math-system
    (:use :common-lisp :asdf))

(in-package :sb-math-system)

(defsystem sb-math
  :description "Mathematics through SBCL alien interface"
  :licence "BSD"
  :version "1.1"
  :depends-on ()
  :serial t
  :components
  ((:module base
	    :serial t
	    :components
	    ((:file "package")
	     (:file "misc")
	     (:file "quaternions")
	     (:file "reader")
	     (:file "interface")
	     (:file "wrappers")
	     (:file "mapping")
	     (:file "matrix")
	     (:file "slicing")
	     (:file "scalar")
	     (:file "elementwize")
	     (:file "permutation")))
   (:module lib
	    :components
	    ((:file "load-libs")))
   (:module pppack
	    :serial t
	    :components
	    ((:file "module")
	     (:file "lowlevel")
	     (:file "highlevel")))
   (:module blas
	    :serial t
	    :components
	    ((:file "module")
	     (:file "wrappers")
	     (:file "aliens")
	     (:file "lowlevel")
	     (:file "highlevel")))
   (:module lapack
	    :serial t
	    :components
	    ((:file "module")
	     (:file "wrappers")
	     (:file "aliens")
	     (:file "lowlevel")
	     (:file "highlevel")))
   (:module applications
	    :serial t
	    :components
	    ((:file "module")
	     (:file "matrix-product")
	     (:file "svd")))))
#|   (:module tests
	    :depends-on (base lib blas lapack)
	    :components
	    ((:file "deftest")))))
	     
|#

