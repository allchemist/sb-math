(defpackage :sb-math-system
    (:use :common-lisp :asdf))

(in-package :sb-math-system)

(defsystem sb-math
  :description "Mathematics through SBCL alien interface"
  :licence "BSD"
  :version "1.1"
  :depends-on ()
  :components
  ((:module base
	    :components
	    ((:file "package")
	     (:file "misc" :depends-on ("package"))
	     (:file "quaternions" :depends-on ("package" "misc"))
	     (:file "reader" :depends-on ("package"))
	     (:file "interface" :depends-on ("package" "misc"))
	     (:file "wrappers" :depends-on ("package" "misc"))
	     (:file "mapping" :depends-on ("package" "misc" "wrappers"))
	     (:file "matrix" :depends-on ("package" "misc" "wrappers"))
	     (:file "slicing" :depends-on ("package" "misc" "wrappers"))
	     (:file "scalar" :depends-on ("package" "misc" "wrappers"))
	     (:file "elementwize" :depends-on ("package" "misc" "wrappers" "mapping"))
	     (:file "permutation" :depends-on ("package" "misc" "wrappers"))))
   (:module lib
	    :depends-on (base)
	    :components
	    ((:file "load-libs")))
   (:module pppack
	    :depends-on (base lib)
	    :serial t
	    :components
	    ((:file "module")
	     (:file "lowlevel")
	     (:file "highlevel")))
   (:module blas
	    :depends-on (base lib)
	    :components
	    ((:file "module")
	     (:file "wrappers" :depends-on ("module"))
	     (:file "aliens" :depends-on ("module" "wrappers"))
	     (:file "lowlevel" :depends-on ("module" "wrappers" "aliens"))
	     (:file "highlevel" :depends-on ("module" "wrappers" "aliens" "lowlevel"))))
   (:module lapack
	    :depends-on (base lib blas)
	    :components
	    ((:file "module")
	     (:file "wrappers" :depends-on ("module"))
	     (:file "aliens" :depends-on ("module" "wrappers"))
	     (:file "lowlevel" :depends-on ("module" "wrappers" "aliens"))
	     (:file "highlevel" :depends-on ("module" "wrappers" "aliens" "lowlevel"))))
   (:module applications
	    :depends-on (base lib blas lapack)
	    :components
	    ((:file "module")
	     (:file "matrix-product" :depends-on ("module"))
	     (:file "svd" :depends-on ("module"))))
#|   (:module tests
	    :depends-on (base lib blas lapack)
	    :components
	    ((:file "deftest")))))
	     
|#
))
