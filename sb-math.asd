(defpackage :sb-math-system
    (:use :common-lisp :asdf))

(in-package :sb-math-system)

(defsystem sb-math
  :description "Mathematics through SBCL alien interface"
  :licence "BSD"
  :version "0.9.2"
  :depends-on ()
  :components
  ((:module base
	    :components
	    ((:file "package")
	     (:file "scalar" :depends-on ("package"))
	     (:file "quaternions" :depends-on ("package" "scalar"))
	     (:file "reader-macro" :depends-on ("package"))
	     (:file "interface" :depends-on ("package"))
	     (:file "matrix" :depends-on ("package" "scalar"))
	     (:file "mapping" :depends-on ("package" "scalar" "matrix"))
	     (:file "typed-matrix" :depends-on ("package" "scalar" "matrix" "mapping"))
	     (:file "packed-matrix" :depends-on ("package" "scalar" "matrix" "mapping" "typed-matrix"))))
   (:module lib
	    :components
	    ((:file "load-libs")))
   (:module blas
	    :depends-on (base lib)
	    :components
	    ((:file "module")
	     (:file "utils" :depends-on ("module"))
	     (:file "aliens" :depends-on ("module" "utils"))
	     (:file "lowlevel" :depends-on ("module" "utils" "aliens"))
	     (:file "highlevel" :depends-on ("module" "utils" "aliens" "lowlevel"))))
   (:module alien-matrix
	    :depends-on (base lib blas)
	    :components
	    ((:file "module")
	     (:file "utils" :depends-on ("module"))
	     (:file "aliens" :depends-on ("module" "utils"))
	     (:file "highlevel" :depends-on ("module" "utils" "aliens"))))
   (:module lapack
	    :depends-on (base lib blas alien-matrix)
	    :components
	    ((:file "module")
	     (:file "utils" :depends-on ("module"))
	     (:file "aliens" :depends-on ("module" "utils"))
	     (:file "lowlevel" :depends-on ("module" "utils" "aliens"))
	     (:file "highlevel" :depends-on ("module" "utils" "aliens" "lowlevel"))))
   (:module applications
	    :depends-on (base lib blas alien-matrix)
	    :components
	    ((:file "module")
	     (:file "matrix-product" :depends-on ("module"))
	     (:file "svd" :depends-on ("module"))))

   (:module tests
	    :depends-on (base lib alien-matrix blas lapack applications)
	    :components
	    ((:file "deftest")))))
