(in-package :sb-math2-system)

(macrolet
    ((without-package-variance-warnings (&body body)
       `(eval-when (:compile-toplevel :load-toplevel :execute)
	  (handler-bind (#+sbcl(sb-int:package-at-variance #'muffle-warning))
	    ,@body))))
  (without-package-variance-warnings
   (defpackage :sb-math2
       (:use :sb-math2-system :common-lisp :sb-alien)
     (:nicknames :mth2))))

(in-package :sb-math2)
#|
(export
 '(
   ;; scalar.lisp
   in-type
   *default-type*
   square
   !
   simple-rng
   plain-rng
   ~=
   ;; matrix.lisp
   rank2-array-p
   general-matrix
   dim0
   dim1
   make-matrix
   make-matrix-like
   do-matrix
   row-bind
   do-rows
   map-matrix
   map-two-matrices
   make-random-matrix
   print-matrix
   square-matrix-p
   do-upper-triangle
   do-lower-triangle
   mirror-upper-triangle
   mirror-lower-triangle
   mirror-upper-hermitian-triangle
   mirror-lower-hermitian-triangle
   do-diag
   diag
   packed-matrix
   diagonal-matrix
   triangular-matrix
   symmetric-matrix
   hermitian-matrix
   pmatrix-storage-vector
   ldm
   ncols
   upper?
   pmatrix-element-type
   make-pmatrix
   map-pmatrix
   map-two-pmatrices
   make-random-pmatrix
   unpack-pmatrix
   ;; quaternions.lisp
   *quaternion-type*
   make-zero-q
   make-identity-q
   make-rotation-q
   q+
   q-
   q*
   q*c
   q-norm
   q-magnitude
   q-prod
   q-conjugate
   q-invert
   q-rotate-vector

))
|#
