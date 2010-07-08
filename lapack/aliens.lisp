(in-package :sb-math)

(define-lapack-routine getrf void
  ((m integer) (n integer) (a float) (lda integer) (ipiv ub32) (info integer)))

(define-lapack-routine getri void
  ((n integer) (a float) (lda integer) (ipiv ub32)
   (work float) (lwork integer) (info integer)))

(define-lapack-routine getrs void
  ((trans char) (n integer) (nhrs integer) (a float) (lda integer)
   (ipiv ub32) (b float) (ldb integer) (info integer)))
