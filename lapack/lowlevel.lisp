(in-package :sb-math)

(define-lapack-wrapper getrf
    :return info
  :lets ((dim (dim0 A)))
  :array-args (A ipiv)
  :aliens ((m integer dim) (n integer dim) (a array) (lda integer dim)
	   (ipiv array) (info integer 0)))


(define-lapack-wrapper getri
    :return info
  :lets ((dim (dim0 A)))
  :array-args (A ipiv work)
  :aliens ((n integer dim) (a array) (lda integer dim) (ipiv array-integer)
	   (work array) (lwork integer (dim0 work)) (info integer)))

