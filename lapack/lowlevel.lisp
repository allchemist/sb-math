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
  :aliens ((n integer dim) (a array) (lda integer dim) (ipiv array)
	   (work array) (lwork integer (dim0 work)) (info integer)))


(define-lapack-wrapper getrs
    :return info
  :lets ((dim (dim0 A)))
  :array-args (A B ipiv)
  :rest-args (trans)
  :aliens ((tr char trans) (n integer dim) (nhrs integer (if (vectorp B) 1 (dim0 B)))
	   (A array) (lda integer dim) (ipiv array) (B array)
	   (ldb integer dim) (info integer)))

#|
;not for complex float data
(define-lapack-wrapper gesvd
    :return info
  :lets ((dim0 (dim1 A)) (dim1 (dim0 A)))
  :array-args (A S VT U work (complex-only rwork))
  :rest-args (jobu jobvt)
  :aliens ((ju char jobu) (jvt char jobvt)
	   (m integer dim0) (n integer dim1)
	   (A array) (lda integer dim0) (S array) (u array)
	   (ldu integer (dim0 u)) (vt array) (ldvt integer (dim0 vt)) (work array)
	   (lwork integer (dim0 work)) (complex-only (rwork array)) (info integer 0)))
|#

(define-lapack-wrapper gesvd
    :return info
  :lets ((dim0 (dim0 A)) (dim1 (dim1 A)))
  :array-args (A S U VT work (complex-only rwork))
  :rest-args (jobu jobvt)
  :aliens ((ju char jobu) (jvt char jobvt)
	   (m integer dim0) (n integer dim1)
	   (A array) (ld0 integer dim0) (S array) (u array)
	   (ldu integer (dim1 u)) (vt array) (ldvt integer (dim1 vt)) (work array)
	   (lwork integer (dim0 work)) (complex-only (rwork array)) (info integer 0)))

