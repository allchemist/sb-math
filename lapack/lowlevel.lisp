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
  :lets ((dim0 (dim1 A)) (dim1 (dim0 A)))
  :array-args (A S U VT work (complex-only rwork))
  :rest-args (jobu jobvt)
  :aliens ((ju char jobu) (jvt char jobvt)
	   (m integer dim0) (n integer dim1)
	   (A array) (lda integer dim0) (S array) (u array)
	   (ldu integer (dim0 u)) (vt array) (ldvt integer (dim0 vt)) (work array)
	   (lwork integer (dim0 work)) (complex-only (rwork array)) (info integer 0)))

(define-lapack-wrapper geev
    :return info
  :lets ((dim (dim0 A)))
  :array-args (A (real-only wr) (real-only wi) (complex-only w) vl vr work (complex-only rwork))
  :rest-args (jobvl jobvr)
  :aliens ((jvl char jobvl) (jvr char jobvr) (N integer dim) (A array) (lda integer dim)
	   (real-only (wr array)) (real-only (wi array)) (complex-only (w array))
	   (vl array) (ldvl integer (dim0 vl)) (vr array) (ldvr integer (dim0 vr))
	   (work array) (lwork integer (dim0 work)) (complex-only (rwork array))
	   (info integer)))
