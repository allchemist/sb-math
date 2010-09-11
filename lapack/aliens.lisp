(in-package :sb-math)

(declaim (inline %sgetrf %dgetrf %cgetrf %zgetrf
		 %sgetri %dgetri %cgetri %zgetri
		 %sgetrs %dgetrs %cgetrs %zgetrs
		 %sgesvd %dgesvd %cgesvd %zgesvd
		 %sgeev %dgeev %cgeev %zgeev))


(define-lapack-routine getrf void
  ((m integer) (n integer) (a float) (lda integer) (ipiv ub32) (info integer)))

(define-lapack-routine getri void
  ((n integer) (a float) (lda integer) (ipiv ub32)
   (work float) (lwork integer) (info integer)))

(define-lapack-routine getrs void
  ((trans char) (n integer) (nhrs integer) (a float) (lda integer)
   (ipiv ub32) (b float) (ldb integer) (info integer)))

(define-lapack-routine gesvd void
  ((jobu char) (jobvt char) (m integer) (n integer) (a float) (lda integer)
   (s real-float) (vt float) (ldvt integer) (u float) (ldu integer) (work float)
   (lwork integer) (complex-only (rwork real-float)) (info integer)))

(define-lapack-routine geev void
  ((jobvl char) (jobvr char) (N integer) (A float) (lda integer)
   (real-only (wr float)) (real-only (wl float)) (complex-only (w float))
   (vl float) (ldvl integer) (vr float) (ldvr integer)
   (work float) (lwork integer) (complex-only (rwork real-float)) (info integer)))
