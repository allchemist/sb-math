(in-package :sb-math)

(define-lapack-routine getrf void
  ((m integer) (n integer) (a float) (lda integer) (ipiv ub32) (info integer)))

(define-lapack-routine getri void
  ((n integer) (a float) (lda integer) (ipiv ub32)
   (work float) (lwork integer) (info integer)))

(define-lapack-routine getrs void
  ((trans char) (n integer) (nhrs integer) (a float) (lda integer)
   (ipiv ub32) (b float) (ldb integer) (info integer)))

#|
;; the same problem as in lowlevel.lisp
;; temprorary decision is using macroexpand

(define-lapack-routine gesvd void
  ((jobu char) (jobvt char) (m integer) (n integer) (a array) (lda integer)
   (s array) (vt array) (ldvt integer) (u array) (ldu integer) (work array)
   (lwork integer) (info integer)))
|#

(PROGN
 (DEFINE-ALIEN-ROUTINE ("sgesvd_" %SGESVD) void (JOBU (* CHAR))
                       (JOBVT (* CHAR)) (M (* INTEGER)) (N (* INTEGER))
                       (A (* (ARRAY SINGLE-FLOAT))) (LDA (* INTEGER))
                       (S (* (ARRAY SINGLE-FLOAT)))
                       (U (* (ARRAY SINGLE-FLOAT))) (LDU (* INTEGER))
                       (VT (* (ARRAY SINGLE-FLOAT))) (LDVT (* INTEGER))
                       (WORK (* (ARRAY SINGLE-FLOAT))) (LWORK (* INTEGER))
                       (INFO (* INTEGER)))
 (DEFINE-ALIEN-ROUTINE ("dgesvd_" %DGESVD) void (JOBU (* CHAR))
                       (JOBVT (* CHAR)) (M (* INTEGER)) (N (* INTEGER))
                       (A (* (ARRAY DOUBLE-FLOAT))) (LDA (* INTEGER))
                       (S (* (ARRAY DOUBLE-FLOAT)))
                       (U (* (ARRAY DOUBLE-FLOAT))) (LDU (* INTEGER))
                       (VT (* (ARRAY DOUBLE-FLOAT))) (LDVT (* INTEGER))
                       (WORK (* (ARRAY single-FLOAT))) (LWORK (* INTEGER))
                       (INFO (* INTEGER)))
 (DEFINE-ALIEN-ROUTINE ("cgesvd_" %CGESVD) void (JOBU (* CHAR))
                       (JOBVT (* CHAR)) (M (* INTEGER)) (N (* INTEGER))
                       (A (* t)) (LDA (* INTEGER))
                       (S (* (ARRAY single-float)))
                       (U (* t)) (LDU (* INTEGER))
                       (VT (* t)) (LDVT (* INTEGER))
                       (WORK (* t))
                       (LWORK (* INTEGER)) (rwork (* (array single-float))) (INFO (* INTEGER)))
 (DEFINE-ALIEN-ROUTINE ("zgesvd_" %ZGESVD) void (JOBU (* CHAR))
                       (JOBVT (* CHAR)) (M (* INTEGER)) (N (* INTEGER))
                       (A (* t)) (LDA (* INTEGER))
                       (S (* (ARRAY double-float)))
                       (U (* t)) (LDU (* INTEGER))
                       (VT (* t)) (LDVT (* INTEGER))
                       (WORK (* t))
                       (LWORK (* INTEGER)) (rwork (* (array double-float))) (INFO (* INTEGER)))) 
