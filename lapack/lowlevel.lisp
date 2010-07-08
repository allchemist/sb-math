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
;; a small fix in generated code needed,
;; so, macroexpand is welcome
(define-lapack-wrapper gesvd
    :return info
  :lets ((dim0 (dim1 A)) (dim1 (dim0 A)))
  :array-args (A S VT U work)
  :rest-args (jobu jobvt)
  :aliens ((ju char jobu) (jvt char jobvt)
	    (m integer dim0) (n integer dim1)
	    (A array) (lda integer dim0) (S array) (u array)
	    (ldu integer (dim0 u)) (vt array) (ldvt integer (dim0 vt)) (work array)
	    (lwork integer (dim0 work)) (info integer 0)))
|#

(PROGN
 (DEFUN SGESVD (JU JVT A S VT U WORK)
   (LET* ((DIM0 (DIM1 A)) (DIM1 (DIM0 A)))
     (WITH-ALIEN ((JOBU CHAR JU) (JOBVT CHAR JVT) (M INTEGER DIM0)
                  (N INTEGER DIM1) (LDA INTEGER DIM0) (LDU INTEGER (DIM0 U))
                  (LDVT INTEGER (DIM0 VT)) (LWORK INTEGER (DIM0 WORK))
                  (INFO INTEGER 0))
       (%SGESVD (ALIEN-SAP (ADDR JOBU)) (ALIEN-SAP (ADDR JOBVT))
                (ALIEN-SAP (ADDR M)) (ALIEN-SAP (ADDR N)) (ARRAY-SAP A)
                (ALIEN-SAP (ADDR LDA)) (ARRAY-SAP S) (ARRAY-SAP U)
                (ALIEN-SAP (ADDR LDU)) (ARRAY-SAP VT) (ALIEN-SAP (ADDR LDVT))
                (ARRAY-SAP WORK) (ALIEN-SAP (ADDR LWORK))
                (ALIEN-SAP (ADDR INFO)))
       INFO)))
 (DEFUN DGESVD (JU JVT A S VT U WORK)
   (LET* ((DIM0 (DIM1 A)) (DIM1 (DIM0 A)))
     (WITH-ALIEN ((JOBU CHAR JU) (JOBVT CHAR JVT) (M INTEGER DIM0)
                  (N INTEGER DIM1) (LDA INTEGER DIM0) (LDU INTEGER (DIM0 U))
                  (LDVT INTEGER (DIM0 VT)) (LWORK INTEGER (DIM0 WORK))
                  (INFO INTEGER 0))
       (%DGESVD (ALIEN-SAP (ADDR JOBU)) (ALIEN-SAP (ADDR JOBVT))
                (ALIEN-SAP (ADDR M)) (ALIEN-SAP (ADDR N)) (ARRAY-SAP A)
                (ALIEN-SAP (ADDR LDA)) (ARRAY-SAP S) (ARRAY-SAP U)
                (ALIEN-SAP (ADDR LDU)) (ARRAY-SAP VT) (ALIEN-SAP (ADDR LDVT))
                (ARRAY-SAP WORK) (ALIEN-SAP (ADDR LWORK))
                (ALIEN-SAP (ADDR INFO)))
       INFO)))
 (DEFUN CGESVD (JU JVT A S VT U WORK rwork)
   (LET* ((DIM0 (DIM1 A)) (DIM1 (DIM0 A)))
     (WITH-ALIEN ((JOBU CHAR JU) (JOBVT CHAR JVT) (M INTEGER DIM0)
                  (N INTEGER DIM1) (LDA INTEGER DIM0) (LDU INTEGER (DIM0 U))
                  (LDVT INTEGER (DIM0 VT)) (LWORK INTEGER (DIM0 WORK))
                  (INFO INTEGER 0))
       (%CGESVD (ALIEN-SAP (ADDR JOBU)) (ALIEN-SAP (ADDR JOBVT))
                (ALIEN-SAP (ADDR M)) (ALIEN-SAP (ADDR N)) (ARRAY-SAP A)
                (ALIEN-SAP (ADDR LDA)) (ARRAY-SAP S) (ARRAY-SAP U)
                (ALIEN-SAP (ADDR LDU)) (ARRAY-SAP VT) (ALIEN-SAP (ADDR LDVT))
                (ARRAY-SAP WORK) (ALIEN-SAP (ADDR LWORK)) (array-sap rwork)
                (ALIEN-SAP (ADDR INFO)))
       INFO)))
 (DEFUN ZGESVD (JU JVT A S VT U WORK rwork)
   (LET* ((DIM0 (DIM1 A)) (DIM1 (DIM0 A)))
     (WITH-ALIEN ((JOBU CHAR JU) (JOBVT CHAR JVT) (M INTEGER DIM0)
                  (N INTEGER DIM1) (LDA INTEGER DIM0) (LDU INTEGER (DIM0 U))
                  (LDVT INTEGER (DIM0 VT)) (LWORK INTEGER (DIM0 WORK))
                  (INFO INTEGER 0))
       (%ZGESVD (ALIEN-SAP (ADDR JOBU)) (ALIEN-SAP (ADDR JOBVT))
                (ALIEN-SAP (ADDR M)) (ALIEN-SAP (ADDR N)) (ARRAY-SAP A)
                (ALIEN-SAP (ADDR LDA)) (ARRAY-SAP S) (ARRAY-SAP U)
                (ALIEN-SAP (ADDR LDU)) (ARRAY-SAP VT) (ALIEN-SAP (ADDR LDVT))
                (ARRAY-SAP WORK) (ALIEN-SAP (ADDR LWORK)) (array-sap rwork)
                (ALIEN-SAP (ADDR INFO)))
       INFO)))) 
