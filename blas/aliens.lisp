(in-package :sb-math)

(declaim (inline
	   ;; blas 1
           %sdsdot %dsdot %sdot 
           %ddot %cdotu_sub %cdotc_sub 
           %zdotu_sub %zdotc_sub %snrm2 
           %sasum %dnrm2 %dasum 
           %scnrm2 %scasum %dznrm2 
           %dzasum %isamax %idamax 
           %icamax %izamax %sswap 
           %scopy %saxpy %dswap 
           %dcopy %daxpy %cswap 
           %ccopy %caxpy %zswap 
           %zcopy %zaxpy %srotg 
           %srotmg %srot %srotm 
           %drotg %drotmg %drot 
           %drotm %sscal %dscal 
           %cscal %zscal %csscal 
           %zdscal %sgemv %dgemv 
	   ;; blas 2
	   %sgemv %dgemv %cgemv %zgemv
	   %strmv %dtrmv %ctrmv %ztrmv
	   %ssymv %dsymv
	   %ssyr %ssyr2
	   %dsyr %dsyr2
	   %chemv %zhemv
	   %cher %cher2
	   %zher %zher2
	   ;; blas 3
	   %sgemm %dgemm %cgemm %zgemm
	   %ssymm %dsymm %csymm %zsymm
	   %ssyrk %dsyrk %csyrk %zsyrk
	   %ssyr2k %dsyr2k %csyr2k %zsyr2k
	   %strmm %dtrmm %ctrmm %ztrmm
	   %strsm %dtrsm %ctrsm %ztrsm
	   %chemm %cherk %cher2k
	   %zhemm %zherk %zger2k))

(declaim (inline
	   ;; blas 2
	   %stpmv %dtpmv %ctpmv %ztpmv
	   %sspmv %sdpmv
	   %sspr %sspr2
	   %dspr %dspr2
	   %chpmv %zhpmv
	   %chpr %chpr2
	   %zhpr %zhpr2
	   ;; blas 3
	   ))

(eval
 `(progn ,@(map nil #'(lambda (name)
			`(defun ,name (&rest args)
			   (declare (ignore args))))
		'(%CSPMV %CSPR %CSYMV %CSYR %DHEMV %DHER %DHPMV %DHPR
		  %SHEMV %SHER %SHPMV %SHPR %ZSPMV %ZSPR %ZSYMV %ZSYR))))

;;; general
;;; ==============================================================
;;; blas 1

(define-alien-routine ("cblas_sdsdot" %sdsdot) single-float
   (N integer) (alpha single-float) (X (* (array single-float)))
   (incX integer) (Y (* (array single-float))) (incY integer))

(define-alien-routine ("cblas_dsdot" %dsdot) double-float
   (N integer) (X (* (array single-float))) (incX integer)
   (Y (* (array single-float))) (incY integer))

(define-alien-routine ("cblas_sdot" %sdot) single-float
   (N integer) (X (* (array single-float))) (incX integer)
   (Y (* (array single-float))) (incY integer))

(define-alien-routine ("cblas_ddot" %ddot) double-float
   (N integer) (X (* (array double-float))) (incX integer)
   (Y (* (array double-float))) (incY integer))

(define-alien-routine ("cblas_cdotu_sub" %cdotu_sub) void
   (N integer) (X system-area-pointer) (incX integer)
   (Y system-area-pointer) (incY integer) (dotu system-area-pointer))

(define-alien-routine ("cblas_cdotc_sub" %cdotc_sub) void
   (N integer) (X system-area-pointer) (incX integer)
   (Y system-area-pointer) (incY integer) (dotc system-area-pointer))

(define-alien-routine ("cblas_zdotu_sub" %zdotu_sub) void
   (N integer) (X system-area-pointer) (incX integer)
   (Y system-area-pointer) (incY integer) (dotu system-area-pointer))

(define-alien-routine ("cblas_zdotc_sub" %zdotc_sub) void
   (N integer) (X system-area-pointer) (incX integer)
   (Y system-area-pointer) (incY integer) (dotc system-area-pointer))

(define-alien-routine ("cblas_snrm2" %snrm2) single-float
   (N integer) (X (* (array single-float))) (incX integer))

(define-alien-routine ("cblas_dnrm2" %dnrm2) double-float
   (N integer) (X (* (array double-float))) (incX integer))

(define-alien-routine ("cblas_scnrm2" %scnrm2) single-float
   (N integer) (X system-area-pointer) (incX integer))

(define-alien-routine ("cblas_dznrm2" %dznrm2) double-float
   (N integer) (X system-area-pointer) (incX integer))

(define-alien-routine ("cblas_sasum" %sasum) single-float
   (N integer) (X (* (array single-float))) (incX integer))

(define-alien-routine ("cblas_dasum" %dasum) double-float
   (N integer) (X (* (array double-float))) (incX integer))

(define-alien-routine ("cblas_scasum" %scasum) single-float
   (N integer) (X system-area-pointer) (incX integer))

(define-alien-routine ("cblas_dzasum" %dzasum) double-float
   (N integer) (X system-area-pointer) (incX integer))

(define-alien-routine ("cblas_isamax" %isamax) integer
   (N integer) (X (* (array single-float))) (incX integer))

(define-alien-routine ("cblas_idamax" %idamax) integer
   (N integer) (X (* (array double-float))) (incX integer))

(define-alien-routine ("cblas_icamax" %icamax) integer
   (N integer) (X system-area-pointer) (incX integer))

(define-alien-routine ("cblas_izamax" %izamax) integer
   (N integer) (X system-area-pointer) (incX integer))

(define-alien-routine ("cblas_sswap" %sswap) void
   (N integer) (X (* (array single-float))) (incX integer)
   (Y (* (array single-float))) (incY integer))

(define-alien-routine ("cblas_scopy" %scopy) void
   (N integer) (X (* (array single-float))) (incX integer)
   (Y (* (array single-float))) (incY integer))

(define-alien-routine ("cblas_saxpy" %saxpy) void
   (N integer) (alpha single-float) (X (* (array single-float)))
   (incX integer) (Y (* (array single-float))) (incY integer))

(define-alien-routine ("cblas_dswap" %dswap) void
   (N integer) (X (* (array double-float))) (incX integer)
   (Y (* (array double-float))) (incY integer))

(define-alien-routine ("cblas_dcopy" %dcopy) void
   (N integer) (X (* (array double-float))) (incX integer)
   (Y (* (array double-float))) (incY integer))

(define-alien-routine ("cblas_daxpy" %daxpy) void
   (N integer) (alpha double-float) (X (* (array double-float)))
   (incX integer) (Y (* (array double-float))) (incY integer))

(define-alien-routine ("cblas_cswap" %cswap) void
   (N integer) (X system-area-pointer) (incX integer)
   (Y system-area-pointer) (incY integer))

(define-alien-routine ("cblas_ccopy" %ccopy) void
   (N integer) (X system-area-pointer) (incX integer)
   (Y system-area-pointer) (incY integer))

(define-alien-routine ("cblas_caxpy" %caxpy) void
   (N integer) (alpha system-area-pointer) (X system-area-pointer)
   (incX integer) (Y system-area-pointer) (incY integer))

(define-alien-routine ("cblas_zswap" %zswap) void
   (N integer) (X system-area-pointer) (incX integer)
   (Y system-area-pointer) (incY integer))

(define-alien-routine ("cblas_zcopy" %zcopy) void
   (N integer) (X system-area-pointer) (incX integer)
   (Y system-area-pointer) (incY integer))

(define-alien-routine ("cblas_zaxpy" %zaxpy) void
   (N integer) (alpha system-area-pointer) (X system-area-pointer)
   (incX integer) (Y system-area-pointer) (incY integer))

(define-alien-routine ("cblas_srotg" %srotg) void
   (a (* (array single-float))) (b (* (array single-float))) (c (* (array single-float)))
   (s (* (array single-float))))

(define-alien-routine ("cblas_srot" %srot) void
   (N integer) (X (* (array single-float))) (incX integer)
   (Y (* (array single-float))) (incY integer) (c single-float)
   (s single-float))

(define-alien-routine ("cblas_srotm" %srotm) void
   (N integer) (X (* (array single-float))) (incX integer)
   (Y (* (array single-float))) (incY integer) (P (* (array single-float))))

(define-alien-routine ("cblas_drotg" %drotg) void
   (a (* (array double-float))) (b (* (array double-float))) (c (* (array double-float)))
   (s (* (array double-float))))

(define-alien-routine ("cblas_drot" %drot) void
   (N integer) (X (* (array double-float))) (incX integer)
   (Y (* (array double-float))) (incY integer) (c double-float)
   (s double-float))

(define-alien-routine ("cblas_drotm" %drotm) void
   (N integer) (X (* (array double-float))) (incX integer)
   (Y (* (array double-float))) (incY integer) (P (* (array double-float))))

(define-alien-routine ("cblas_sscal" %sscal) void
   (N integer) (alpha single-float) (X (* (array single-float)))
   (incX integer))

(define-alien-routine ("cblas_dscal" %dscal) void
   (N integer) (alpha double-float) (X (* (array double-float)))
   (incX integer))

(define-alien-routine ("cblas_cscal" %cscal) void
   (N integer) (alpha system-area-pointer) (X system-area-pointer)
   (incX integer))

(define-alien-routine ("cblas_zscal" %zscal) void
   (N integer) (alpha system-area-pointer) (X system-area-pointer)
   (incX integer))

;;; ==============================================================
;;; blas 2

;; gemv
(define-alien-routine ("cblas_sgemv" %sgemv) void
   (order CBLAS_ORDER) (TransA CBLAS_TRANSPOSE) (M integer)
   (N integer) (alpha single-float) (A (* (array single-float)))
   (lda integer) (X (* (array single-float))) (incX integer)
   (beta single-float) (Y (* (array single-float))) (incY integer))

(define-alien-routine ("cblas_dgemv" %dgemv) void
   (order CBLAS_ORDER) (TransA CBLAS_TRANSPOSE) (M integer)
   (N integer) (alpha double-float) (A (* (array double-float)))
   (lda integer) (X (* (array double-float))) (incX integer)
   (beta double-float) (Y (* (array double-float))) (incY integer))

(define-alien-routine ("cblas_cgemv" %cgemv) void
   (order CBLAS_ORDER) (TransA CBLAS_TRANSPOSE) (M integer)
   (N integer) (alpha system-area-pointer) (A system-area-pointer)
   (lda integer) (X system-area-pointer) (incX integer)
   (beta system-area-pointer) (Y system-area-pointer) (incY integer))

(define-alien-routine ("cblas_zgemv" %zgemv) void
   (order CBLAS_ORDER) (TransA CBLAS_TRANSPOSE) (M integer)
   (N integer) (alpha system-area-pointer) (A system-area-pointer)
   (lda integer) (X system-area-pointer) (incX integer)
   (beta system-area-pointer) (Y system-area-pointer) (incY integer))

(define-alien-routine ("cblas_sger" %sger) void
  (order CBLAS_ORDER) (M integer) (N integer) (alpha single-float)
  (X (* (array single-float))) (incX integer) (Y (* (array single-float)))
  (incY integer) (A (* (array single-float))) (lda integer))

(define-alien-routine ("cblas_dger" %dger) void
  (order CBLAS_ORDER) (M integer) (N integer) (alpha double-float)
  (X (* (array double-float))) (incX integer) (Y (* (array double-float)))
  (incY integer) (A (* (array double-float))) (lda integer))

(define-alien-routine ("cblas_cgeru" %cgeru) void
  (order CBLAS_ORDER) (M integer) (N integer) (alpha system-area-pointer)
  (X system-area-pointer) (incX integer) (Y system-area-pointer)
  (incY integer) (A system-area-pointer) (lda integer))

(define-alien-routine ("cblas_cgerc" %cgerc) void
  (order CBLAS_ORDER) (M integer) (N integer) (alpha system-area-pointer)
  (X system-area-pointer) (incX integer) (Y system-area-pointer)
  (incY integer) (A system-area-pointer) (lda integer))

(define-alien-routine ("cblas_zgeru" %zgeru) void
  (order CBLAS_ORDER) (M integer) (N integer) (alpha system-area-pointer)
  (X system-area-pointer) (incX integer) (Y system-area-pointer)
  (incY integer) (A system-area-pointer) (lda integer))

(define-alien-routine ("cblas_zgerc" %zgerc) void
  (order CBLAS_ORDER) (M integer) (N integer) (alpha system-area-pointer)
  (X system-area-pointer) (incX integer) (Y system-area-pointer)
  (incY integer) (A system-area-pointer) (lda integer))

;; trmv
(define-alien-routine ("cblas_strmv" %strmv) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE) (diag CBLAS_DIAG)
  (N integer) (A (* (array single-float))) (lda integer) (X (* (array single-float))) (incX integer))

(define-alien-routine ("cblas_dtrmv" %dtrmv) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE) (diag CBLAS_DIAG)
  (N integer) (A (* (array double-float))) (lda integer) (X (* (array double-float))) (incX integer))

(define-alien-routine ("cblas_ctrmv" %ctrmv) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE) (diag CBLAS_DIAG)
  (N integer) (A system-area-pointer) (lda integer)
  (X system-area-pointer) (incX integer))

(define-alien-routine ("cblas_ztrmv" %ztrmv) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE) (diag CBLAS_DIAG)
  (N integer) (A system-area-pointer) (lda integer)
  (X system-area-pointer) (incX integer))

;; symv

(define-alien-routine ("cblas_ssymv" %ssymv) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (N integer) (alpha single-float)
  (A (* (array single-float))) (lda integer) (X (* (array single-float)))
  (incX integer) (beta single-float) (Y (* (array single-float))) (incY integer))

(define-alien-routine ("cblas_dsymv" %dsymv) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (N integer) (alpha double-float)
  (A (* (array double-float))) (lda integer) (X (* (array double-float)))
  (incX integer) (beta double-float) (Y (* (array double-float))) (incY integer))

;; syr

(define-alien-routine ("cblas_ssyr" %ssyr) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (N integer) (alpha single-float)
  (X (* (array single-float))) (incX integer) (A (* (array single-float))) (lda integer))

(define-alien-routine ("cblas_ssyr2" %ssyr2) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (N integer) (alpha single-float)
  (X (* (array single-float))) (incX integer) (Y (* (array single-float)))
  (incY integer) (A (* (array single-float))) (lda integer))

(define-alien-routine ("cblas_dsyr" %dsyr) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (N integer) (alpha double-float)
  (X (* (array double-float))) (incX integer) (A (* (array double-float))) (lda integer))

(define-alien-routine ("cblas_dsyr2" %dsyr2) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (N integer) (alpha double-float)
  (X (* (array double-float))) (incX integer) (Y (* (array double-float)))
  (incY integer) (A (* (array double-float))) (lda integer))

;; hemv

(define-alien-routine ("cblas_chemv" %chemv) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (N integer) (alpha system-area-pointer)
  (A system-area-pointer) (lda integer) (X system-area-pointer)
  (incX integer) (beta system-area-pointer) (Y system-area-pointer) (incY integer))

(define-alien-routine ("cblas_zhemv" %zhemv) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (N integer) (alpha system-area-pointer)
  (A system-area-pointer) (lda integer) (X system-area-pointer)
  (incX integer) (beta system-area-pointer) (Y system-area-pointer) (incY integer))

(define-alien-routine ("cblas_cher" %cher) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (N integer) (alpha system-area-pointer)
  (X system-area-pointer) (incX integer) (A system-area-pointer) (lda integer))

(define-alien-routine ("cblas_cher2" %cher2) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (N integer) (alpha system-area-pointer)
  (X system-area-pointer) (incX integer) (Y system-area-pointer)
  (A system-area-pointer) (lda integer))

(define-alien-routine ("cblas_zher" %zher) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (N integer) (alpha system-area-pointer)
  (X system-area-pointer) (incX integer) (A system-area-pointer) (lda integer))

(define-alien-routine ("cblas_zher2" %zher2) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (N integer) (alpha system-area-pointer)
  (X system-area-pointer) (incX integer) (Y system-area-pointer)
  (A system-area-pointer) (lda integer))


;;; ==============================================================
;;; blas 3

;; gemm
(define-alien-routine ("cblas_sgemm" %sgemm) void
   (Order CBLAS_ORDER) (TransA CBLAS_TRANSPOSE) (TransB CBLAS_TRANSPOSE)
   (M integer) (N integer) (K integer)
   (alpha single-float) (A (* (array single-float))) (lda integer)
   (B (* (array single-float))) (ldb integer) (beta single-float)
   (C (* (array single-float))) (ldc integer))

(define-alien-routine ("cblas_dgemm" %dgemm) void
   (Order CBLAS_ORDER) (TransA CBLAS_TRANSPOSE) (TransB CBLAS_TRANSPOSE)
   (M integer) (N integer) (K integer)
   (alpha double-float) (A (* (array double-float))) (lda integer)
   (B (* (array double-float))) (ldb integer) (beta double-float)
   (C (* (array double-float))) (ldc integer))

(define-alien-routine ("cblas_cgemm" %cgemm) void
   (Order CBLAS_ORDER) (TransA CBLAS_TRANSPOSE) (TransB CBLAS_TRANSPOSE)
   (M integer) (N integer) (K integer)
   (alpha system-area-pointer) (A system-area-pointer) (lda integer)
   (B system-area-pointer) (ldb integer) (beta system-area-pointer)
   (C system-area-pointer) (ldc integer))

(define-alien-routine ("cblas_zgemm" %zgemm) void
   (Order CBLAS_ORDER) (TransA CBLAS_TRANSPOSE) (TransB CBLAS_TRANSPOSE)
   (M integer) (N integer) (K integer)
   (alpha system-area-pointer) (A system-area-pointer) (lda integer)
   (B system-area-pointer) (ldb integer) (beta system-area-pointer)
   (C system-area-pointer) (ldc integer))

;;

(define-alien-routine ("cblas_ssymm" %ssymm) void
  (order CBLAS_ORDER) (side CBLAS_SIDE) (uplo CBLAS_UPLO) (M integer) (N integer)
  (alpha single-float) (A (* (array single-float))) (lda integer)
  (B (* (array single-float))) (ldb integer) (beta single-float)
  (C (* (array single-float))) (ldc integer))

(define-alien-routine ("cblas_ssyrk" %ssyrk) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE) (N integer)
  (K integer) (alpha single-float) (A (* (array single-float))) (lda integer)
  (beta single-float) (C (* (array single-float))) (ldc integer))

(define-alien-routine ("cblas_ssyr2k" %ssyr2k) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE) (N integer)
  (K integer) (alpha single-float) (A (* (array single-float))) (lda integer)
  (B (* (array single-float))) (ldb integer)
  (beta single-float) (C (* (array single-float))) (ldc integer))

(define-alien-routine ("cblas_strmm" %strmm) void
  (order CBLAS_ORDER) (side CBLAS_SIDE) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE)
  (diag CBLAS_DIAG) (M integer) (N integer) (alpha single-float)
  (A (* (array single-float))) (lda integer) (B (* (array single-float))) (ldb integer))

(define-alien-routine ("cblas_strsm" %strsm) void
  (order CBLAS_ORDER) (side CBLAS_SIDE) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE)
  (diag CBLAS_DIAG) (M integer) (N integer) (alpha single-float)
  (A (* (array single-float))) (lda integer) (B (* (array single-float))) (ldb integer))

;; D prefix
(define-alien-routine ("cblas_dsymm" %dsymm) void
  (order CBLAS_ORDER) (side CBLAS_SIDE) (uplo CBLAS_UPLO) (M integer) (N integer)
  (alpha double-float) (A (* (array double-float))) (lda integer)
  (B (* (array double-float))) (ldb integer) (beta double-float)
  (C (* (array double-float))) (ldc integer))

(define-alien-routine ("cblas_dsyrk" %dsyrk) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE) (N integer)
  (K integer) (alpha double-float) (A (* (array double-float))) (lda integer)
  (beta double-float) (C (* (array double-float))) (ldc integer))

(define-alien-routine ("cblas_dsyr2k" %dsyr2k) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE) (N integer)
  (K integer) (alpha double-float) (A (* (array double-float))) (lda integer)
  (B (* (array double-float))) (ldb integer)
  (beta double-float) (C (* (array double-float))) (ldc integer))

(define-alien-routine ("cblas_dtrmm" %dtrmm) void
  (order CBLAS_ORDER) (side CBLAS_SIDE) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE)
  (diag CBLAS_DIAG) (M integer) (N integer) (alpha double-float)
  (A (* (array double-float))) (lda integer) (B (* (array double-float))) (ldb integer))

(define-alien-routine ("cblas_dtrsm" %dtrsm) void
  (order CBLAS_ORDER) (side CBLAS_SIDE) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE)
  (diag CBLAS_DIAG) (M integer) (N integer) (alpha double-float)
  (A (* (array double-float))) (lda integer) (B (* (array double-float))) (ldb integer))

;; C prefix
(define-alien-routine ("cblas_csymm" %csymm) void
  (order CBLAS_ORDER) (side CBLAS_SIDE) (uplo CBLAS_UPLO) (M integer) (N integer)
  (alpha system-area-pointer) (A system-area-pointer) (lda integer)
  (B system-area-pointer) (ldb integer) (beta system-area-pointer)
  (C system-area-pointer) (ldc integer))

(define-alien-routine ("cblas_csyrk" %csyrk) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE) (N integer)
  (K integer) (alpha system-area-pointer) (A system-area-pointer) (lda integer)
  (beta system-area-pointer) (C system-area-pointer) (ldc integer))

(define-alien-routine ("cblas_csyr2k" %csyr2k) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE) (N integer)
  (K integer) (alpha system-area-pointer) (A system-area-pointer) (lda integer)
  (B system-area-pointer) (ldb integer)
  (beta system-area-pointer) (C system-area-pointer) (ldc integer))

(define-alien-routine ("cblas_ctrmm" %ctrmm) void
  (order CBLAS_ORDER) (side CBLAS_SIDE) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE)
  (diag CBLAS_DIAG) (M integer) (N integer) (alpha system-area-pointer)
  (A system-area-pointer) (lda integer) (B system-area-pointer) (ldb integer))

(define-alien-routine ("cblas_ctrsm" %ctrsm) void
  (order CBLAS_ORDER) (side CBLAS_SIDE) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE)
  (diag CBLAS_DIAG) (M integer) (N integer) (alpha system-area-pointer)
  (A system-area-pointer) (lda integer) (B system-area-pointer) (ldb integer))

;; Z prefix
(define-alien-routine ("cblas_zsymm" %zsymm) void
  (order CBLAS_ORDER) (side CBLAS_SIDE) (uplo CBLAS_UPLO) (M integer) (N integer)
  (alpha system-area-pointer) (A system-area-pointer) (lda integer)
  (B system-area-pointer) (ldb integer) (beta system-area-pointer)
  (C system-area-pointer) (ldc integer))

(define-alien-routine ("cblas_zsyrk" %zsyrk) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE) (N integer)
  (K integer) (alpha system-area-pointer) (A system-area-pointer) (lda integer)
  (beta system-area-pointer) (C system-area-pointer) (ldc integer))

(define-alien-routine ("cblas_zsyr2k" %zsyr2k) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE) (N integer)
  (K integer) (alpha system-area-pointer) (A system-area-pointer) (lda integer)
  (B system-area-pointer) (ldb integer)
  (beta system-area-pointer) (C system-area-pointer) (ldc integer))

(define-alien-routine ("cblas_ztrmm" %ztrmm) void
  (order CBLAS_ORDER) (side CBLAS_SIDE) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE)
  (diag CBLAS_DIAG) (M integer) (N integer) (alpha system-area-pointer)
  (A system-area-pointer) (lda integer) (B system-area-pointer) (ldb integer))

(define-alien-routine ("cblas_ztrsm" %ztrsm) void
  (order CBLAS_ORDER) (side CBLAS_SIDE) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE)
  (diag CBLAS_DIAG) (M integer) (N integer) (alpha system-area-pointer)
  (A system-area-pointer) (lda integer) (B system-area-pointer) (ldb integer))

;; C and Z prefix only
(define-alien-routine ("cblas_chemm" %chemm) void
  (order CBLAS_ORDER) (side CBLAS_SIDE) (uplo CBLAS_UPLO) (M integer) (N integer)
  (alpha system-area-pointer) (A system-area-pointer) (lda integer)
  (B system-area-pointer) (ldb integer) (beta system-area-pointer)
  (C system-area-pointer) (ldc integer))

(define-alien-routine ("cblas_cherk" %cherk) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE) (N integer)
  (K integer) (alpha system-area-pointer) (A system-area-pointer) (lda integer)
  (beta system-area-pointer) (C system-area-pointer) (ldc integer))

(define-alien-routine ("cblas_cher2k" %cher2k) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE) (N integer)
  (K integer) (alpha system-area-pointer) (A system-area-pointer) (lda integer)
  (B system-area-pointer) (ldb integer)
  (beta system-area-pointer) (C system-area-pointer) (ldc integer))

(define-alien-routine ("cblas_zhemm" %zhemm) void
  (order CBLAS_ORDER) (side CBLAS_SIDE) (uplo CBLAS_UPLO) (M integer) (N integer)
  (alpha system-area-pointer) (A system-area-pointer) (lda integer)
  (B system-area-pointer) (ldb integer) (beta system-area-pointer)
  (C system-area-pointer) (ldc integer))

(define-alien-routine ("cblas_zherk" %zherk) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE) (N integer)
  (K integer) (alpha system-area-pointer) (A system-area-pointer) (lda integer)
  (beta system-area-pointer) (C system-area-pointer) (ldc integer))

(define-alien-routine ("cblas_zher2k" %zher2k) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE) (N integer)
  (K integer) (alpha system-area-pointer) (A system-area-pointer) (lda integer)
  (B system-area-pointer) (ldb integer)
  (beta system-area-pointer) (C system-area-pointer) (ldc integer))


;;; packed
;;; ==============================================================
;;; blas 2

;; tpmv
(define-alien-routine ("cblas_stpmv" %stpmv) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE) (diag CBLAS_DIAG)
  (N integer) (Ap (* (array single-float))) (X (* (array single-float))) (incX integer))

(define-alien-routine ("cblas_dtpmv" %dtpmv) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE) (diag CBLAS_DIAG)
  (N integer) (Ap (* (array double-float))) (X (* (array double-float))) (incX integer))

(define-alien-routine ("cblas_ctpmv" %ctpmv) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE) (diag CBLAS_DIAG)
  (N integer) (Ap system-area-pointer)
  (X system-area-pointer) (incX integer))

(define-alien-routine ("cblas_ztpmv" %ztpmv) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (transA CBLAS_TRANSPOSE) (diag CBLAS_DIAG)
  (N integer) (Ap system-area-pointer)
  (X system-area-pointer) (incX integer))

;; S and D prefixes only

(define-alien-routine ("cblas_sspmv" %sspmv) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (N integer) (alpha single-float)
  (Ap (* (array single-float))) (X (* (array single-float)))
  (incX integer) (beta single-float) (Y (* (array single-float))) (incY integer))

(define-alien-routine ("cblas_dspmv" %dspmv) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (N integer) (alpha double-float)
  (Ap (* (array double-float))) (X (* (array double-float)))
  (incX integer) (beta double-float) (Y (* (array double-float))) (incY integer))

(define-alien-routine ("cblas_sspr" %sspr) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (N integer) (alpha single-float)
  (X (* (array single-float))) (incX integer) (Ap (* (array single-float))))

(define-alien-routine ("cblas_sspr2" %sspr2) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (N integer) (alpha single-float)
  (X (* (array single-float))) (incX integer) (Y (* (array single-float)))
  (incY integer) (Ap (* (array single-float))))

(define-alien-routine ("cblas_dspr" %dspr) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (N integer) (alpha double-float)
  (X (* (array double-float))) (incX integer) (Ap (* (array single-float))))

(define-alien-routine ("cblas_dspr2" %dspr2) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (N integer) (alpha double-float)
  (X (* (array double-float))) (incX integer) (Y (* (array double-float)))
  (incY integer) (Ap (* (array double-float))))

;; C and Z prefixes only

(define-alien-routine ("cblas_chpmv" %chpmv) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (N integer) (alpha system-area-pointer)
  (Ap system-area-pointer) (X system-area-pointer)
  (incX integer) (beta system-area-pointer) (Y system-area-pointer) (incY integer))

(define-alien-routine ("cblas_zhpmv" %zhpmv) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (N integer) (alpha system-area-pointer)
  (Ap system-area-pointer) (X system-area-pointer)
  (incX integer) (beta system-area-pointer) (Y system-area-pointer) (incY integer))

(define-alien-routine ("cblas_chpr" %chpr) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (N integer) (alpha system-area-pointer)
  (X system-area-pointer) (incX integer) (A system-area-pointer))

(define-alien-routine ("cblas_chpr2" %chpr2) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (N integer) (alpha system-area-pointer)
  (X system-area-pointer) (incX integer) (Y system-area-pointer)
  (Ap system-area-pointer))

(define-alien-routine ("cblas_zhpr" %zhpr) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (N integer) (alpha system-area-pointer)
  (X system-area-pointer) (incX integer) (A system-area-pointer))

(define-alien-routine ("cblas_zhpr2" %zhpr2) void
  (order CBLAS_ORDER) (uplo CBLAS_UPLO) (N integer) (alpha system-area-pointer)
  (X system-area-pointer) (incX integer) (Y system-area-pointer)
  (Ap system-area-pointer))
