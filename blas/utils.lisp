(in-package :sb-math)

(define-alien-type CBLAS_ORDER (enum nil (CblasRowMajor 101) (CblasColMajor 102)))
(define-alien-type CBLAS_TRANSPOSE (enum nil (:notrans 111) (:trans 112) (:conjtrans 113)))
(define-alien-type CBLAS_UPLO (enum nil (:upper 121) (:lower 122)))
(define-alien-type CBLAS_DIAG (enum nil (:nonunit 131) (:unit 132)))
(define-alien-type CBLAS_SIDE (enum nil (:left 141) (:right 142)))
;(define-alien-type CBLAS_INDEX integer)


(defun cblas-alien-name (type name)
  (concat-as-strings (list 'cblas_ type name)))

(defun define-blas-routine (name result &rest args)
  (%define-foreign-routine name result #'cblas-alien-name args))
