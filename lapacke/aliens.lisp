(in-package :sb-math)

(define-lapack-alien getrf
    (m int) (n int) (a (* float)) (lda int) (ipiv (* (unsigned 32))))

(define-lapack-alien getri
    (n int) (a (* float)) (lda int) (ipiv (* (unsigned 32))))

(define-lapack-alien getrs
    (trans char) (n int) (nrhs int) (a (* float)) (lda int)
    (ipiv (* (unsigned 32))) (b (* float)) (ldb int))

(define-lapack-alien gesvd
    (jobu char) (jobvt char) (m int) (n int) (a (* float)) (lda int)
    (s (* float)) (u (* float)) (ldu int) (vt (* float)) (ldvt int) (superb (* float)))

(define-lapack-alien ge_trans
    (m int) (n int) (in (* float)) (ldin int) (out (* float)) (ldout int))
