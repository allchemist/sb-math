(in-package :sb-math)

;; in-place transposition

(define-foreign-routine trans void
  (array (* float)) (dest (* float)) (dim0 integer) (dim1 integer))

;; column slicing
(define-foreign-routine col void
  (matrix (* float)) (col (* float))
  (dim0 integer) (dim1 integer) (index integer))

(define-foreign-routine setcol void
  (matrix (* float)) (col (* float))
  (dim0 integer) (dim1 integer) (index integer))

;; row slicing
(define-foreign-routine row void
  (matrix (* float)) (row (* float))
  (dim0 integer) (dim1 integer) (index integer))

(define-foreign-routine setrow void
  (matrix (* float)) (row (* float))
  (dim0 integer) (dim1 integer) (index integer))

;; row permuting

(define-foreign-routine rowperm void
  (matrix (* float)) (perm (* (unsigned 32)))
  (dest (* float)) (dim0 integer) (dim1 integer))

;; submatrix

(define-foreign-routine submatrix void
  (matrix (* float)) (dest (* float))
  (dim0 integer) (dim1 integer) (x_start integer) (y_start integer)
  (sub-dim0 integer) (sub-dim1 integer))

(define-foreign-routine setsubmatrix void
  (matrix (* float)) (source (* float))
  (dim0 integer) (dim1 integer) (x_start integer) (y_start integer)
  (sub-dim0 integer) (sub-dim1 integer))

;; missing elementwize operrations

(define-foreign-routine mplusc void
  (matrix (* float)) (const float) (size integer))

(define-foreign-routine mmult void
  (m1 (* float)) (m2 (* float)) (size integer))

;; missing stats operations

(define-foreign-routine imin integer
  (array (* float)) (size integer))

(define-foreign-routine imax integer
  (array (* float)) (size integer))

(define-foreign-routine iamin integer
  (array (* float)) (size integer))

(define-foreign-routine msum float
  (array (* float)) (size integer))
