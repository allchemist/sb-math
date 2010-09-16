(in-package :sb-math)

(export
 '(m+c m-c m/c m* m/))

;; only the note "can't tell the rank at compile time" appears
;; so muffle this note for the file
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

(define-with-types m+c (:matrix-args matrix :float-args const)
  (dotimes (i (the fixnum (array-total-size matrix)))
    (incf (row-major-aref matrix i) const)))

(define-with-types m/c (:matrix-args matrix :float-args const)
  (dotimes (i (the fixnum (array-total-size matrix)))
    (setf (row-major-aref matrix i) (/ (row-major-aref matrix i) const))))

(defun m+c (matrix const)
  (float-choice-funcall (array-element-type matrix) m+c nil matrix const))

(defun m-c (matrix const)
  (float-choice-funcall (array-element-type matrix) m+c nil matrix (- const)))

(defun m/c (matrix const)
  (float-choice-funcall (array-element-type matrix) m/c nil matrix const))

(defun m* (m1 m2) (map-two-matrices-* m1 m2))
(defun m/ (m1 m2) (map-two-matrices-/ m1 m2))

(declaim (sb-ext:unmuffle-conditions sb-ext:compiler-note))
