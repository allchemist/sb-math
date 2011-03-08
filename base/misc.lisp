(in-package :sb-math)

;; misc utils
;; kludges and bicycles inside

(export
 '(*default-type* in-type square random-value
   ! simple-rng plain-rng ~= +e+ linspace))

(defparameter *default-type* 'single-float)

(defconstant +e+ (exp 1))

(defun in-type (num) (coerce num *default-type*))
(defun square (x) (* x x))
(defun random-value (max-value)
  (- max-value (random (* max-value 2))))

(defun concat-as-strings (&rest args)
  (apply #'concatenate 'string (mapcar #'string (remove nil args))))

;; factorial

(declaim (ftype (function (fixnum fixnum) fixnum) %!))
(defun %! (n acc)
  (declare (type fixnum n acc)
	   (optimize speed (safety 0)))
  (if (<= n 1)
      acc
      (%! (- n 1) (* acc n))))
(declaim (ftype (function (fixnum) fixnum) !))
(defun ! (n) (%! n 1))

;; simple random generators

(defun simple-rng (x)
  (etypecase x
    ((complex single-float) (complex (random 1.0) (random 1.0)))
    ((complex double-float) (complex (random 1d0) (random 1d0)))
    ((complex number) (complex (random 10) (random 10)))
    (single-float (random 1.0))
    (double-float (random 1d0))
    (number (random 10))))

(defun plain-rng (left right &key (element-type *default-type*))
  (flet ((mod-fn (x) (+ (* x (- right left)) left)))
    (cond ((eq element-type 'single-float) (mod-fn (random 1.0)))
	  ((eq element-type 'double-float) (mod-fn (random 1d0)))
	  ((equal element-type '(complex single-float))
	   (complex (mod-fn (random 1.0)) (mod-fn (random 1.0))))
	  ((equal element-type '(complex double-float))
	   (complex (mod-fn (random 1d0)) (mod-fn (random 1d0))))
	  (t (error "element-type is not float")))))

;; approx equality
						      
(defgeneric ~= (X Y eps))

(defmethod ~= ((X real) (Y real) eps)
  (< (abs (- X Y)) eps))

(defmethod ~= ((X complex) (Y complex) eps)
  (and (< (abs (- (realpart X) (realpart Y))) eps)
       (< (abs (- (imagpart X) (imagpart Y))) eps)))

(defun linspace (xmin xmax points)
  (let* ((vec (make-matrix points))
	 (type (array-element-type vec)))
    (loop for idx from 0 below points
	  for val from xmin to xmax by (/ (- xmax xmin) (1- points))
	  do (setf (aref vec idx) (coerce val type)))
    vec))
