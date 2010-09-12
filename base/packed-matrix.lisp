(in-package :sb-math)


;; packed matrices

;; types

(defclass packed-matrix ()
  ((storage-vector
    :initarg :storage-vector
    :accessor pmatrix-storage-vector)
   (ldm
    :initarg :ldm
    :accessor ldm)))

(defclass diagonal-matrix (packed-matrix)
  ((ncols
    :initarg :ncols
    :accessor ncols)))

(defclass triangular-matrix (packed-matrix)
  ((upper?
    :initarg :upper?
    :accessor upper?)))

(defclass symmetric-matrix (packed-matrix)
  ((upper?
    :initarg :upper?
    :accessor upper?)))

(defclass hermitian-matrix (packed-matrix)
  ((upper?
    :initarg :upper?
    :accessor upper?)))

(defun pmatrix-element-type (pmatrix)
  (array-element-type (pmatrix-storage-vector pmatrix)))

;; creating

(defun make-pmatrix (dimensions &key
		     (element-type *default-type*)
		     (pack-type :general)
		     (upper? t))
  (let ((ldm (first dimensions)))
    (ecase pack-type
      (:general (make-matrix dimensions :element-type element-type))
      (:diagonal
	 (make-instance 'diagonal-matrix
			:storage-vector (make-matrix (apply #'min dimensions)
						     :element-type element-type)
		      :ldm ldm
		      :ncols (second dimensions)))
      (:triangular
	 (progn
	   (assert (apply #'= dimensions) nil "Triangular matrix should be square")
	   (make-instance 'triangular-matrix
			  :storage-vector (make-matrix (/ (* ldm (1+ ldm)) 2)
						       :element-type element-type)
			  :ldm ldm
			  :upper? upper?)))
      (:symmetric
	 (progn
	   (assert (apply #'= dimensions) nil "Symmetric matrix should be square")
	   (make-instance 'symmetric-matrix
			  :storage-vector (make-matrix (/ (* ldm (1+ ldm)) 2)
						       :element-type element-type)
			  :ldm ldm
			  :upper? upper?)))
      (:hermitian
	 (progn
	   (assert (apply #'= dimensions) nil "Hermitian matrix should be square")
	   (assert (subtypep element-type 'complex)
		   nil "Hermitian matrix should be complex")
	   (make-instance 'hermitian-matrix
			  :storage-vector (make-matrix (/ (* ldm (1+ ldm)) 2)
						       :element-type element-type)
			  :ldm ldm
			  :upper? upper?))))))

;; mapping

(defun map-pmatrix (pmatrix func)
  (map-matrix (pmatrix-storage-vector pmatrix) func)
  pmatrix)

(defun map-two-pmatrices (pm1 pm2 func)
  (map-two-matrices (pmatrix-storage-vector pm1)
		    (pmatrix-storage-vector pm2)
		    func)
  pm1)

(defun make-random-pmatrix (dimensions &key
			    (element-type *default-type*)
			    (pack-type :general)
			    (upper? t)
			    (rng #'simple-rng))
  (let ((random-pmatrix (map-pmatrix (make-pmatrix dimensions
						   :element-type element-type
						   :pack-type pack-type
						   :upper? upper?)
				     rng)))
    (when (eq pack-type :hermitian)
      (let ((dim (first dimensions))
	    (pos (1- (- (first dimensions))))
	    (storage-vector (pmatrix-storage-vector random-pmatrix)))
	(dotimes (i dim)
	  (setf pos (+ pos 1 (- dim i)))
	  (setf (aref storage-vector pos)
		(complex (realpart (aref storage-vector pos)))))))
    random-pmatrix))

(defgeneric unpack-pmatrix (pmatrix &optional destination))

(defmethod unpack-pmatrix ((pmatrix diagonal-matrix) &optional destination)
  (let ((dest (or destination (make-matrix (list (ldm pmatrix) (ncols pmatrix))
					   :element-type (pmatrix-element-type pmatrix))))
	(storage-vector (pmatrix-storage-vector pmatrix)))
    (do-diag (dest i)
      (setf (aref dest i i)
	    (aref storage-vector i)))
    dest))

(defun hlp-unpack-pmatrix (pmatrix destination)
  (let ((dest (or destination (make-matrix (list (ldm pmatrix) (ldm pmatrix))
					   :element-type (pmatrix-element-type pmatrix))))
	(storage-vector (pmatrix-storage-vector pmatrix)))
    (let ((k 0))
      (if (upper? pmatrix)
	  (do-upper-triangle (dest i j)
	    (setf (aref dest i j) (aref storage-vector k))
	    (incf k))
	  (do-lower-triangle (dest i j)
	    (setf (aref dest i j) (aref storage-vector k))
	    (incf k))))
    dest))

(defmethod unpack-pmatrix ((pmatrix triangular-matrix) &optional destination)
  (hlp-unpack-pmatrix pmatrix destination))
	   
(defmethod unpack-pmatrix ((pmatrix symmetric-matrix) &optional destination)
  (funcall (if (upper? pmatrix)
	       #'mirror-upper-triangle
	       #'mirror-lower-triangle)
	   (hlp-unpack-pmatrix pmatrix destination)))

(defmethod unpack-pmatrix ((pmatrix hermitian-matrix) &optional destination)
  (funcall (if (upper? pmatrix)
	       #'mirror-upper-hermitian-triangle
	       #'mirror-lower-hermitian-triangle)
	   (hlp-unpack-pmatrix pmatrix destination)))

;; printing

(defmethod print-matrix ((matrix packed-matrix) &key (dest t) (prec 3) (exp? nil))
  (print-matrix (unpack-pmatrix matrix) :dest dest :prec prec :exp? exp?))


;; approximate equality

(defmethod ~= ((X packed-matrix) (Y packed-matrix) eps)
  (and (eq (class-of X) (class-of Y)))
  (~= (pmatrix-storage-vector X) (pmatrix-storage-vector Y) eps))
