(in-package :sb-math)

(declaim (inline sdsdot dsdot sdot ddot cdotu cdotc zdotu zdotc
		 sger dger cger zger))

;; dot

(defun sdsdot (X Y alpha)
  (%sdsdot (min (array-total-size X) (array-total-size Y))
	   alpha (array-sap X) 1 (array-sap Y) 1))

(defun dsdot (X Y)
  (%dsdot (min (array-total-size X) (array-total-size Y))
	  (array-sap X) 1 (array-sap Y) 1))

(defun sdot (X Y)
  (%sdot (min (array-total-size X) (array-total-size Y))
	 (array-sap X) 1 (array-sap Y) 1))

(defun ddot (X Y)
  (%ddot (min (array-total-size X) (array-total-size Y))
	 (array-sap X) 1 (array-sap Y) 1))

(defun cdotu (X Y)
  (let ((dotu #C(1.0 0.0)))
    (%cdotu_sub (min (array-total-size X) (array-total-size Y))
		(array-sap X) 1 (array-sap Y) 1 (complex-sap dotu))
    dotu))

(defun cdotc (X Y)
  (let ((dotc #C(1.0 0.0)))
    (%cdotc_sub (min (array-total-size X) (array-total-size Y))
		(array-sap X) 1 (array-sap Y) 1 (complex-sap dotc))
    dotc))

(defun zdotu (X Y)
  (let ((dotu #C(1d0 0d0)))
    (%zdotu_sub (min (array-total-size X) (array-total-size Y))
		(array-sap X) 1 (array-sap Y) 1 (complex-sap dotu))
    dotu))

(defun zdotc (X Y)
  (let ((dotc #C(1d0 0d0)))
    (%zdotc_sub (min (array-total-size X) (array-total-size Y))
		(array-sap X) 1 (array-sap Y) 1 (complex-sap dotc))
    dotc))

;; ger

(defun sger (A X Y alpha conj)
  (declare (ignore conj))
  (let ((m (dim0 X)) (n (dim0 Y)))
    (%sger 'CBlasRowMajor m n alpha (array-sap X) 1 (array-sap Y) 1 (array-sap A) n)))

(defun dger (A X Y alpha conj)
  (declare (ignore conj))
  (let ((m (dim0 X)) (n (dim0 Y)))
    (%dger 'CBlasRowMajor m n alpha (array-sap X) 1 (array-sap Y) 1 (array-sap A) n)))

(defun cger (A X Y alpha conj)
  (let ((m (dim0 X)) (n (dim0 Y)))
    (funcall (ecase conj (:noconj #'%cgeru) (:conj #'%cgerc))
	     'CBlasRowMajor m n (complex-sap alpha) (array-sap X) 1 (array-sap Y) 1 (array-sap A) n)))

(defun zger (A X Y alpha conj)
  (let ((m (dim0 X)) (n (dim0 Y)))
    (funcall (ecase conj (:noconj #'%zgeru) (:conj #'%zgerc))
	     'CBlasRowMajor m n (complex-sap alpha) (array-sap X) 1 (array-sap Y) 1 (array-sap A) n)))
