(in-package :sb-math)

;;; ==============================================================
;;; single

;; BLAS 1

(let ((v1 (make-random-matrix 5))
      (v2 (make-random-matrix 5)))

  (define-test "dot-single"
    (inner-prod v1 v2)
    (reduce #'+ (map 'vector #'* v1 v2))
    :eps *eps-single*)

  (define-test "e-norm-single"
    (e-norm v1)
    (sqrt (reduce #'+ (map 'vector #'square v1)))
    :eps *eps-single*)

  (define-test "amax-single"
    (ammax v1)
    (apply #'max (coerce v1 'list))
    :eps *eps-single*)

  (define-test "copy-with-offset-single"
    (let ((dest (make-matrix 7)))
      (copy-with-offset v1 dest 1)
      (if (and (zerop (elt dest 0))
	       (zerop (elt dest 6)))
	  (e-norm dest)
	  0.0))
    (e-norm v1)
    :eps *eps-single*)

  (define-test "scale-single" (/ (e-norm (m*c (copy v1) 3.0)) (e-norm v1)) 3.0 :eps *eps-single*)
)

;; BLAS 2

(let ((m1 (make-matrix '(3 5)))
      (v1 (make-matrix 5))
      (v2 (make-matrix 3)))
  (dotimes (i (dim0 m1))
    (dotimes (j (dim1 m1))
      (setf (aref m1 i j) (exp (/ (+ (- i j) 0.5))))))
  (dotimes (i (dim0 v1))
    (setf (aref v1 i) (exp (/ i (dim0 v1)))))
  (dotimes (i (dim0 v2))
    (setf (aref v2 i) (/ (- (exp i) 0.5))))

  (define-test "gemv-single"
    (gemv m1 v1)
    (make-array (dim0 m1) :element-type 'single-float
		:initial-contents '(11.214129 13.601975 16.283197))
    :eps *eps-single*)

  (define-test "gemv-trans-single"
    (gemv m1 v2 :transA :trans)
    (make-array (dim1 m1) :element-type 'single-float
	        :initial-contents '(15.8727 3.884381 2.1604223 1.5917332 1.8796608))
    :eps *eps-single*)

  (define-test "ger-single"
    (ger v1 v2)
    (make-array '(5 3) :element-type 'single-float
		:initial-contents
		'((2.0 0.45079938 0.14515777)
		  (2.4428055 0.5506076 0.1772961)
		  (2.9836495 0.67251366 0.21654995)
		  (3.6442375 0.82141 0.2644947)
		  (4.4510818 1.0032724 0.32305455)))
    :eps *eps-single*)
)

(let ((m (make-matrix '(5 5)))
      (v (make-random-matrix 5)))
  (dotimes (i 5)
    (dotimes (j 5)
      (when (>= j i) (setf (aref m i j) (random 1.0)))))
  (dotimes (i 5)
    (setf (aref v i) (random 1.0)))
  (define-test "trmv-single" (gemv m v) (trmv m v) :eps *eps-single*)
  (dotimes (i 5)
    (dotimes (j 5)
      (when (>= j i) (setf (aref m i j) (setf (aref m i j) (random 1.0))))))
  (define-test "symv-single" (gemv m v) (symv m v) :eps *eps-single*)
)

;; BLAS 3

(let ((m1 (make-matrix '(3 5)))
      (m3x3 nil))
  (dotimes (i 3)
    (dotimes (j 5)
      (setf (aref m1 i j) (exp (/ (+ (- i j) 0.5))))))

  (setf m3x3 #m((55.894108 16.309284 15.556982)
	       (16.309284 59.12306 18.711231)
	       (15.556982 18.711231 60.89927)))

 (define-test "gemm-single"
    (gemm m1 (transpose m1)) m3x3 :eps *eps-single*)

  (define-test "gemm-transa-single"
    (gemm m1 m1 :transa :trans)
    #m((60.61736 18.297594 15.080442 6.154929 7.6242414)
       (18.297594 58.410133 15.461399 4.147983 6.0547338)
       (15.080442 15.461399 54.880066 1.4136373 4.2702074)
       (6.154929 4.147983 1.4136373 0.7312417 0.9173675)
       (7.6242414 6.0547338 4.2702074 0.9173675 1.2776442))
    :eps *eps-single*)

  (define-test "gemm-transb-single"
      (gemm m1 m1 :transb :trans) m3x3 :eps *eps-single*)

  (define-test "gemm-transa-transb-single"
      (gemm (transpose m1) m1 :transa :trans :transb :trans) m3x3 :eps *eps-single*)

  (define-test "gemm-alpha-beta-single"
      (gemm m1 (transpose m1) :alpha 2 :beta 0)
    #m((111.788216 32.61857 31.113964)
       (32.61857 118.24612 37.422462)
       (31.113964 37.422462 121.79854))
    :eps *eps-single*)
)

;;; ==============================================================
;;; double

;; BLAS 1

(let ((v1 (make-random-matrix 5 :element-type 'double-float))
      (v2 (make-random-matrix 5 :element-type 'double-float)))

  (define-test "dot-double"
    (inner-prod v1 v2)
    (reduce #'+ (map 'vector #'* v1 v2))
    :eps *eps-double*)

  (define-test "e-norm-double"
    (e-norm v1)
    (sqrt (reduce #'+ (map 'vector #'square v1)))
    :eps *eps-double*)

  (define-test "amax-double"
    (ammax v1)
    (apply #'max (coerce v1 'list))
    :eps *eps-double*)

  (define-test "copy-with-offset-double"
    (let ((dest (make-matrix 7 :element-type 'double-float)))
      (copy-with-offset v1 dest 1)
      (if (and (zerop (elt dest 0))
	       (zerop (elt dest 6)))
	  (e-norm dest)
	  0d0))
    (e-norm v1)
    :eps *eps-double*)

  (define-test "scale-double" (/ (e-norm (m*c (copy v1) 3d0)) (e-norm v1)) 3d0 :eps *eps-double*)
)

;; BLAS 2

(let ((m1 (make-matrix '(3 5) :element-type 'double-float))
      (v1 (make-matrix 5 :element-type 'double-float))
      (v2 (make-matrix 3 :element-type 'double-float)))
  (do-matrix (m1 i j)
    (setf (aref m1 i j) (coerce (exp (/ (+ (- i j) 0.5))) 'double-float)))
  (do-matrix (v1 i)
    (setf (aref v1 i) (coerce (exp (/ i (dim0 v1))) 'double-float)))
  (do-matrix (v2 i)
    (setf (aref v2 i) (coerce (/ (- (exp i) 0.5)) 'double-float)))

  (define-test "gemv-double"
    (gemv m1 v1)
    (make-array (dim0 m1) :element-type 'double-float
		:initial-contents
		'(11.214129515182728d0 13.601975737139748d0 16.28319692189361d0))
    :eps *eps-double*)

  (define-test "gemv-trans-double"
    (gemv m1 v2 :transA :trans)
    (make-array (dim1 m1) :element-type 'double-float
	        :initial-contents
		'(15.872699403103192d0 3.884381075376563d0 2.160422164525806d0
		  1.5917331699499888d0 1.8796609178889978d0))
    :eps *eps-double*)

  (define-test "ger-double"
    (ger v1 v2)
    (make-array '(5 3) :element-type 'double-float
		:initial-contents
		'((2.0d0 0.450799363896619d0 0.14515776474075123d0)
		  (2.442805528640747d0 0.5506075892171964d0 0.17729609511692002d0)
		  (2.983649492263794d0 0.6725136466014943d0 0.21654994553344484d0)
		  (3.644237518310547d0 0.821409977571294d0 0.2644946861711707d0)
		  (4.4510817527771d0 1.0032724114018823d0 0.32305453895573444d0)))
    :eps *eps-double*)
)

(let ((m (make-matrix '(5 5) :element-type 'double-float))
      (v (make-random-matrix 5 :element-type 'double-float)))
  (do-matrix (m i j)
    (when (>= j i) (setf (aref m i j) (random 1d0))))
  (define-test "trmv-double" (gemv m v) (trmv m v) :eps *eps-double*)
  (do-matrix (m i j)
    (when (>= j i) (setf (aref m i j) (setf (aref m j i) (random 1d0)))))
  (define-test "symv-double" (gemv m v) (symv m v) :eps *eps-double*)
)

;; BLAS 3

(let ((m1 (make-matrix '(3 5) :element-type 'double-float))
      (m3x3 nil))
  (do-matrix (m1 i j)
    (setf (aref m1 i j) (coerce (exp (/ (+ (- i j) 0.5))) 'double-float)))

  (setf m3x3 #m((55.89410989627369d0 16.30928362692707d0 15.556980673559046d0)
		(16.30928362692707d0 59.123059668949104d0 18.711231080120108d0)
		(15.556980673559046d0 18.711231080120108d0 60.899271633324354d0)))

 (define-test "gemm-double"
    (gemm m1 (transpose m1)) m3x3 :eps *eps-double*)

  (define-test "gemm-transa-double"
    (gemm m1 m1 :transa :trans)
    #m((60.61735885631989d0 18.297593842031894d0 15.080441413440507d0
			    6.154928942389771d0 7.624241386035439d0)
       (18.297593842031894d0 58.41013356671615d0 15.461399546372695d0
			     4.1479829860883175d0 6.054733816699342d0)
       (15.080441413440507d0 15.461399546372695d0 54.8800628101487d0
			     1.413637238088214d0 4.270207154801715d0)
       (6.154928942389771d0 4.1479829860883175d0 1.413637238088214d0
			    0.7312417411216826d0 0.9173675317771773d0)
       (7.624241386035439d0 6.054733816699342d0 4.270207154801715d0
			    0.9173675317771773d0 1.2776442242407076d0))
    :eps *eps-double*)

  (define-test "gemm-transb-double"
      (gemm m1 m1 :transb :trans) m3x3 :eps *eps-double*)

  (define-test "gemm-transa-transb-double"
      (gemm (transpose m1) m1 :transa :trans :transb :trans) m3x3 :eps *eps-double*)

  (define-test "gemm-alpha-beta-double"
      (gemm m1 (transpose m1) :alpha 2 :beta 0)
    #m((111.78821979254738d0 32.61856725385414d0 31.113961347118092d0)
       (32.61856725385414d0 118.24611933789821d0 37.422462160240215d0)
       (31.113961347118092d0 37.422462160240215d0 121.79854326664871d0))
    :eps *eps-double*)
)

;;; ==============================================================
;;; complex-single

;; BLAS 1

(let ((v1 (make-random-matrix 5 :element-type '(complex single-float)))
      (v2 (make-random-matrix 5 :element-type '(complex single-float))))

  (define-test "dot-complex-single"
    (inner-prod v1 v2)
    (reduce #'+ (map 'vector #'* v1 v2))
    :eps *eps-single*)

  (define-test "e-norm-complex-single"
    (e-norm v1)
    (sqrt (reduce #'+ (map 'vector #'(lambda (x) (square (abs x))) v1)))
    :eps *eps-single*)

;  (define-test "amax-complex-single"
;    (abs (aref v1 (amax v1)))
;    (apply #'max (map 'list #'abs v1))
;    :eps *eps-single*)

  (define-test "copy-with-offset-complex-single"
    (let ((dest (make-matrix 7 :element-type '(complex single-float))))
      (copy-with-offset v1 dest 1)
      (if (and (zerop (elt dest 0))
	       (zerop (elt dest 6)))
	  (e-norm dest)
	  #C(1.0 0.0)))
    (e-norm v1)
    :eps *eps-single*)

  (define-test "scale-complex-single" (/ (e-norm (m*c (copy v1) #C(3.0 0.0))) (e-norm v1)) 3.0
    :eps *eps-single*)
)

;; BLAS 2

(let ((m1 (make-matrix '(3 5) :element-type '(complex single-float)))
      (v1 (make-matrix 5 :element-type '(complex single-float)))
      (v2 (make-matrix 3 :element-type '(complex single-float))))
  (dotimes (i 3) (dotimes (j 5)
    (setf (aref m1 i j) (coerce (exp (/ (+ (- i j) #C(0.5 0.5)))) '(complex single-float)))))
  (dotimes (i 5)
    (setf (aref v1 i) (coerce (exp (complex (/ i (dim0 v1))
					    (dim0 v1)))
			      '(complex single-float))))
  (dotimes (i 3)
    (setf (aref v2 i) (coerce (complex (/ (- (exp i) 0.5)) (exp i)) '(complex single-float))))

  (define-test "gemv-complex-single"
    (gemv m1 v1)
    #10m(#C(-1.3272094 -6.056485) #C(-1.9651418 -7.220763) #C(-2.4799757 -8.446573))
    :eps *eps-single*)

   (define-test "gemv-trans-complex-single"
    (gemv m1 v2 :transA :trans)
    (make-array (dim1 m1) :element-type '(complex single-float)
	        :initial-contents
		(list #C(8.060576 12.391497) #C(10.520908 15.683644) #C(19.230438 11.240795)
		      #C(4.2647696 3.4107726) #C(2.872455 6.474587)))
    :eps *eps-single*)

  (define-test "ger-complex-single"
    (ger v1 v2)
    #10m((#C(1.5262487 -1.6341864) #C(2.7345011 0.3387913) #C(7.1267214 1.9568006))
	 (#C(1.8641644 -1.9959998) #C(3.3399274 0.41380063) #C(8.704597 2.3900416))
	 (#C(2.2768955 -2.4379196) #C(4.0793967 0.5054173) #C(10.631819 2.9192035))
	 (#C(2.7810063 -2.9776816) #C(4.982586 0.617318) #C(12.985732 3.5655231))
	 (#C(3.396729 -3.6369486) #C(6.085744 0.75399387) #C(15.860809 4.35494)))
    :eps *eps-single*)
)

(let ((m (make-matrix '(5 5) :element-type '(complex single-float)))
      (v (make-random-matrix 5 :element-type '(complex single-float))))
  (dotimes (i 5) (dotimes (j 5)
    (when (>= j i) (setf (aref m i j) (complex (random 1.0) (random 1.0))))))
  (dotimes (i 5)
    (setf (aref v i) (complex (random 1.0) (random 1.0))))
  (define-test "trmv-complex-single" (gemv m v) (trmv m v) :eps *eps-single*)
)

;; BLAS 3

(let ((m1 (make-matrix '(3 5) :element-type '(complex single-float)))
      (m3x3 nil))
  (dotimes (i 3) (dotimes (j 5)
    (setf (aref m1 i j) (coerce (exp (/ (+ (- i j) #C(0.5 0.5)))) '(complex single-float)))))

  (setf m3x3 #10m((#C(-1.8265547 -7.075855) #C(2.32209 -5.8760457) #C(3.198052 -5.8503995))
		  (#C(2.32209 -5.8760457) #C(0.66209346 -8.323121) #C(4.3859315 -6.5478544))
		  (#C(3.198052 -5.8503995) #C(4.3859315 -6.5478544) #C(2.336813 -8.582822))))

 (define-test "gemm-complex-single"
    (gemm m1 (transpose m1)) m3x3 :eps *eps-single*)

  (define-test "gemm-transa-complex-single"
    (gemm m1 m1 :transa :trans)
    #10m((#C(2.1157143 -8.342472) #C(3.9534228 -6.2575417) #C(2.6764483 -5.5313087)
	    #C(2.0543962 -2.4945269) #C(3.008594 -2.3312786))
	 (#C(3.9534228 -6.2575417) #C(-0.07322146 -8.134824)
	    #C(1.4517815 -5.7138925) #C(0.90217906 -2.2356992)
	    #C(1.9389575 -2.2584543))
	 (#C(2.6764483 -5.5313087) #C(1.4517815 -5.7138925)
	    #C(-2.8538332 -6.9591994) #C(0.016361792 -1.1996094)
	    #C(1.0621778 -1.7095298))
	 (#C(2.0543962 -2.4945269) #C(0.90217906 -2.2356992)
	    #C(0.016361792 -1.1996094) #C(0.6789955 -0.31135732)
	    #C(0.9434673 -0.35032862))
	 (#C(3.008594 -2.3312786) #C(1.9389575 -2.2584543) #C(1.0621778 -1.7095298)
	    #C(0.9434673 -0.35032862) #C(1.304697 -0.2339453)))
    :eps *eps-single*)

  (define-test "gemm-transb-complex-single"
      (gemm m1 m1 :transb :trans) m3x3 :eps *eps-single*)

  (define-test "gemm-transa-transb-complex-single"
      (gemm (transpose m1) m1 :transa :trans :transb :trans) m3x3 :eps *eps-single*)

  (define-test "gemm-alpha-beta-complex-single"
      (gemm m1 (transpose m1) :alpha 2 :beta 0)
    #10m((#C(-3.6531093 -14.15171) #C(4.64418 -11.752091) #C(6.396104 -11.700799))
	 (#C(4.64418 -11.752091) #C(1.3241869 -16.646242) #C(8.771863 -13.095709))
	 (#C(6.396104 -11.700799) #C(8.771863 -13.095709) #C(4.673626 -17.165644)))
    :eps *eps-single*)
)

;;; ==============================================================
;;; complex-double

;; BLAS 1


(let ((v1 (make-random-matrix 5 :element-type '(complex double-float)))
      (v2 (make-random-matrix 5 :element-type '(complex double-float))))

  (define-test "dot-complex-double"
    (inner-prod v1 v2)
    (reduce #'+ (map 'vector #'* v1 v2))
    :eps *eps-double*)

  (define-test "e-norm-complex-double"
    (e-norm v1)
    (sqrt (reduce #'+ (map 'vector #'(lambda (x) (square (abs x))) v1)))
    :eps *eps-double*)

;  (define-test "amax-complex-double"
;    (abs (aref v1 (amax v1)))
;    (apply #'max (map 'list #'abs v1))
;    :eps *eps-double*)

  (define-test "copy-with-offset-complex-double"
    (let ((dest (make-matrix 7 :element-type '(complex double-float))))
      (copy-with-offset v1 dest 1)
      (if (and (zerop (elt dest 0))
	       (zerop (elt dest 6)))
	  (e-norm dest)
	  #C(1d0 0d0)))
    (e-norm v1)
    :eps *eps-double*)

  (define-test "scale-complex-double" (/ (e-norm (m*c (copy v1) #C(3d0 0d0))) (e-norm v1)) 3d0
    :eps *eps-double*)
)

;; BLAS 2
(let ((m1 (make-matrix '(3 5) :element-type '(complex double-float)))
      (v1 (make-matrix 5 :element-type '(complex double-float)))
      (v2 (make-matrix 3 :element-type '(complex double-float))))
  (dotimes (i 3) (dotimes (j 5)
    (setf (aref m1 i j) (coerce (exp (/ (+ (- i j) #C(0.5 0.5)))) '(complex double-float)))))
  (dotimes (i 5)
    (setf (aref v1 i) (coerce (exp (complex (/ i (dim0 v1))
					    (dim0 v1)))
			      '(complex double-float))))
  (dotimes (i 3)
    (setf (aref v2 i) (coerce (complex (/ (- (exp i) 0.5)) (exp i)) '(complex double-float))))

  (define-test "gemv-complex-double"
    (gemv m1 v1)
    (make-array (dim0 m1) :element-type '(complex double-float)
		:initial-contents
		'(#C(-1.327209347261007d0 -6.056484941561951d0)
		  #C(-1.9651417261616424d0 -7.220763349064781d0)
		  #C(-2.4799757035207293d0 -8.446573547342686d0)))
    :eps *eps-double*)

   (define-test "gemv-trans-complex-double"
    (gemv m1 v2 :transA :trans)
    (make-array (dim1 m1) :element-type '(complex double-float)
	        :initial-contents
		'(#C(8.060576422294835d0 12.391497009551333d0)
		  #C(10.520908481601813d0 15.683644438299712d0)
		  #C(19.230439035561275d0 11.240794818157278d0)
		  #C(4.264769744730811d0 3.410772643311717d0)
		  #C(2.8724548327341113d0 6.474586870562336d0)))
    :eps *eps-double*)

  (define-test "ger-complex-double"
    (ger v1 v2)
    (make-array '(5 3) :element-type '(complex double-float)
		:initial-contents
		'((#C(1.5262486934661865d0 -1.6341863870620728d0)
		   #C(2.7345011454589745d0 0.3387913072633353d0)
		   #C(7.126721274052953d0 1.956800627658751d0))
		  (#C(1.864164412021637d0 -1.9959998428821564d0)
		   #C(3.3399273582817655d0 0.41380062553983166d0)
		   #C(8.70459733585413d0 2.3900416982855557d0))
		  (#C(2.276895582675934d0 -2.437919706106186d0)
		   #C(4.07939650059678d0 0.5054172587198522d0)
		   #C(10.631819216651632d0 2.9192036160190717d0))
		  (#C(2.781006336212158d0 -2.9776816368103027d0)
		   #C(4.982585770716227d0 0.6173179825739368d0)
		   #C(12.985732361915318d0 3.5655230703751d0))
		  (#C(3.3967288732528687d0 -3.6369486451148987d0)
		   #C(6.085744130031719d0 0.753993891502418d0)
		   #C(15.860809658503687d0 4.3549397745944916d0))))
    :eps *eps-double*)
)

(let ((m (make-matrix '(5 5) :element-type '(complex double-float)))
      (v (make-random-matrix 5 :element-type '(complex double-float))))
  (dotimes (i 5) (dotimes (j 5)
    (when (>= j i) (setf (aref m i j) (complex (random 1d0) (random 1d0))))))
  (dotimes (i 5)
    (setf (aref v i) (complex (random 1d0) (random 1d0))))
  (define-test "trmv-complex-double" (gemv m v) (trmv m v) :eps *eps-double*)
)

;; BLAS 3

(let ((m1 (make-matrix '(3 5) :element-type '(complex double-float)))
      (m3x3 nil))
  (dotimes (i 3) (dotimes (j 5)
    (setf (aref m1 i j) (coerce (exp (/ (+ (- i j) #C(0.5 0.5)))) '(complex double-float)))))

  (setf m3x3 #11m((#C(-1.8265545278069952d0 -7.075854306338542d0)
		     #C(2.322090008480045d0 -5.876045534645553d0)
		     #C(3.1980518934822717d0 -5.85039943071468d0))
		  (#C(2.322090008480045d0 -5.876045534645553d0)
		     #C(0.6620934591563497d0 -8.323120724461964d0)
		     #C(4.385931439664744d0 -6.547853836641243d0))
		  (#C(3.1980518934822717d0 -5.85039943071468d0)
		     #C(4.385931439664744d0 -6.547853836641243d0)
		     #C(2.336813334617367d0 -8.582822085210687d0))))

  (define-test "gemm-complex-double"
    (gemm m1 (transpose m1)) m3x3 :eps *eps-double*)

  (define-test "gemm-transa-complex-double"
    (gemm m1 m1 :transa :trans)
    #11m((#C(2.1157143753012084d0 -8.342471489069144d0)
	    #C(3.9534227947460816d0 -6.257541820752559d0)
	    #C(2.676448368347941d0 -5.5313086918078955d0)
	    #C(2.054396028351197d0 -2.494526821942628d0)
	    #C(3.008594010716256d0 -2.3312784253426533d0))
	 (#C(3.9534227947460816d0 -6.257541820752559d0)
	    #C(-0.07322135344336633d0 -8.134823433049126d0)
	    #C(1.45178147046874d0 -5.713892382275819d0)
	    #C(0.9021790274749666d0 -2.2356992730000407d0)
	    #C(1.9389573572392143d0 -2.258454244023234d0))
	 (#C(2.676448368347941d0 -5.5313086918078955d0)
	    #C(1.45178147046874d0 -5.713892382275819d0)
	    #C(-2.853833148844849d0 -6.959199591876729d0)
	    #C(0.016361789971886198d0 -1.1996093745419996d0)
	    #C(1.0621778247203675d0 -1.70952978553678d0))
	 (#C(2.054396028351197d0 -2.494526821942628d0)
	    #C(0.9021790274749666d0 -2.2356992730000407d0)
	    #C(0.016361789971886198d0 -1.1996093745419996d0)
	    #C(0.6789954537356715d0 -0.31135731061831784d0)
	    #C(0.9434673112034566d0 -0.3503286251668145d0))
	 (#C(3.008594010716256d0 -2.3312784253426533d0)
	    #C(1.9389573572392143d0 -2.258454244023234d0)
	    #C(1.0621778247203675d0 -1.70952978553678d0)
	    #C(0.9434673112034566d0 -0.3503286251668145d0)
	    #C(1.3046969392180567d0 -0.233945291397875d0)))
    :eps *eps-double*)

  (define-test "gemm-transb-complex-double"
      (gemm m1 m1 :transb :trans) m3x3 :eps *eps-double*)

  (define-test "gemm-transa-transb-complex-double"
      (gemm (transpose m1) m1 :transa :trans :transb :trans) m3x3 :eps *eps-double*)

  (define-test "gemm-alpha-beta-complex-double"
      (gemm m1 (transpose m1) :alpha 2 :beta 0)
    #11m((#C(-3.6531090556139905d0 -14.151708612677083d0)
	    #C(4.64418001696009d0 -11.752091069291106d0)
	    #C(6.396103786964543d0 -11.70079886142936d0))
	 (#C(4.64418001696009d0 -11.752091069291106d0)
	    #C(1.3241869183126993d0 -16.646241448923927d0)
	    #C(8.771862879329488d0 -13.095707673282487d0))
	 (#C(6.396103786964543d0 -11.70079886142936d0)
	    #C(8.771862879329488d0 -13.095707673282487d0)
	    #C(4.673626669234734d0 -17.165644170421373d0)))
    :eps *eps-double*)

)
