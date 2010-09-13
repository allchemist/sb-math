(in-package :sb-math)

(defmacro get-run-time (op)
  `(let ((start (get-internal-run-time)))
     ,op
     (/ (- (get-internal-run-time) start)
	1000.0)))

(defun gemm-bench-sbmath (m n k iter)
  (get-run-time
   (let ((A (make-random-matrix `(,m ,k)))
	 (B (make-random-matrix `(,k ,n)))
	 (C (make-matrix `(,m ,n))))
     (dotimes (i iter)
       (gemm A B :dest C)))))

(defun gemv-bench-sbmath (m n iter)
  (get-run-time
   (let ((A (make-random-matrix `(,m ,n)))
	 (X (make-random-matrix n))
	 (Y (make-matrix m)))
     (dotimes (i iter)
       (gemv A X :dest Y)))))

(defun sgemm-bench-sbmath (m n k iter)
  (get-run-time
   (let ((A (make-random-matrix `(,m ,k)))
	 (B (make-random-matrix `(,k ,n)))
	 (C (make-matrix `(,m ,n))))
     (dotimes (i iter)
       (sgemm A B C 1.0 0.0 :notrans :notrans)))))

(defun sgemv-bench-sbmath (m n iter)
  (get-run-time
   (let ((A (make-random-matrix `(,m ,n)))
	 (X (make-random-matrix n))
	 (Y (make-matrix m)))
     (dotimes (i iter)
       (sgemv A X Y 1.0 0.0 :notrans)))))

(defun gemm-bench-gcc (m n k iter)
  (read
   (sb-ext:process-output
    (sb-ext:run-program
     "/bin/sh"
     `("-c" ,(format nil "/usr/bin/time -f '%U' sh -c \"echo '~A,~A,~A,~A' | ~Atests/gemm_bench\""
		     m n k iter *root-path*))
     :output :stream))))   

(defun gemv-bench-gcc (m n iter)
  (read
   (sb-ext:process-output
    (sb-ext:run-program
     "/bin/sh"
     `("-c" ,(format nil "/usr/bin/time -f '%U' sh -c \"echo '~A,~A,~A' | ~Atests/gemv_bench\""
		     m n iter *root-path*))
     :output :stream))))

(defun make-random-foreign-matrix (dimensions)
  (let ((matrix (grid:make-foreign-array 'single-float :dimensions dimensions)))
    (dotimes (i (first dimensions))
      (dotimes (j (second dimensions))
	(setf (grid:gref matrix i j) (random 1.0))))
    matrix))

(defun make-random-foreign-vector (length)
  (let ((vector (grid:make-foreign-array 'single-float :dimensions length)))
    (dotimes (i length)
      (setf (grid:gref vector i) (random 1.0)))
    vector))


(defun gemm-bench-gsll (m n k iter)
  (get-run-time
   (let ((A (make-random-foreign-matrix `(,m ,k)))
	 (B (make-random-foreign-matrix `(,k ,n)))
	 (C (make-random-foreign-matrix `(,m ,n))))
     (dotimes (i iter)
       (gsll:matrix-product A B C)))))

(defun gemv-bench-gsll (m n iter)
  (get-run-time
   (let ((A (make-random-foreign-matrix `(,m ,n)))
	 (X (make-random-foreign-vector n))
	 (Y (make-random-foreign-vector m)))
     (dotimes (i iter)
       (gsll:matrix-product A X Y)))))

(defparameter *gemm-bench-params*
  '((3 3 3 1000000)
    (4 4 4 1000000)
    (5 5 5 1000000)
    (6 6 6 1000000)
    (7 7 7 1000000)
    (8 8 8 1000000)
    (9 9 9 1000000)
    (10 10 10 1000000)
    (20 20 20 100000)
    (30 30 30 10000)
    (40 40 40 10000)
    (50 50 50 10000)
    (100 100 100 1000)
    (200 200 200 100)
    (300 300 300 100)
    (400 400 400 100)
    (500 500 500 10)))

(defparameter *gemv-bench-params*
  '((3 3 1000000)
    (4 4 1000000)
    (5 5 1000000)
    (6 6 1000000)
    (7 7 1000000)
    (8 8 1000000)
    (9 9 1000000)
    (10 10 1000000)
    (20 20 1000000)
    (30 30 1000000)
    (40 40 1000000)
    (50 50 1000000)
    (100 100 100000)
    (200 200 100000)
    (300 300 10000)
    (400 400 10000)
    (500 500 10000)))

(defun run-gemm-benchmark ()
  (let ((results nil))
    (dolist (p *gemm-bench-params*)
      (push
       (print (list
	       p
	       (list 'sbmath (apply #'gemm-bench-sbmath p))
	       (list 'sbmath-typed (apply #'sgemm-bench-sbmath p))
	       (list 'gsll (get-run-time (apply #'gemm-bench-gsll p)))
	       (list 'gcc (apply #'gemm-bench-gcc p))))
       results))
    (nreverse results)))

(defun run-gemv-benchmark ()
  (let ((results nil))
    (dolist (p *gemv-bench-params*)
      (push
       (print (list
	       p
	       (list 'sbmath (apply #'gemv-bench-sbmath p))
	       (list 'sbmath-typed (apply #'sgemv-bench-sbmath p))
	       (list 'gsll (get-run-time (apply #'gemv-bench-gsll p)))
	       (list 'gcc (apply #'gemv-bench-gcc p))))
       results))
    (nreverse results)))

(gplt:gplt-start)

(defun plot-gemm-bench-results (bench-results &key max-dim)
  (with-open-file (s "/tmp/gemm_bench.dat"
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
    (dolist (r bench-results)
      (when (and max-dim
		 (> (first (first r)) max-dim))
	(return))
      (format s "~A ~A ~A ~A ~A~%"
	      (first (first r))
	      (float (/ (second (second r))
			(fourth (first r))))
	      (float (/ (second (third r))
			(fourth (first r))))
	      (float (/ (second (fourth r))
			(fourth (first r))))
	      (float (/ (second (fifth r))
			(fourth (first r)))))))

  (gplt:gplt-restart)
  (if max-dim
      (mapcar #'gplt:gplt-exec
	   `((set title "'gemm benchmark, limited dimension'")
	     (set yrange (range 3 0.00002))
	     (set xrange (range 3 10))
	     (set xlabel " \"dimension\"")
	     (set ylabel " \"time (sec/call)\"")))
      (mapcar #'gplt:gplt-exec
	   `((set title "'gemm benchmark'")
	     (set yrange (range 0 3))
	     (set xrange (range 0 500))
	     (set xlabel " \"dimension\"")
	     (set ylabel " \"time (sec/call)\""))))
  (gplt:gplt-exec '(set term png))
  (gplt:gplt-exec '(set out "'/tmp/gemm_bench.png'"))
  (gplt:gplt-exec '("plot '/tmp/gemm_bench.dat' using 1:2 title 'sb-math' with lines lc rgb 'red', '/tmp/gemm_bench.dat' using 1:3 title 'typed sb-math' with lines lc rgb 'black', '/tmp/gemm_bench.dat' using 1:4 title 'gsll' with lines lc rgb 'green', '/tmp/gemm_bench.dat' using 1:5 title 'gcc' with lines lc rgb 'blue'"))
  (gplt:gplt-display))

(defun plot-gemv-bench-results (bench-results &key min-dim max-dim)
  (with-open-file (s "/tmp/gemv_bench.dat"
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
    (dolist (r bench-results)
      (when (and max-dim
		 (> (first (first r)) max-dim))
	(return))
      (when (or (not min-dim)
		(> (first (first r)) min-dim))
	(format s "~A ~A ~A ~A ~A~%"
		(first (first r))
		(float (/ (second (second r))
			  (third (first r))))
		(float (/ (second (third r))
			  (third (first r))))
		(float (/ (second (fourth r))
			  (third (first r))))
		(float (/ (second (fifth r))
			  (third (first r))))))))

  (gplt:gplt-restart)
  (if max-dim
      (mapcar #'gplt:gplt-exec
	   `((set title "'gemv benchmark, limited dimension'")
	     (set yrange (range 3 0.00002))
	     (set xrange (range 3 10))
	     (set xlabel " \"dimension\"")
	     (set ylabel " \"time (sec/call)\"")))
      (mapcar #'gplt:gplt-exec
	   `((set title "'gemv benchmark'")
	     (set yrange (range 0 3))
	     (set xrange (range 0 500))
	     (set xlabel " \"dimension\"")
	     (set ylabel " \"time (sec/call)\""))))
  (gplt:gplt-exec '(set term png))
  (gplt:gplt-exec '(set out "'/tmp/gemv_bench.png'"))
  (gplt:gplt-exec '("plot '/tmp/gemv_bench.dat' using 1:2 title 'sb-math' with lines lc rgb 'red', '/tmp/gemv_bench.dat' using 1:3 title 'typed sb-math' with lines lc rgb 'black', '/tmp/gemv_bench.dat' using 1:4 title 'gsll' with lines lc rgb 'green', '/tmp/gemv_bench.dat' using 1:5 title 'gcc' with lines lc rgb 'blue'"))
  (gplt:gplt-display))
