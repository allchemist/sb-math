(in-package :sb-math)

(export '(store-matrix restore-matrix))

(defun store-matrix (matrix path &optional append)
  (with-open-file (s path
		     :direction :output
		     :if-exists (if append :append :supersede)
		     :if-does-not-exist :create)
    (princ "sb-math matrix storage" s)
    (print (array-dimensions matrix) s)
    (print (array-element-type matrix) s) (terpri s)
    (dotimes (i (array-total-size matrix))
      (princ (aref (sb-ext:array-storage-vector matrix) i) s) (princ " " s)))
  path)

(defun restore-matrix (path)
  (macrolet ((body (s)
	       `(progn
		  (assert (string= (read-line ,s) "sb-math matrix storage") nil "Not a matrix storage")
		  (let* ((dimensions (read ,s))
			 (element-type (read ,s))
			 (matrix (make-matrix dimensions :element-type element-type)))
		    (dotimes (i (array-total-size matrix))
		      (setf (aref (sb-ext:array-storage-vector matrix) i) (read ,s nil nil)))
		    matrix))))
    (if (open-stream-p path)
	(body path)
	(with-open-file (s path)
	  (body s)))))
