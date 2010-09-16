(in-package :sb-math)

(defparameter *libs* nil)
(defparameter *root-path*
  (let ((path (namestring (asdf:component-relative-pathname (asdf:find-system :sb-math)))))
    (if (search ".asd" path)
	(subseq path 0 (1+ (position #\/ path :from-end t)))
	path)))

(defun define-foreign-library (name &rest pathnames)
  (push
   (list name
	 (mapcar
	  #'(lambda (path)
	      (cond ((string= (subseq path 0 3) "lib") ;; treat as relative path
		     (concatenate 'string *root-path* path))
		    ((char= (elt path 0)  #\/) ;; treat path as absolute
		     path)
		    (t (error "Path not found ~A" path))))
	  pathnames))
     *libs*))
  
(defun load-foreign-library (name)
  (map nil #'load-shared-object (second (assoc name *libs*))))

#|
(defun compile-c-source ()
  (when (not (probe-file (concatenate 'string *root-path* "lib/libsbmath.so")))
    (format *query-io* "; Compiling C source~%")
    (if (zerop
	 (sb-ext:process-exit-code
	  (sb-ext:run-program 
	   (concatenate 'string *root-path* "lib/build_c_source.sh")
	   (list *root-path*))))
	(format *query-io* "; Successfully~%")
	(cerror "Continue anyway" "Compiling C source returned non-zero code"))))

(compile-c-source)

(define-foreign-library :alien-matrix "lib/libsbmath.so")
|#

;; if using GSL CBLAS

#+x86 (define-foreign-library :blas "lib/libgslcblas.so")
#+x86-64 (define-foreign-library :blas "lib/libgslcblas64.so")

;; if using ATLAS
;; #+x86 (define-foreign-library :blas "lib/libatlas.so" "lib/libcblas_atlas.so")
;; #+x86-64 (define-foreign-library :blas "lib/libatlas64.so" "lib/libcblas_atlas64.so")


#+x86 (define-foreign-library :lapack "lib/liblapack.so")
#+x86-64 (define-foreign-library :lapack "lib/liblapack64.so")
