(in-package :sb-math2)

;; utils for macro reading arrays with type auto-detecting

(defun coerce-tree (tree type)
  (if (consp tree)
      (cons (coerce-tree (car tree) type) (coerce-tree (cdr tree) type))
      (if (null tree) nil (coerce tree type))))

(defun tree-element-type (tree)
  (if (not (consp tree))
      (type-of tree)
      (tree-element-type (car tree))))

(defun %tree-dims (tree dims)
  (if (not (consp tree))
      dims
      (progn
	(push (length tree) dims)
	(%tree-dims (car tree) dims))))

(defun tree-dims (tree)
  (nreverse (%tree-dims tree '())))

(defun sharp-M (stream ignore arg) 
  (declare (ignore ignore)) 
  (let ((args (read stream t nil t)))
    (let ((type (or (case arg
		      (2  'fixnum)
		      (3  '(unsigned-byte 32))
		      (00 'single-float)
		      (01 'double-float)
		      (10 '(complex single-float))
		      (11 '(complex double-float)))
		    (tree-element-type args))))
      (make-array (tree-dims args)
		  :element-type type
		  :initial-contents (coerce-tree args type)))))

(set-dispatch-macro-character #\# #\M #'sharp-m)
