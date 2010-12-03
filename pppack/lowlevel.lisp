(in-package :sb-math)

(defun %cubspl (tau c ibcbeg ibcend)
  (let ((n (dim0 tau)))
    (with-alien ((*n integer n)
		 (*ibcbeg integer ibcbeg)
		 (*ibcend integer ibcend))
      (alien-funcall
       (extern-alien "cubspl_"
		     (function
		      void 
		      (* double-float) (* double-float) (* integer) (* integer) (* integer)))
       (array-sap tau) (array-sap c) (alien-sap (addr *n))
       (alien-sap (addr *ibcbeg)) (alien-sap (addr *ibcend)))))
  c)

(defun %ppvalu (break coef l k x jderiv)
  (with-alien ((*l integer l)
	       (*k integer k)
	       (*x double-float x)
	       (*jderiv integer jderiv))
    (alien-funcall
     (extern-alien "ppvalu_"
		   (function double-float
			     (* double-float) (* double-float) (* integer)
			     (* integer) (* double-float) (* integer)))
     (array-sap break) (array-sap coef) (alien-sap (addr *l))
     (alien-sap (addr *k)) (alien-sap (addr *x)) (alien-sap (addr *jderiv)))))

