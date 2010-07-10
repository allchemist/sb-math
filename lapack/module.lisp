(in-package :sb-math)

(load-foreign-library :lapack)

(export
 '(lu
   lu-solve
   lu-inverse
   svd
   eigen))
