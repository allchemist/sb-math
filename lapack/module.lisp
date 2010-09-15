(in-package :sb-math2)

(load-foreign-library :lapack)

(export
 '(lu
   lu-solve
   lu-inverse
   svd
   eigen))
