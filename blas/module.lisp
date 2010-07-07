(in-package :sb-math)

(load-foreign-library :blas)

(export
 '(inner-prod e-norm normalize amsum iamax ammax swap copy
   copy-with-offset axpy m+ m- m*c
   gemv ger trmv symv hemv syr her gemm ))

(export
 '(tpmv spmv hpmv spr hpr))
