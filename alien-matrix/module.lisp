(in-package :sb-math)

(load-foreign-library :alien-matrix)

(export
 '(
   transpose
   col
   row
   submatrix
   m+c
   m-c
   m*
   imin
   mmin
   imax
   mmax
   iamin
   ammax
   msum
   mean
   ))
