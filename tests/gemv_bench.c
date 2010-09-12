#include <stdio.h>
#include <stdlib.h>
#include <gsl/gsl_cblas.h>

void random_fill (float *array, int size)
{
  int i;
  for (i = 0; i < size; i++)
    array[i]=rand();
}
      
int main (void)
{
  int m,n;
  int iter;
  scanf("%d,%d,%d", &m, &n, &iter);
  
  float *A, *X, *Y;
  A = malloc(m*n*sizeof(float));
  X = malloc(n*sizeof(float));
  Y = malloc(m*sizeof(float));

  random_fill(A, m*n);
  random_fill(X, n);
  random_fill(Y, m);

  int lda=m;

  int i = 1;
  for(i = 1; i < iter; i++){
    
    cblas_sgemv (CblasRowMajor, 
                 CblasNoTrans, m, n,
                 1.0, A, lda, X, 1, 0.0, Y, 1);
  }
  
  return 0;  
}
