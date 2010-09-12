#include <stdio.h>
#include <stdlib.h>
#include <gsl/gsl_cblas.h>

void random_fill (float *array, int m, int n)
{
  int i;
  for (i = 0; i < m*n; i++)
    array[i]=rand();
}
      
int main (void)
{
  int m,n,k;
  int iter;
  scanf("%d,%d,%d,%d", &m, &n, &k, &iter);
  
  float *A, *B, *C;
  A = malloc(m*k*sizeof(float));
  B = malloc(k*n*sizeof(float));
  C = malloc(m*n*sizeof(float));

  random_fill(A, m, k);
  random_fill(B, k, n);
  random_fill(C, m, n);

  int lda=m;
  int ldb = k;
  int ldc = m;

  int i = 1;
  for(i = 1; i < iter; i++){
    
    cblas_sgemm (CblasRowMajor, 
                 CblasNoTrans, CblasNoTrans, m, n, k,
                 1.0, A, lda, B, ldb, 0.0, C, ldc);
  }
  
  return 0;  
}
