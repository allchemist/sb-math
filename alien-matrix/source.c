#include <complex.h>
#include <math.h>

// transposition

void strans (float array[], float dest[], int dim0, int dim1)
{
  int i, j;
  for (i = 0; i < dim0; i++) {
    for (j = 0; j < dim1; j++) {
      dest[j*dim0+i]=array[i*dim1+j];
    }
  }
}

void dtrans (double array[], double dest[], int dim0, int dim1)
{
  int i, j;
  for (i = 0; i < dim0; i++) {
    for (j = 0; j < dim1; j++) {
      dest[j*dim0+i]=array[i*dim1+j];
    }
  }
}

void ctrans (float complex array[], float complex dest[], int dim0, int dim1)
{
  int i, j;
  for (i = 0; i < dim0; i++) {
    for (j = 0; j < dim1; j++) {
      dest[j*dim0+i]=array[i*dim1+j];
    }
  }
}

void ztrans (double complex array[], double complex dest[], int dim0, int dim1)
{
  int i, j;
  for (i = 0; i < dim0; i++) {
    for (j = 0; j < dim1; j++) {
      dest[j*dim0+i]=array[i*dim1+j];
    }
  }
}

// column slicing

void scol (float array[], float col[], int dim0, int dim1, int index)
{ int i; for (i = 0; i < dim0; i++) { col[i] = array[i*dim1+index]; }}

void dcol (double array[], double col[], int dim0, int dim1, int index)
{ int i; for (i = 0; i < dim0; i++) { col[i] = array[i*dim1+index]; }}

void ccol (float complex array[], float complex col[], int dim0, int dim1, int index)
{ int i; for (i = 0; i < dim0; i++) { col[i] = array[i*dim1+index]; }}

void zcol (double complex array[], double complex col[], int dim0, int dim1, int index)
{ int i; for (i = 0; i < dim0; i++) { col[i] = array[i*dim1+index]; }}

void ssetcol (float array[], float col[], int dim0, int dim1, int index)
{ int i; for (i = 0; i < dim0; i++) { array[i*dim1+index] = col[i]; }}

void dsetcol (double array[], double col[], int dim0, int dim1, int index)
{ int i; for (i = 0; i < dim0; i++) { array[i*dim1+index] = col[i]; }}

void csetcol (float complex array[], float complex col[], int dim0, int dim1, int index)
{ int i; for (i = 0; i < dim0; i++) { array[i*dim1+index] = col[i]; }}

void zsetcol (double complex array[], double complex col[], int dim0, int dim1, int index)
{ int i; for (i = 0; i < dim0; i++) { array[i*dim1+index] = col[i]; }}

// row slicing

void srow (float array[], float row[], int dim0, int dim1, int index)
{ int i; for (i = 0; i < dim1; i++) { row[i] = array[dim1*index+i]; }}

void drow (double array[], double row[], int dim0, int dim1, int index)
{ int i; for (i = 0; i < dim1; i++) { row[i] = array[dim1*index+i]; }}

void crow (float complex array[], float complex row[], int dim0, int dim1, int index)
{ int i; for (i = 0; i < dim1; i++) { row[i] = array[dim1*index+i]; }}

void zrow (double complex array[], double complex row[], int dim0, int dim1, int index)
{ int i; for (i = 0; i < dim1; i++) { row[i] = array[dim1*index+i]; }}

void ssetrow (float array[], float row[], int dim0, int dim1, int index)
{ int i; for (i = 0; i < dim1; i++) { array[dim1*index+i] = row[i]; }}

void dsetrow (double array[], double row[], int dim0, int dim1, int index)
{ int i; for (i = 0; i < dim1; i++) { array[dim1*index+i] = row[i]; }}

void csetrow (float complex array[], float complex row[], int dim0, int dim1, int index)
{ int i; for (i = 0; i < dim1; i++) { array[dim1*index+i] = row[i]; }}

void zsetrow (double complex array[], double complex row[], int dim0, int dim1, int index)
{ int i; for (i = 0; i < dim1; i++) { array[dim1*index+i] = row[i]; }}

// submatrix

void ssubmatrix (float array[], float dest[], int dim0, int dim1,
		int x_start, int y_start, int sub_dim0, int sub_dim1)
{
  int i,j;
  for (i = 0; i < sub_dim0; i++) {
    for (j = 0; j < sub_dim1; j++) {
      dest[i*sub_dim1+j]=array[(x_start+i)*dim1+j+y_start];
    }
  }
}

void dsubmatrix (double array[], double dest[], int dim0, int dim1,
		 int x_start, int y_start, int sub_dim0, int sub_dim1)
{
  int i,j;
  for (i = 0; i < sub_dim0; i++) {
    for (j = 0; j < sub_dim1; j++) {
      dest[i*sub_dim1+j]=array[(x_start+i)*dim1+j+y_start];
    }
  }
}

void csubmatrix (float complex array[], float complex dest[], int dim0, int dim1,
		 int x_start, int y_start, int sub_dim0, int sub_dim1)
{
  int i,j;
  for (i = 0; i < sub_dim0; i++) {
    for (j = 0; j < sub_dim1; j++) {
      dest[i*sub_dim1+j]=array[(x_start+i)*dim1+j+y_start];
    }
  }
}

void zsubmatrix (double complex array[], double complex dest[], int dim0, int dim1,
		 int x_start, int y_start, int sub_dim0, int sub_dim1)
{
  int i,j;
  for (i = 0; i < sub_dim0; i++) {
    for (j = 0; j < sub_dim1; j++) {
      dest[i*sub_dim1+j]=array[(x_start+i)*dim1+j+y_start];
    }
  }
}


void ssetsubmatrix (float array[], float source[], int dim0, int dim1,
		int x_start, int y_start, int sub_dim0, int sub_dim1)
{
  int i,j;
  for (i = 0; i < sub_dim0; i++) {
    for (j = 0; j < sub_dim1; j++) {
      array[(x_start+i)*dim1+j+y_start]=source[i*sub_dim1+j];
    }
  }
}

void dsetsubmatrix (double array[], double source[], int dim0, int dim1,
		 int x_start, int y_start, int sub_dim0, int sub_dim1)
{
  int i,j;
  for (i = 0; i < sub_dim0; i++) {
    for (j = 0; j < sub_dim1; j++) {
      array[(x_start+i)*dim1+j+y_start]=source[i*sub_dim1+j];
    }
  }
}

void csetsubmatrix (float complex array[], float complex source[], int dim0, int dim1,
		 int x_start, int y_start, int sub_dim0, int sub_dim1)
{
  int i,j;
  for (i = 0; i < sub_dim0; i++) {
    for (j = 0; j < sub_dim1; j++) {
      array[(x_start+i)*dim1+j+y_start]=source[i*sub_dim1+j];
    }
  }
}

void zsetsubmatrix (double complex array[], double complex source[], int dim0, int dim1,
		 int x_start, int y_start, int sub_dim0, int sub_dim1)
{
  int i,j;
  for (i = 0; i < sub_dim0; i++) {
    for (j = 0; j < sub_dim1; j++) {
      array[(x_start+i)*dim1+j+y_start]=source[i*sub_dim1+j];
    }
  }
}

// permutation

void srowperm (float array[], int perm[], float dest[], int dim0, int dim1)
{
  int i, j, source_pos, dest_pos; 
  for (j = 0; j < dim0; j++) {
    dest_pos = dim1*perm[j];
    source_pos = dim1*j;
    for (i = 0; i < dim1; i++) {
      dest[dest_pos] = array[source_pos];
      dest_pos++;
      source_pos++;
    }
  }
}

void drowperm (double array[], int perm[], double dest[], int dim0, int dim1)
{
  int i, j, source_pos, dest_pos; 
  for (j = 0; j < dim0; j++) {
    dest_pos = dim1*perm[j];
    source_pos = dim1*j;
    for (i = 0; i < dim1; i++) {
      dest[dest_pos] = array[source_pos];
      dest_pos++;
      source_pos++;
    }
  }
}

void crowperm (float complex array[], int perm[], float complex dest[], int dim0, int dim1)
{
  int i, j, source_pos, dest_pos; 
  for (j = 0; j < dim0; j++) {
    dest_pos = dim1*perm[j];
    source_pos = dim1*j;
    for (i = 0; i < dim1; i++) {
      dest[dest_pos] = array[source_pos];
      dest_pos++;
      source_pos++;
    }
  }
}

void zrowperm (double complex array[], int perm[], double complex dest[], int dim0, int dim1)
{
  int i, j, source_pos, dest_pos; 
  for (j = 0; j < dim0; j++) {
    dest_pos = dim1*perm[j];
    source_pos = dim1*j;
    for (i = 0; i < dim1; i++) {
      dest[dest_pos] = array[source_pos];
      dest_pos++;
      source_pos++;
    }
  }
}

// missing elementwize operations

void smplusc (float array[], float c, int size)
{
  int pos;
  for (pos = 0; pos < size; pos++)
    {
      array[pos] = array[pos] + c;
    }
}

void smmult (float array1[], float array2[], int size)
{
  int pos;
  for (pos = 0; pos < size; pos++)
    {
      array1[pos] = array1[pos] * array2[pos];
    }
}

void dmplusc (double array[], double c, int size)
{
  int pos;
  for (pos = 0; pos < size; pos++)
    {
      array[pos] = array[pos] + c;
    }
}

void dmmult (double array1[], double array2[], int size)
{
  int pos;
  for (pos = 0; pos < size; pos++)
    {
      array1[pos] = array1[pos] * array2[pos];
    }
}

void cmplusc (float complex array[], float complex c, int size)
{
  int pos;
  for (pos = 0; pos < size; pos++)
    {
      array[pos] = array[pos] + c;
    }
}

void cmmult (float complex array1[], float complex array2[], int size)
{
  int pos;
  for (pos = 0; pos < size; pos++)
    {
      array1[pos] = array1[pos] * array2[pos];
    }
}

void zmplusc (double complex array[], double complex c, int size)
{
  int pos;
  for (pos = 0; pos < size; pos++)
    {
      array[pos] = array[pos] + c;
    }
}

void zmmult (double complex array1[], double complex array2[], int size)
{
  int pos;
  for (pos = 0; pos < size; pos++)
    {
      array1[pos] = array1[pos] * array2[pos];
    }
}

// missing stats operations

int simin (float array[], int size)
{
  int pos;
  float val=array[0];
  int min_pos=0;
  float cur_val;
  for (pos = 1; pos < size; pos++) {
    cur_val = array[pos];
    if (cur_val < val) {
      val = cur_val;
      min_pos = pos;
    }
  }
  return min_pos;
}

int dimin (double array[], int size)
{
  int pos;
  double val=array[0];
  int min_pos=0;
  double cur_val;
  for (pos = 1; pos < size; pos++) {
    cur_val=array[pos];
    if (cur_val < val) {
      val = cur_val;
      min_pos = pos;
    }
  }
  return min_pos;
}

int simax (float array[], int size)
{
  int pos;
  float val=array[0];
  int max_pos=0;
  float cur_val;
  for (pos = 1; pos < size; pos++) {
    cur_val=array[pos];
    if (cur_val > val) {
      val = cur_val;
      max_pos = pos;
    }
  }
  return max_pos;
}

int dimax (double array[], int size)
{
  int pos;
  double val=array[0];
  int max_pos=0;
  double cur_val;
  for (pos = 1; pos < size; pos++) {
    cur_val=array[pos];
    if (cur_val > val) {
      val = cur_val;
      max_pos = pos;
    }
  }
  return max_pos;
}

int siamin (float array[], int size)
{
  int pos;
  float val=fabs(array[0]);
  int min_pos=0;
  float cur_val;
  for (pos = 1; pos < size; pos++) {
    cur_val=fabs(array[pos]);
    if (cur_val < val) {
      val = cur_val;
      min_pos = pos;
    }
  }
  return min_pos;
}

int diamin (double array[], int size)
{
  int pos;
  double val=fabs(array[0]);
  int min_pos=0;
  double cur_val;
  for (pos = 1; pos < size; pos++) {
    cur_val=fabs(array[pos]);
    if (cur_val < val) {
      val = cur_val;
      min_pos = pos;
    }
  }
  return min_pos;
}

int ciamin (float complex array[], int size)
{
  int pos;
  float val=fabs(array[0]);
  int min_pos=0;
  float cur_val;
  for (pos = 1; pos < size; pos++) {
     cur_val=fabs(array[pos]);
     if (cur_val < val) {
      val = cur_val;
      min_pos = pos;
    }
  }
  return min_pos;
}

int ziamin (double complex array[], int size)
{
  int pos;
  double val=fabs(array[0]);
  int min_pos=0;
  double cur_val;
  for (pos = 1; pos < size; pos++) {
    cur_val=fabs(array[pos]);
    if (cur_val < val) {
      val = cur_val;
      min_pos = pos;
    }
  }
  return min_pos;
}

float smsum (float array[], int size)
{
  int pos;
  float sum=0;
  for (pos = 0; pos < size; pos++) {
    sum += array[pos];
  }
  return sum;
}

double dmsum (double array[], int size)
{
  int pos;
  double sum=0;
  for (pos = 0; pos < size; pos++) {
    sum += array[pos];
  }
  return sum;
}

// mapping matrices

void smapmatrix (float array[], int size, float (*fn)(float x))
{
  int pos;
  for (pos=0; pos < size; pos++)
    array[pos]=(*fn)(array[pos]);
}

void dmapmatrix (double array[], int size, double (*fn)(double x))
{
  int pos;
  for (pos=0; pos < size; pos++)
    array[pos]=(*fn)(array[pos]);
}

void cmapmatrix (float complex array[], int size, float complex (*fn)(float complex x))
{
  int pos;
  for (pos=0; pos < size; pos++)
    array[pos]=(*fn)(array[pos]);
}

void zmapmatrix (double complex array[], int size, double complex (*fn)(double complex x))
{
  int pos;
  for (pos=0; pos < size; pos++)
    array[pos]=(*fn)(array[pos]);
}

// sample mapping
#|
void ssqmatrix (float array[], int size)
{
  int pos;
  for (pos=0; pos < size; pos++)
    array[pos]=tanh(array[pos]);
}

void dsqmatrix (double array[], int size)
{
  int pos;
  for (pos=0; pos < size; pos++)
    array[pos]++;
}

void csqmatrix (float complex array[], int size)
{
  int pos;
  for (pos=0; pos < size; pos++)
    array[pos]++;
}

void zsqmatrix (double complex array[], int size)
{
  int pos;
  for (pos=0; pos < size; pos++)
    array[pos]++;
}
|#
