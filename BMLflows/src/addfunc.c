void add(int *x, int *y, int *n, int *out){
  
  for (int i = 0; i < *n; i++){
    out[i] = x[i] + y[i];
  }
}