#include <time.h>
#include <stdio.h>
#include <stdlib.h>

double clock_now()
{
  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);
  return (double)(now.tv_sec) + (double)(now.tv_nsec/1.0e9);
}

int fib(int n) {
  return n < 2 ? n : fib(n-1) + fib(n-2);
}

int iterative_fib(int n) {
  if (n < 2) return 1;
  
  int sum = 0, i;

  for(i = 1; i <= n; i++) {
    sum += i;
  } 

  return sum;
}

double *rand_double_array(int n) {
  double *d = (double *)malloc(n*sizeof(double));
  for(int i = 0; i <n; i++) {
    d[i] = rand()%1;
  }
  return d;
}

void quicksort(double *a, int lo, int hi) {
  int i = lo;
  int j = hi;
  while (i < hi) {
    double pivot = a[(lo+hi)/2];
    // Partition
    while (i <= j) {
      while (a[i] < pivot) {
        i = i + 1;
      }
      while (a[j] > pivot) {
        j = j - 1;
      }
      if (i <= j) {
        double t = a[i];
        a[i] = a[j];
        a[j] = t;
        i = i + 1;
        j = j - 1;
      }
    }

    // Recursion for quicksort
    if (lo < j) {
      quicksort(a, lo, j);
    }
    lo = i;
    j = hi;
  }
}

double pisum() {
  double sum = 0.0;
  for (int j=0; j<500; ++j) {
    sum = 0.0;
    for (int k=1; k<=10000; ++k) {
      sum += 1.0/(k*k);
    }
  }
  return sum;
}

void printfd(int n) {
  FILE *f = fopen("/dev/null", "w");
  long i = 0;
  for (i = 0; i < n; i++)
    fprintf(f, "%ld %ld\n", i, i+1);
  fclose(f);
}

long parse_int(const char *s, long base) {
  long n = 0;
  for (; *s; ++s) {
    char c = *s;
    long d = 0;
    if (c >= '0' && c <= '9') d = c-'0';
    else if (c >= 'A' && c <= 'Z') d = c-'A' + (int) 10;
    else if (c >= 'a' && c <= 'z') d = c-'a' + (int) 10;
    else exit(-1);

    if (base <= d) exit(-1);
    n = n*base + d;
  }
  return n;
}

void print_perf(char *test, double time) {
  printf("%s,%s,%.9f\n", "C", test, time);
}

int main(int argc, char **argv) { 
  double t;

  tmin = INFINITY;
  for (int i=0; i<NITER; ++i) {
    t = clock_now();
    fib(30);
    t = clock_now()-t;
    if (t < tmin) tmin = t;
  }
  print_perf("recursion_fibonacci", t);

  tmin = INFINITY;
  for (int i=0; i<NITER; ++i) {
    t = clock_now();
    iterative_fib(30);
    t = clock_now()-t;
    if (t < tmin) tmin = t;
  }
  print_perf("iteration_fibonacci", t);

  tmin = INFINITY;
  for (int i=0; i<NITER; ++i) {
    t = clock_now();
    pisum();
    t = clock_now()-t;
    if (t < tmin) tmin = t;
  }
  print_perf("iteration_pi_sum", t);

  tmin = INFINITY;
  for (int i=0; i<NITER; ++i) {
    t = clock_now();
    printfd(100000);
    t = clock_now()-t;
    if (t < tmin) tmin = t;
  }
  print_perf("print_to_file", t);

  tmin = INFINITY;
  for (int i=0; i<NITER; ++i) {
    double *d = rand_double_array(5000);
    t = clock_now();
    quicksort(d, 0, 5000-1);
    t = clock_now()-t;
    if (t < tmin) tmin = t;
  }
  print_perf("imperative_recursion_quicksort", t);

  tmin = INFINITY;
  for (int i=0; i<NITER; ++i) {
    t = clock_now();
    char s[11];
    for (int k=0; k<1000; ++k) {
      uint32_t n = rand();
      sprintf(s, "%x", n);
      uint32_t m = (uint32_t)parse_int(s, 16);
      assert(m == n);
    }
    t = clock_now()-t;
    if (t < tmin) tmin = t;
  }
  print_perf("parse_integers", t);
  
  return 0;
}
