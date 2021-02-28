#include <time.h>
#include <stdio.h>

double clock_now()
{
  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);
  return (double)(now.tv_sec) + (double)(now.tv_nsec/1.0e9);
}

/* int fib(int n) { */
/*   return n < 2 ? n : fib(n-1) + fib(n-2); */
/* } */
int fib(int n) {
  if (n < 2) return 1;
  
  int sum = 0, i;

  for(i = 1; i <= n; i++) {
    sum += i;
  } 

  return sum;
}

int main(int argc, char **argv) { 
  double t1, t2;
  t1 = clock_now();
  fib(30);
  t2 = clock_now();

  printf("%.16f\n", t2-t1);

  return 0;
}
