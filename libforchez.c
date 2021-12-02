#include <stdio.h>
#include <time.h>

double clock_now()
{
  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);
  return (double)(now.tv_sec) + (double)(now.tv_nsec/1.0e9);
}

void internal_printf(const char* impl, const char* test_name, double bench_result)
{
  printf("%s,%s,%.9f\n", impl, test_name, bench_result);
}
