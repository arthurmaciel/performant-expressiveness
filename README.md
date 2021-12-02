# Performant expressiveness
Benchmarks investigating performance and expressiveness

Julia has a series of [microbenchmarks](https://julialang.org/benchmarks/) created mainly to compare its performance against languages used for numerical analysis.

On the other side, the aim of the PE benchmarks is to compare languages emphasizing _expressiveness and performance_. Stated more clearly: this is an _aesthetically_ curated set of benchmarks.

Currently the chosen languages match the author's preferences. The idea is to expand these microbenchmarks to encompass relevant and beautiful algorithms in different languages.

We assume as good criteria to consider some code as "expressive":
- fewer lines of code;
- faster inteligibility (e.g. new programmers can _understand_ the meaning, execution and result of a piece of code with less thinking time).

Of course these are terms subject to controversy. The idea is not to estimulate definition wars (and no other type of flame war). The benchmarks are meant to aggregate beautiful and performant code.

Contributions are very welcomed!

---------- 
For a quick reference on performance, note how a _recursive_ algorithm to compute the first 20 Fibonacci numbers runs in two Scheme implementations (Chez, Cyclone and Gambit)...

```Scheme 
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 2)) (fib (- n 2)))))
```
and in Julia...

```Julia
fib(n) = n < 2 ? n : fib(n-1) + fib(n-2)
```
faster than in C!
```C
int fib(int n) {
  return n < 2 ? n : fib(n-1) + fib(n-2);
}
```

![Graph](/benchmarks.png) 

The graph was generated in a Intel i5 4GB RAM Linux 5.4.0-89-generic #100-Ubuntu SMP Fri Sep 24 14:50:10 UTC 2021 x86_64 x86_64 x86_64 GNU/Linux. For raw data in seconds see [bench.csv](https://github.com/arthurmaciel/performant-expressiveness/blob/master/bench.csv).
