# Performant expressiveness
Benchmarks investigating performance and expressiveness

Julia has a series of [microbenchmarks](https://julialang.org/benchmarks/) created mainly to compare its performance against languages used for numerical analysis.

On the other side, the aim of the PE benchmarks is to compare languages emphasizing _expressiveness and performance_. Stated more clearly: this is an _aesthetically_ curated set of benchmarks.

Currently the only languages chosen match the author's preferences. The idea is to expand these microbenchmarks to encompass relevant and beautiful algorithms in different languages.

We assume as good criteria to consider some code as "expressive":
- fewer numbers of lines of code;
- faster inteligibility (e.g. new programmers can _understand_ the meaning, execution and result of a piece of code with less thinking time).

Of course these are terms subject to controversy. The idea is not to estimulate definition (or preference) wars. It is just to aggregate beautiful and performant code.

The project is at its very beggining, so all contributions are very welcomed!

For a quick reference on performance, note how a _recursive_ algorithm to compute the first 20 Fibonacci numbers runs in two Scheme implementations (Cyclone and Gambit)...

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
