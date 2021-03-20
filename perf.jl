# This file was formerly a part of Julia. License is MIT: https://julialang.org/license
# https://github.com/JuliaLang/Microbenchmarks/blob/master/perf.jl

# using Base
using Printf
using Random
using BenchmarkTools

import LinearAlgebra

macro print_elapsed(f, name, desc)
    name = esc(name)
    desc = esc(desc)
    quote 
        Printf.@printf "julia,%s,%.9f\n" $name (@belapsed $f)
    end
end

## recursive fib ##
fib(n) = n < 2 ? n : fib(n-1) + fib(n-2)

@print_elapsed fib(20) "recursion_fibonacci" "Recursive fibonacci"


## tail recursive fib ##
helper(current, next, n) = n==0 ? current : helper(next, next+current, n-1)
tr_fib(n) = helper(Int(0), Int(1), n)

@print_elapsed tr_fib(20) "tail_recursive_fibonacci" "Recursive fibonacci"


## parse integer ##
# function parseintperf(t)
#     local n, m
#     for i=1:t
#         n = rand(UInt32)
#         s = string(n, base = 16)
#         m = UInt32(parse(Int64, s, base = 16))
#         @assert m == n
#     end
#     return n
# end

# @print_elapsed parseintperf(1000) "parse_integers" "Integer parsing"


## mandelbrot set: complex arithmetic and comprehensions ##
# function myabs2(z)
#     return real(z)*real(z) + imag(z)*imag(z)
# end

# function mandel(z)
#     c = z
#     maxiter = 80
#     for n = 1:maxiter
#         if myabs2(z) > 4
#             return n-1
#         end
#         z = z^2 + c
#     end
#     return maxiter
# end

# mandelperf() = [ mandel(complex(r,i)) for i=-1.:.1:1., r=-2.0:.1:0.5 ]
# @print_elapsed mandelperf() "userfunc_mandelbrot" "Calculation of mandelbrot set"


## numeric vector sort ##
function qsort!(a,lo,hi)
    i, j = lo, hi
    while i < hi
        pivot = a[(lo+hi)>>>1]
        while i <= j
            while a[i] < pivot; i += 1; end
            while a[j] > pivot; j -= 1; end
            if i <= j
                a[i], a[j] = a[j], a[i]
                i, j = i+1, j-1
            end
        end
        if lo < j; qsort!(a,lo,j); end
        lo, j = i, hi
    end
    return a
end

sortperf(n) = qsort!(rand(n), 1, n)
@print_elapsed sortperf(5000) "imperative_recursion_quicksort" "Sorting of random numbers using quicksort"


## slow pi series ##
function pisum()
    sum = 0.0
    for j = 1:500
        sum = 0.0
        for k = 1:10000
            sum += 1.0/(k*k)
        end
    end
    sum
end

@print_elapsed pisum() "iteration_pi_sum" "Summation of a power series"


## random matrix statistics ##
# function randmatstat(t)
#     n = 5
#     v = zeros(t)
#     w = zeros(t)
#     for i=1:t
#         a = randn(n,n)
#         b = randn(n,n)
#         c = randn(n,n)
#         d = randn(n,n)
#         P = [a b c d]
#         Q = [a b; c d]
#         @static if VERSION >= v"0.7.0" 
#             v[i] = LinearAlgebra.tr((P'*P)^4)
#             w[i] = LinearAlgebra.tr((Q'*Q)^4)
#         else
#             v[i] = trace((P'*P)^4)
#             w[i] = trace((Q'*Q)^4)
#         end
#     end
# end

# @print_elapsed randmatstat(1000) "matrix_statistics" "Statistics on a random matrix"


# ## largish random number gen & matmul ##
# @print_elapsed rand(1000,1000)*rand(1000,1000) "matrix_multiply" "Multiplication of random matrices"


## printfd ##
function printfd(n)
    open("/dev/null", "w") do io
        for i = 1:n
            Printf.@printf(io, "%d %d\n", i, i + 1)
        end
    end
end

@print_elapsed printfd(100000) "print_to_file" "Printing to a file descriptor"
