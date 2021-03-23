(c-declare #<<c-declare-end
#include <stdio.h>
#include <time.h>
c-declare-end
)

(define perf-counter
  (c-lambda ()
            double 
#<<c-lambda-end
   struct timespec now;
   clock_gettime(CLOCK_MONOTONIC, &now);
   ___return((double)(now.tv_sec) + (double)(now.tv_nsec/1.0e9));
c-lambda-end
))

(define printf
  (c-lambda (nonnull-char-string 
             nonnull-char-string
             nonnull-char-string
             double)
            void
   "printf"))

(define-syntax elapsed
  (syntax-rules ()
    ((_ proc)
     (elapsed 5 proc))
    ((_ n proc)
     (let loop ((i 0)
                (min-time (expt 2 100)))
       (if (= i n)
           min-time
           (begin
             (set! t1 (perf-counter))        
             proc
             (set! t2 (- (perf-counter) t1))
             (loop (+ i 1)
                   (min min-time t2))))))))

(define (print-perf test-name bench-result)
  (printf "%s,%s,%.9f\n" "gambit" test-name bench-result))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 2)) (fib (- n 2)))))

(print-perf "recursion_fibonacci" (elapsed (fib 20)))

(define (iterative-fib n)
  (do ((n n (- n 1))
       (current 0 next)
       (next 1 (+ next current)))
      ((= n 0) current)))

(print-perf "iteration_fibonacci" (elapsed (iterative-fib 20)))

(define (list-ref lst n)
  (let loop ((i 1) (lst lst))
    (if (= i n)
        (car lst)
        (loop (+ i 1) (cdr lst)))))

(define filter
  (lambda (pred lst)
    (cond ((null? lst) '())
          ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
          (else (filter pred (cdr lst))))))

(define (quicksort lst)
  (cond
   ((or (null? lst)                    ; empty list is sorted
        (null? (cdr lst)))             ; single-element list is sorted
    lst)
   (else
    (let ((pivot (list-ref lst
                           (floor (/ (length lst) 2))))
          (rest (cdr lst)))
      (append
       (quicksort        ; Recursively sort the list of smaller values
        (filter (lambda (x) (< x pivot)) rest)) ; Select the smaller values
       (list pivot)       ; Add the pivot in the middle
       (quicksort         ; Recursively sort the list of larger values
        (filter (lambda (x) (>= x pivot)) rest))))))) ; Select the larger and equal values

(define-syntax while
  (syntax-rules ()
    ((while condition body ...)
     (let loop ()
       (when condition
         body ...
         (loop))))))

(define (random-list n)
  (let loop ((i 0)
             (lst '()))
    (if (= i n)
        lst
        (loop (+ i 1) (cons (random-real) lst)))))

(print-perf "functional_recursion_quicksort" (elapsed (quicksort (random-list 5000))))

(define (quicksort! vec lo hi)
  (define i lo)
  (define j hi)
  (while (< i hi)
         (let ((pivot (vector-ref vec (floor (/ (+ lo hi) 2)))))
           (while (<= i j)
                  (while (< (vector-ref vec i) pivot)
                         (set! i (+ i 1)))
                  (while (> (vector-ref vec j) pivot)
                         (set! j (- j 1)))
                  (when (<= i j)
                    (let ((e (vector-ref vec i)))
                      (vector-set! vec i (vector-ref vec j))
                      (vector-set! vec j e)
                      (set! i (+ i 1))
                      (set! j (- j 1)))))
           (when (< lo j)
             (quicksort! vec lo j))
           (set! lo i)
           (set! j hi)))
  vec)

(define (random-vector n)
  (do ((v (make-vector n))
       (i 0 (+ i 1)))
      ((= i n) v)
    (vector-set! v i (random-real))))

(print-perf "imperative_recursion_quicksort" (elapsed (quicksort! (random-vector 5000) 0 4999)))

(define-syntax for
  (syntax-rules (in as)
    ((for element in list
          body ...)
     (for-each (lambda (element)
                 body ...)
               list))
    ((for list as element
          body ...)
     (for element in list
          body ...))))

(define (pisum)
  (define sum 0.0)
  (for j in (iota 500 1)
       (set! sum 0.0)
       (for k in (iota 10000 1)
            (set! sum (+ sum (/ 1.0 (* k k))))))
  sum)

(print-perf "iteration_pi_sum" (elapsed (pisum)))

(define (printfd n)
  (with-output-to-file "/dev/null"
    (lambda ()
      (let loop ((i 0))
        (when (< i n)
          (display i)
          (display " ")
          (display (+ i 1))
          (newline)
          (loop (+ i 1)))))))

(print-perf "print_to_file" (elapsed (printfd 100000)))

(define (parse-int t)
  (for i in (iota t 1)
       (let* ((n (random-integer (- (expt 2 32) 1)))
              (s (number->string n 16))
              (m (string->number s 16)))
         (unless (= n m) (error "Incorrect parsing of integers!"))
         n)))

(print-perf "parse_integers" (elapsed (parse-int 1000)))



