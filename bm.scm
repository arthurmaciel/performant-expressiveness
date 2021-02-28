(import (scheme base)
        (only (scheme write) display)
        (cyclone foreign)
        (srfi 27) ;; random numbers
        )
(include-c-header "<time.h>")

(c-define printf int "printf" string double)

(define-c clock-now
  "(void *data, int argc, closure _, object k)"
  "struct timespec now;
   clock_gettime(CLOCK_MONOTONIC, &now);
   make_double(res, ((double)(now.tv_sec) + (double)(now.tv_nsec/1.0e9)));
   return_closcall1(data, k, &res);")

(define-syntax time
  (syntax-rules ()
    ((time proc)
     (let* ((t1 (clock-now))
            (proc)
            (t2 (- (clock-now) t1)))
       (printf "%.9f\n" t2)
       t2))))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (fib n)
  (do ((n n (- n 1))
       (current 0 next)
       (next 1 (+ next current)))
      ((= n 0) current)))

(let* ((t1 (clock-now))
       (fib 30)
       (t2 (- (clock-now) t1)))
  (printf "%.16f\n" t2)
  (newline))

(define (quicksort lst)
  (cond
   ((or (null? lst)                    ; empty list is sorted
        (null? (cdr lst)))             ; single-element list is sorted
    lst)
   (else
    (let ((pivot (/ (length lst) 2)) ; Select the first element as the pivot
          (rest (cdr lst)))
      (append
       (quicksort        ; Recursively sort the list of smaller values
        (filter (lambda (x) (< x pivot)) rest)) ; Select the smaller values
       (list pivot)       ; Add the pivot in the middle
       (quicksort         ; Recursively sort the list of larger values
        (filter (lambda (x) (>= x pivot)) rest))))))) ; Select the larger and equal values

;; (define (quicksort! vec lo hi)
;;   (define i lo)
;;   (define j hi)
;;   (let loop ((i lo) (j hi))
;;     (cond ((< i hi)
;;            (let ((pivot (vector-ref (/ (+ lo hi) 2))))
;;              (let loop2 ((k i)))))

;;           ))
;;   )
(define (random-list n)
  (let loop ((i 0)
             (lst '()))
    (if (= i n)
        lst
        (loop (+ i 1) (cons (random-real) lst)))))

;; (define (faster-random-list n)
;;   (define lst (make-list n))
;;   (let loop ((i 0))
;;     (if (= i n)
;;         lst
;;         (loop (+ i 1) ( (random-real) lst)))))

(let* ((t1 (clock-now))
       (quicksort (random-list 5000))
       (t2 (- (clock-now) t1)))
  (printf "%.16f\n" t2)
  (newline))


