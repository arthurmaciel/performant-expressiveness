(define-library (cyclone printf)
  (import (scheme base)
          (cyclone foreign))
  (export printf)
  (begin
    (c-define _printf double "printf" string string string double)
    (define printf _printf)))
