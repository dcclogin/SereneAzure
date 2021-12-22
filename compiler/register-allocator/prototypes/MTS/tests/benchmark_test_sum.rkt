(define (sum [n : Integer] [r : Integer]) : Integer
  (if (eq? n 0)
      r
      (sum (- n 1) (+ n r))))

(+ (sum 5 0) 27)
