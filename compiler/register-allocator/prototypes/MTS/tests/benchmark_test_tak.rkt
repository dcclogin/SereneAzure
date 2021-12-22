(define (tak [x : Integer] [y : Integer] [z : Integer]) : Integer
  (if (>= y x)
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(tak 16 8 4)
