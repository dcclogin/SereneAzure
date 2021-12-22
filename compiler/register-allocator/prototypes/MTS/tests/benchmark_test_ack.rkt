(define (ack [m : Integer] [n : Integer]) : Integer
  (if (eq? m 0)
      (+ n 1)
      (if (eq? n 0)
          (ack (- m 1) 1)
          (ack (- m 1) (ack m (- n 1))))))

(ack 3 4)
