#lang racket

; single save slot
(define save-slot #f)

(define count 0)

(define save
  (λ (v)
    (let/cc k
      (set! save-slot k)
      v)))

(define replay
  (λ (nv)
    (let ([c save-slot])
      (if c
          (c nv)
          (error "no saves.")))))

(if (= 4 (save count))
    (begin
      (writeln "Success.")
      (if (< 7 (save count))
          (begin
            (writeln "Success 2."))
          (begin
            (writeln "Fail. Please travel back 2.")
            (set! count (add1 count))
            (replay count))))
    (begin
      (writeln "Fail. Please travel back.")
      (set! count (add1 count))
      (replay count)))
