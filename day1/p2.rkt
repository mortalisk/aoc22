#lang racket
(require "../aoc.rkt")

(define make-totals 
  (lambda (lines)
    (for/fold ([totals '(0)])
              ([line lines])
       (if (equal? line "")
           (cons 0 totals)
           (cons
            (+ (car totals) (string->number line))
            (cdr totals))))))

(define solve
  (lambda (lines)
    (let ([sorted (sort (make-totals lines) >)])
      (+ (car sorted)
         (cadr sorted)
         (caddr sorted)))))


(call-with-lines "input1" solve)