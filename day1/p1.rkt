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
    (car (sort (make-totals lines) >))))


(call-with-lines "input1" solve)