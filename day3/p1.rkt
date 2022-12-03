#lang racket
(require "../aoc.rkt")
(require racket/set)

(define (char->priority) 1)

(define (split-midle str)
  (define-values 
    (a b)
    (split-at (string->list str) (/ (string-length str) 2)))
  (list a b))

(define (priority line)
  (let*
    ([parts (split-midle line)]
     [c (first (set-intersect (first parts) (second parts)))]
     [i (char->integer c)])
    (if (> i 96) (- i 96) (- i 38))))

(define (solve1 lines)
  (let 
    ([priorities (map priority lines)])
    (apply + priorities)))

(call-with-lines "input" solve1)