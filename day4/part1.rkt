#lang racket
(require "../aoc.rkt")

(define (line->range line)
  (let* ([s-comma (string-split line ",")]
         [s-dash (map (curryr string-split "-") s-comma)]
         [as-number (map (curry map string->number) s-dash)]
         [inc-second (map (lambda (l) (list (first l) (+ 1 (second l)))) as-number)]
         )
    (map (curry apply range) inc-second)
    ))

(define (either-contains-other a b)
  (or (subset? a b) (subset? b a)))

(define (solve lines)
  (let ([ranges (map line->range lines)])
    (count (curry apply either-contains-other) ranges)))

(call-with-lines "input" solve)