#lang racket
(require "../aoc.rkt")
(require racket/set)

(define (priority c)
  (let ([i (char->integer c)])
    (if (> i 96) (- i 96) (- i 38))))

(define (sliding list n)
  (let loop ([list list][acc empty])
    (if (null? list) 
      (reverse acc)
      (loop (drop list n) (cons (take list n) acc)))))


(define (solve2 lines)
  (let*
    ([lists (map string->list lines)]
     [groups (sliding lists 3)]
     [badges (map first (map (curry apply set-intersect) groups))]
     [priorities (map priority badges)])
    (apply + priorities)))

(call-with-lines "input" solve)