#lang racket

(require "../aoc.rkt")

(define (line->points line)
  (map (compose (curry map string->number) (curryr string-split ",")) (string-split line " -> " )))

(define (addrange p points)
  (let ([ax (first (first points))]
        [ay (second (first points))]
        [bx (+ 1 (first  p))]
        [by (+ 1 (second p))])
    (for/fold ([points points]) ([x (range ax bx)][y (range ay by)])
      (cons (list x y) points))))

(define (expand points)
  (foldl addrange (list (first points)) points))

(define (add2cave points cave)
  (foldl (lambda (p h) (hash-set h p "#")) (hash) points))

(define (solve lines)
  (let* ([endpoints (map line->points lines)]
         [points (map expand endpoints)]
         [cave (foldl add2cave (hash) (insp points))])
    cave))


(call-with-lines "input" solve)