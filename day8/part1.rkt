#lang racket
(require "../aoc.rkt")

(define (visible x y row column)
  (let* ([val (list-ref row x)]
         [can-see (lambda (line i) (or (null? (filter (curry <= val) (take i line))) (null? (filter (curry <= val) (drop line (+ i 1))))))])
  (or (can-see row x) (can-see column y))))

(define (solve by-row)
  (let* ([by-column (transpose by-row)]
         [y (length by-row)]
         [x (length by-column)])
    (for*/fold ([count 0])
               ([i (in-range 1 (- x 1))]
                [j (in-range 1 (- y 1))]
                #:when (visible i j (list-ref by-row j) (list-ref by-column i)))
      (+ count 1))))
               
(call-with-lines "test" (curryr do-to
                                (curry map string->list)
                                (curry map (curry map (compose (curryr - 48) char->integer)))
                                solve))