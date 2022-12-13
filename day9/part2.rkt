#lang racket

(require "../aoc.rkt")


(define (single-moves line)
  (match (regexp-match #rx"(.) ([0-9]+)" line)
    [(list _ "R" n) (make-list (string->number n) (list 1 0))]
    [(list _ "L" n) (make-list (string->number n) (list -1 0))]
    [(list _ "U" n) (make-list (string->number n) (list 0 1))]
    [(list _ "D" n) (make-list (string->number n) (list 0 -1))]))

(define (distance a b)
  (apply max (map abs (map - a b))))

(define (subtract a b)
  (map - a b))

(define (add a b)
  (map + a b))

(define (cap n)
  (if (> n 0) 1 (if (< n 0) -1 0)))

(define (tail-move h t)
  (if (> (distance h t) 1)
      (add (map cap (subtract h t)) t)
      t))

(define (visits lines)
  (let ([moves (apply append (map single-moves lines))])
    (for/fold ([visited (list)][h (list 0 0)][ts (make-list 9 (list 0 0))]
               #:result visited)
              ([move moves])
      (let* ([h (add h move)]
            [ts (foldl (tail-move h t)])
        (values (cons t visited) h t)))))

(define (solve lines)
  (let ([visited (visits lines)])
       (set-count (list->set visited))))


(call-with-lines "input" solve)