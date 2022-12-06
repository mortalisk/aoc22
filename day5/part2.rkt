#lang racket
(require "../aoc.rkt")

(define (transpose xss)
  (apply map list xss))

(define (move-n stacks n from to)
  stacks
  (let* ([items (take (list-ref stacks from) n)]
         [removed (list-set stacks from (drop (list-ref stacks from) n))])
    (list-set removed to (append items (list-ref stacks to)))))

(define (move line stacks)
  (match (regexp-match #rx"move ([0-9]+) from ([0-9]+) to ([0-9]+)" line)
          [#f stacks]
          [(list _ a b c) (let loop ([a (string->number a)][b (- (string->number b) 1)][c (- (string->number c) 1)])
                            (move-n stacks a b c))]))

(define (readstacks lines)
  (let* ([by-line (map (curry map first) (map (curryr sliding 4)  (map (curryr drop 1) (map string->list (filter (curryr string-contains? "[") lines)))))]
         [by-column (transpose by-line)]
         [no-spaces (map (curry filter (negate (curry char=? #\space))) by-column)])
  no-spaces))

(define (solve lines)
  (let* ([stacks (readstacks lines)]
         [after-moves (foldl move stacks lines)])
    (list->string (map first after-moves))))

(call-with-lines "input" solve)