#lang racket
(require "../aoc.rkt")

(define (countstacks lines heigth)
  (if
   (number? (string->number (first (string-split (first lines)))))
   (values
    (length (string-split (first lines)))
    heigth)
   (countstacks (rest lines) (+ 1 heigth))))
      

(define (fillstacks lines stacks n)
  (if (= n 0)
   stacks
   (let* ([as-list (string->list (first lines))]
          [elems (map first (sliding (rest as-list) 4))])
     (fillstacks (rest lines) (cons elems stacks) (- n 1)))))
  

(define (readstacks lines)
  (let*-values ([(n h) (countstacks lines 0)]
                [(stacks) (fillstacks lines '() n)])
    stacks))

(define (readmoves lines)
  (let loop ([lines lines][acc '()])
    (if (null? lines)
        (reverse acc)
        (match (regexp-match #rx"move ([0-9]+) from ([0-9]+) to ([0-9]+)" (first lines))
          [#f (loop (rest lines) acc)]
          [(list _ a b c) (loop (rest lines) (cons (map string->number (list a b c)) acc))]))))

(define (transpose xss)
  (apply map list xss))

(define (move-n stacks n from to)
  (insp stacks)
  (let* ([from (- from 1)]
         [to (- to 1)]
         [from-stack (list-ref stacks from)]
         [to-stack (list-ref stacks to)]
         [items (take from-stack n)]
         [removed (list-set stacks from (drop from-stack n))]
         [added-back (list-set removed to (append items to-stack))])
  added-back))

(define (move inst stacks)
  (move-n stacks (first inst) (second inst) (third inst)))

(define (remove-spaces list)
  (filter (negate (curry char=? #\space)) list))

(define (solve lines)
  (let* ([stacks (map reverse (map remove-spaces (transpose (readstacks lines))))]
         [moves (insp (readmoves lines))]
         [after-moves (foldl move stacks moves)])
    (list->string (map first after-moves))))

(call-with-lines "input" solve)