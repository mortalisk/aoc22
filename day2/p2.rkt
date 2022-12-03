#lang racket
(require "../aoc.rkt")

(define (choice round)
  (let ([a (first round)]
        [b (second round)])
    (cond [(= b 2) a]
          [(= b 1) (if (= a 1) 3 (- a 1))]
          [else (if (= a 3) 1 (+ a 1))])))

(define (game-score a b)
  (cond [(= a b) 3]
        [(= b 3) (if (= a 1) 0 6)]
        [(= a 3) (if (= b 1) 6 0)]
        [(< a b) 6]
        [(> a b) 0]))

(define (score round)
  (let ([b (choice round)]
        [a (first round)])
  (+ (game-score a b) b)))
    

(define (to-num str)
  (cond [(equal? str "A") 1]
        [(equal? str "B") 2]
        [(equal? str "C") 3]
        [(equal? str "X") 1]
        [(equal? str "Y") 2]
        [(equal? str "Z") 3]))

(define (solve lines)
  (let ([l (map (curry map to-num) (map string-split lines))])
    (display (apply + (map score l)))))
    
(call-with-lines "input" solve)
