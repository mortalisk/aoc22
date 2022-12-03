#lang racket
(require "../aoc.rkt")

(define (game-score round)
  (let ([a (first round)]
        [b (second round)])
    (cond [(= a b) 3]
          [(= b 3) (if (= a 1) 0 6)]
          [(= a 3) (if (= b 1) 6 0)]
          [(< a b) 6]
          [(> a b) 0])))

(define (score round)
  (+ (game-score round) (second round)))
    

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
