#lang racket
(require "../aoc.rkt")

(define inf 10)

(define (score line i)
  (define height (inspect "height" (list-ref (inspect "line" line) (inspect "i" i))))
  (define (count trees)
    (let*-values
          ([(trees) trees]
           [(_ c) (for/fold ([blocked #f][cnt 0]) ([tree trees])  
                    (if blocked
                        (values blocked cnt)
                        (values (>= tree height)  (+ cnt 1))))])
      c))
  (inspect "score" (* (count (reverse (take line i))) (count (drop line (+ i 1))))))

(define (solve by-row)
  (let* ([by-column (transpose by-row)]
         [y (length by-row)]
         [x (length by-column)])
    (apply max (for*/list ([i (in-range 0 x)]
                           [j (in-range 0 y)])
                 (inspect "total" (* (score (list-ref by-row j) i) (score (list-ref by-column i) j)))))))
               
(call-with-lines "test" (curryr do-to
                                (curry map string->list)
                                (curry map (curry map (compose (curryr - 48) char->integer)))
                                solve))