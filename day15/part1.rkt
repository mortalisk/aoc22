#lang racket

(require "../aoc.rkt")

(define (parse line)
  (match line
    [(regexp "Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)" (list _ x y bx by)) (map string->number (list x y bx by))]))

(define (manhattan lockon)
    (match
      lockon
    [(list x y bx by)
     (+ (abs (- x bx)) (abs (- y by)))]))

(define (mark n lockon line)
  (let* ([sensorrange (manhattan lockon)]
        [distline (abs (- n (second lockon)))]
        [x (first lockon)]
        [y (second lockon)]
        (by (fourth lockon))
        [bx (third lockon)]
        [linerange (- sensorrange distline)]
        [start (- x linerange)]
        [end (+ x linerange)]
        )
    #;(x (inspect "line" lockon)
    (inspect "sensorrange" sensorrange)
    (inspect "distline" distline)
    (inspect "linerange" linerange)
    (inspect "start" start)
    (inspect "end" end)
    (insp ""))
    (if
     (> distline sensorrange)
     line
     (foldl (lambda (x line)
              (if (not (and (= x bx) (= n by))) 
                  (hash-set line x "#")
                  line))
            line
            (range start (+ end 1))))))

(define (solve n lines)
  (let* ([lockons (map parse lines)]
        [line (foldl (curry mark n) (hash) lockons)])
    (hash-count line)))
      
(call-with-lines "input" (curry solve 2000000))