#lang racket
(require "../aoc.rkt")

(define (update-dir dirs dir size)
  (if (null? dir) dirs (update-dir (hash-update dirs dir (curry + (string->number size))) (rest dir) size)))

(define (calc-dirs current-dir dirs lines)
   (let loop ([current-dir current-dir][dirs dirs][lines lines])
     ;(if (not (null? lines)) (insp (list (first lines) "->" dirs)) (insp "done"))
     (if (null? lines)
         dirs
         (match (first lines)
           [(regexp #rx"\\$ cd \\.\\." (list _)) (loop (rest current-dir) dirs (rest lines))]
           [(regexp #rx"\\$ cd (.*)" (list _ dir)) (loop (cons dir current-dir) (hash-set dirs (cons dir current-dir) 0) (rest lines))]
           [(regexp #rx"([0-9]+) .*" (list _ size)) (loop current-dir (update-dir dirs current-dir size) (rest lines))]
           [_ (loop current-dir dirs (rest lines))]))))

(call-with-lines "input"
                 (lambda (lines)
                   (let* ([dirs (calc-dirs '() (hash) lines)])
                     (foldl (lambda (item acc) (if (>= 100000 (cdr item)) (+ acc (cdr item)) acc)) 0 (insp (hash->list dirs))))))