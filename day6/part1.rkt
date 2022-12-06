#lang racket
(require "../aoc.rkt")

(define (marker pos chars)
  (if (= 4 (set-count (list->set (take chars 4))))
      pos
      (marker (+ pos 1) (drop chars 1))))

(call-with-lines "test" (compose (curry marker 4) string->list first))