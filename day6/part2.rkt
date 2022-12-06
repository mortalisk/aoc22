#lang racket
(require "../aoc.rkt")

(define (message pos chars)
  (if (= 14 (set-count (list->set (take chars 14))))
      pos
      (marker (+ pos 1) (drop chars 1))))

(call-with-lines "input" (compose (curry message 14) string->list first))