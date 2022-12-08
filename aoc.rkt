#lang racket

(define call-with-lines
  (lambda (file func)
    (with-input-from-file file
      (lambda ()
        (let loop ((lines '())
                   (next-line (read-line)))
          (if (eof-object? next-line) ; when we hit the end of file
              (func (reverse lines))         ; return the lines
              (loop (cons next-line lines) ; else loop, keeping this line
                    (read-line))))))))       ; and move to next one the file's contents

(define (my-drop list n)
  (if (or (empty? list) (= n 0)) list (my-drop (rest list) (- n 1))))

(define (my-take list n)
  (let loop ([list list][n n][acc '()])
    (if (or (empty? list) (= n 0)) (reverse acc) (loop (rest list) (- n 1) (cons (first list) acc)))))

(define (sliding list n)
  (let loop ([list list][acc empty])
    (if (null? list) 
      (reverse acc)
      (loop (my-drop list n) (cons (my-take list n) acc)))))

(define (transpose xss)
  (apply map list xss))

(define (insp val)
  (display val)
  (display "\n")
  val)

(define (inspect msg val)
  ;(display msg)
  ;(display ": ")
  ;(display val)
  ;(display "\n")
  val)

(define (do-to item . funcs)
  (foldl (lambda (func item) (func item)) item funcs))

(provide (all-defined-out))