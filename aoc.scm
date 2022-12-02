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


(define (insp val)
  (display val)
  val)


(provide (all-defined-out))