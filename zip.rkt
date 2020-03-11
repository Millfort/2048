#lang racket

(provide zip)

(define (zip lst)
  "make pairs (zip '(2 2 3 4 5)) => '((2 . 2) (2 . 3) (3 . 4) (4 . 5))"
  (if (>= (length lst) 2)
      (cons (cons (car lst) (cadr lst))
            (zip (cdr lst)))
      '()))