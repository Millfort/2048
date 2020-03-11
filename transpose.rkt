#lang racket

(provide transpose)

(define (cars matrix)
  "Return a list with all the cars of the lists in matrix"
  (if (null? matrix)
      '()
      (cons (car (car matrix)) (cars (cdr matrix)))))

(define (cdrs matrix)
  "Return a list with all the cdrs of the lists in matrix"
  (if (null? matrix)
      '()
      (cons (cdr (car matrix)) (cdrs (cdr matrix)))))

(define (transpose matrix)
  "Transpose matrix"
  (cond ((null? matrix) '())
        ((null? (car matrix)) '())
        (#t (cons (cars matrix) (transpose (cdrs matrix))))))