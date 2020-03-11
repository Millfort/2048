#lang racket

(require "../transpose.rkt"
         "../zip.rkt")

(provide area-not-filled
         area-is-win
         area-is-fail)

(define (area-not-filled area)
  (ormap (lambda (line)
           (ormap (lambda (x)
                    (eq? x 0)) line)) area))

(define (area-is-win area)
  (ormap (lambda (line)
           (ormap (lambda (x)
                    (eq? x 2048)) line)) area))

(define (area-is-fail area)
  (and (andmap (lambda (line) (is-line-fail line)) area)
       (andmap (lambda (line) (is-line-fail line)) (transpose area))))

(define (is-line-fail line)
  ; !(if there are 2 identical elements next to each other or if there are zeros (blanks))
  (let ([ziped (zip line)])
    (andmap (lambda (p)
              (not (or (eq? (car p) (cdr p))
                       (zero? (car p))
                       (zero? (cdr p))))) ziped)))