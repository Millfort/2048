#lang racket

(provide set-random-number)

(define (new-random-number)
  (let* ([seed '(2 2 2 2 4)]
         [pos (random (length seed))])
    (list-ref seed pos)))

(define (set-random-number area)
  (let* ([x (random (length area))]
         [y (random (length area))]
         [r (new-random-number)]
         [v (list-ref (list-ref area x) y)])
    (cond [(not (zero? v)) (set-random-number area)]
          [else (list-set area x (list-set (list-ref area x) y r))])))