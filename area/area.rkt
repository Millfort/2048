#lang racket

(require "merge.rkt"
         "states.rkt"
         "random-number.rkt")

(provide area%)

(define area% 
  (class object%
    (init size)
    (define area-size size)
    (define area (set-random-number (set-random-number (new-empty-area size))))
      
    (super-new)
      
    (define/public (get) area)
    
    (define/public (merge-up) (set! area (merge-up-area area)))
    (define/public (merge-down) (set! area (merge-down-area area)))
    (define/public (merge-left) (set! area (merge-left-area area)))
    (define/public (merge-right) (set! area (merge-right-area area)))

    (define/public (add-random-number) (cond
                                         [(area-not-filled area) (set! area (set-random-number area)) #t]
                                         [else #f]))

    (define/public (is-fail) (area-is-fail area))
    (define/public (is-win) (area-is-win area)) ))


(define (new-empty-area size)
  (build-list size (lambda (x)
                     (build-list size (lambda (y) 0)))))









