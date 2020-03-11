#lang racket

(require "../transpose.rkt")

(provide merge-up-area
         merge-down-area
         merge-left-area
         merge-right-area)

(define (merge-up-area area)
  (transpose (merge-left (transpose area))))

(define (merge-down-area area)
  (transpose (merge-right (transpose area))))

(define (merge-left-area area)
  (map merge-line-left area))

(define (merge-right-area area)
  (map merge-line-right area))


(define (merge-right a)
  (map merge-line-right a))

(define (merge-left a)
  (map merge-line-left a))

(define (merge-line-left line)
  (let ([merge-result (merge line)])
    (append merge-result (make-list (- (length line) (length merge-result)) 0))))

(define (merge-line-right line)
  (let ([merge-result (reverse (merge (reverse line)))])
    (append  (make-list (- (length line) (length merge-result)) 0) merge-result)))
  

(define (merge line)
  (filter-not zero? (foldl merge-aux '() (filter-not zero? line))))

(define (merge-aux elem result)
  (cond
    ; first element is added as is
    [(empty? result) (cons elem '())]
    ; if the elements are equal, 
    ; they will be folded and 0 will be added to them, 
    ; so as not to fold this element with the subsequent one, 
    ; i.e., so that the addition for one element works only once.
    [(eq? (last result) elem) (append (drop-right result 1) (list (* elem 2) 0))]
    ; if not equal, just add to the list.
    [else (append result (list elem))]))