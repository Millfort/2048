#lang racket/gui

(require "area/area.rkt")

(define BLOCK-WIDTH 60)
(define BLOCK-GAP 2)

(define frame 0)
(define canvas 0)
(define lose-dialog 0)

; * * * * * * * *
; * 2048 canvas *
; * * * * * * * *

(define 2048-canvas%
  (class canvas%
    (init size)
    (define area-size size)
    (define area (new area% [size area-size]))
    (define prev-area (send area get))
    (define/public (render) (begin (send (send this get-dc) clear) (render-area (send area get) (send this get-dc))))
    (define/public (update) (begin
      (send area add-random-number)
      (cond
        [(send area is-fail) (send lose-dialog show #t)]
        [else (send this render)])))
    (define/public (reset) (begin (set! area (new area% [size area-size])) (send this render)) )
    (define/override (on-char event)
      (match (send event get-key-code)
        ['up (send area merge-up)]
        ['down (send area merge-down)]
        ['left (send area merge-left)]
        ['right (send area merge-right)]
        [_ #t] )
      (match (send event get-key-code)
        [(or 'up 'down 'left 'right)
         #:when (not (equal? (send area get) prev-area))
         (begin
           (send this update)
           (set! prev-area (send area get)))]
        [_ #t]))
    (super-new)))

(define (render-area area dc)
  (define line 0)
  (for-each
   (lambda (x)
     (render-line x line dc)
     (set! line (+ line 1))) area))

(define (render-line line-data line dc)
  (define elem 0)
  (for-each
   (lambda (x)
     (render-element x line elem dc)
     (set! elem (+ elem 1))) line-data))

(define text-size-dc (new bitmap-dc% [bitmap (make-object bitmap% 60 60)]))

(define (render-element num line pos dc)
  (if (> num 0) (let ()
                  (send dc set-brush "gray" 'solid)
                  (send dc set-pen "black" 2 'solid)
                  (define x (+ (* pos BLOCK-WIDTH) (* (+ pos 1) BLOCK-GAP)))
                  (define y (+ (* line BLOCK-WIDTH) (* (+ line 1) BLOCK-GAP)))
                  (send dc draw-rectangle x y BLOCK-WIDTH BLOCK-WIDTH)
                  (define-values (text-width text-height t1 t2) (send text-size-dc get-text-extent "2048"))
                  (define x3 (+ (center BLOCK-WIDTH text-width) x))
                  (define y3 (+ (center BLOCK-WIDTH text-height) y))
                  (send dc draw-text (number->string num) x3 y3)) #t))

(define (center outer inner)
  (/ (- outer inner) 2))

; * * * * * * * * * * * * * * *
; * initialization functions *
; * * * * * * * * * * * * * * *

(define (calc-size block-count)
  (+
    (* block-count BLOCK-WIDTH)
    (* 
      (+ block-count 1) 
      BLOCK-GAP)))


(define (init-canvas block-count)
  (new 2048-canvas%
                    [parent frame]
                    [size block-count]
                    [paint-callback (lambda (canvas dc) (send canvas render))] ))
  

(define (init-frame block-count)
  (let ([size (calc-size block-count)])
    (new frame%
      [label "2048"]
      [width size]
      [height size])))

(define (new-game block-count)
  (begin 
  (set! frame (init-frame block-count))
  (set! canvas (init-canvas block-count))
  (set! lose-dialog (init-lose-dialog))
  (send frame show #t)
  (send area-size-dialog show #f)))

; * * * * * * * * 
; * Lose window *
; * * * * * * * * 

(define (create-lose-dialog)
  (new dialog% 
    [label "You lose"] 
    [width 300] 
    [height 100] 
    [parent frame] 
    [style (list 'no-caption)]))

(define (init-lose-dialog)
  (let ([dialog (create-lose-dialog)])
    (new message% 
      [parent dialog] 
      [label "You lose :("])
    (new button% 
      [parent dialog]
      [label "Exit"]
      [callback (lambda (button event) (exit))])
    (new button% 
      [parent dialog]
      [label "New"]
      [callback (lambda (button event) 
        (send canvas show #f)
        (send lose-dialog show #f)
        (send frame show #f)
        (start))])
    dialog))


; * * * * * * * * * * * * * * *
; * Area size choosing window *
; * * * * * * * * * * * * * * *

(define area-size-dialog 
  (new dialog% 
    [label "Choose area size"] 
    [width 250] [height 100]))

(define panel 
  (new horizontal-panel% 
    [parent area-size-dialog]
    [alignment '(center center)]))

(new message% 
  [parent area-size-dialog] 
  [label "Select size"])

(new button% 
  [parent panel]
  [label "4"]
  [callback 
    (lambda (button event)
      (new-game 4))])

(new button% 
  [parent panel]
  [label "5"]
  [callback 
    (lambda (button event)
      (new-game 5))])

(new button% 
  [parent panel]
  [label "6"]
  [callback 
    (lambda (button event)
      (new-game 6))])

; * * * * * * 
; * launch  *
; * * * * * *

(define (start)
  (send area-size-dialog show #t))

(start)





