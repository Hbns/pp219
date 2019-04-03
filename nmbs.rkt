#lang racket

(require "railway.rkt")
(require "infrabel.rkt")


; Definition of start values
(define last-location 'D1)


; Function to ask for train location and update the gui.
(define (update-location train)
  (let ((dblock (get-train-dblock train))
        (index (string->number (substring (symbol->string train)2 3)))
        (last-index (string->number (substring (symbol->string last-location)1 2))))
    (if(and (not (eq? dblock #f))(not (eq? dblock last-location)))
       (begin
         (set-gui-train-location! index dblock last-index)
         (set! last-location dblock))
       'Unkown-Location-or-No-Update-Required)))

; A thread to constantly update the detection blocks for train location.
;(define update (thread (lambda ()
;                         (let loop ()
;                           (update-location 'T-1)
;                           (update-location 'T-2)
;                           (loop)))))
;
;