#lang racket

(require "railway.rkt")
(require "communication.rkt")
(require "gui.rkt")

(define (get-dblock-nr position)
  (if (string<? position "2-1")
      (string->number (substring position 2 3))
      (+ (string->number (substring position 2 3)) 8)))


; Definition of start values
(define last-location (make-vector 3 0)); move constants to communications?

; Continue here, move constants to communications fix this to run on new setup.
(define (update-location trains)
  (if (empty? trains)
      'done
      (begin
  (let* [(train (first trains))
         (dblock (get-dblock-nr (symbol->string (get-train-dblock (string->symbol train)))))
        (index (string->number (substring train 2 3)))
        (last-index (vector-ref last-location index))]
    (if(and (not (eq? dblock #f))(not (eq? dblock last-location)))
       (begin
         (set-gui-train-location! index dblock last-index)
         (set! last-location dblock))
       'Unkown-Location-or-No-Update-Required)))))

; A thread to constantly update the detection blocks for train location.
;(define update (thread (lambda ()
;                        (let loop ()
;                          (update-location 'T-1)
;                            (loop)))))
(define (update-location-start-screen trains)
  (if (empty? trains)
      'done
      (begin
        (let [(train (first trains))]
          (show-train-location (string->number (substring train 2 3)) (get-train-dblock (string->symbol train)))
          (set-dblock-occp! (get-dblock-nr (symbol->string (get-train-dblock (string->symbol train))))))
        (update-location-start-screen (cdr trains)))))

(update-location-start-screen list-of-trains)
