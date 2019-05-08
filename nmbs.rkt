#lang racket

(require "railway.rkt")
(require "nmbs-client.rkt")
(require "gui.rkt")

;; returns an integer from a dblock string.
(define (get-dblock-nr position)
  (if (string<? position "2-1")
      (string->number (substring position 2 3))
      (+ (string->number (substring position 2 3)) 8)))

;; vector to store the previous position of train
(define last-position (make-vector (+ (length list-of-trains) 1)0))

;; update's the gui for train position and dblock status.
(define (update-location trains)
  (if (empty? trains)
      'done
      (begin
        (let* [(train (first trains))
               (train-nr (string->number (substring train 2 3)))
               (dblock (get-train-dblock (string->symbol train)))
               (previous-dblock-nr (vector-ref last-position train-nr))]
          (set-dblock-free! previous-dblock-nr)
          (if(not (eq? dblock #f))
             (begin
               (let [(dblock-nr (get-dblock-nr (symbol->string dblock)))]
                 (vector-set! last-position train-nr dblock-nr)
                 (show-train-location train-nr dblock)
                 (set-dblock-occp! dblock-nr)
                 (update-location (cdr trains))))
             'Unkown-Location)))))

;; update's the gui for train position and dblock status at startup.
(define (update-location-start-screen trains)
  (if (empty? trains)
      'done
      (begin
        (let* [(train (first trains))
               (train-nr (string->number (substring train 2 3)))
               (dblock (get-train-dblock (string->symbol train)))
               (dblock-nr (get-dblock-nr (symbol->string dblock)))]
          (vector-set! last-position train-nr dblock-nr)
          (show-train-location train-nr dblock)
          (set-dblock-occp! dblock-nr))
        (update-location-start-screen (cdr trains)))))

(update-location-start-screen list-of-trains)

;; thread that runs update-location.
(define update (thread (lambda ()
                         (let loop ()
                           (update-location list-of-trains)
                           (loop)))))
