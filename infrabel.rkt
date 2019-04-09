#lang racket

(require "gui_simulator/interface.rkt") ; Simulator interface
;(require "hw_interface/interface.rkt") ; Hardware interface
(require "graphtrack.rkt")
(require (prefix-in q: "a-d/queue/linked.rkt"))

(provide set-speed! set-sw-position! get-train-dblock stopat set-route add-train trains-positions)

; Total number of trains define the length of qvector
(define nr-of-trains 2)

; Setup and sart the simulator 
;(setup-loop-and-switches)
(setup-hardware)
(start)
; Train location and previous location.
(define trains-positions (make-vector (+ nr-of-trains 1)0))

(define (fill-trains-positions)
  (define (fill count)
    (if (<= count nr-of-trains)
        (begin (vector-set! trains-positions count (make-vector 2 0)) (fill (+ 1 count)))
        (display "filled")))
  (fill 0))

(fill-trains-positions)
        

; Add train .
(define (add-train train previous-pos position)
  (vector-set! (vector-ref trains-positions train) 0 previous-pos)
  (vector-set! (vector-ref trains-positions train) 1 position)
  (add-loco (string->symbol (string-append "T-"(number->string train))) previous-pos position))

;(define (update-trains-locations)
;  (define (do-all-trains train)
;    (if (<= train nr-of-trains)
;        (begin
;          (let ((previous-location (vector-ref (vector-ref trains-locations train) 0 ))
;                (actual-location (get-train-dblock (string->symbol (string-append "T-"(number->string train))))))
;            (if (eq? previous-location actual-location)
;                (begin
;                  (display "no-updt") (do-all-trains (+ 1 train)))
;                (begin
;                  (vector-set! (vector-ref trains-locations train) 0 previous-location)
;                  (vector-set! (vector-ref trains-locations train) 1 actual-location)
;                  (display train)
;                  (do-all-trains (+ 1 train))
;                  ))))
;        (display "done")))
;  (do-all-trains 1))

              
    
; vector to keep the queues of the trains.
(define qvector (make-vector (+ nr-of-trains 1) (q:new))) 

; The following tree functions are provided to ask for or set changes in the simulator.
(define (set-speed! train speed)
  (set-loco-speed! train speed))

(define (set-sw-position! switch position)
  (set-switch-position! switch position))

(define (get-train-dblock train)
  (get-loco-detection-block train))

; A thread to detect train location and set the train speed to zero when train is on dblock
(define (stopat dblock train)
  (thread (lambda ()
            (let loop ()
              (if (eq? (get-train-dblock train) dblock)
                  (set-speed! train 0)
                  (loop))))))
;
;(define (collision-prevention train)
;  (let* ((train-number (string->number (substring (symbol->string train) 2 3)))
;        (next-location (q:serve! (vector-ref qvector train-number)))
;        (list-of-trains (remove train-number (sequence->list (in-range 1 (+ nr-of-trains 1))))))
;    (define (loop)
;     (display x) )))
; eq? over the get train location
  
              
              

; This function is called from gui to get the train going to destination.
; No real check for reservation, the idea is a queue with positions for each train.
; where serve! and peek are used to compare the routes of diferent trains and prevent collision.
(define (set-route to train)
  (route-please (get-train-dblock (string->symbol train)) to))
        

(define (set-route2 to train)
  (let ((route (route-please (symbol->string (get-train-dblock (string->symbol train))) (string->symbol to)))
        (from (symbol->string(get-train-dblock train)))
        (q (vector-ref qvector (string->number (substring (symbol->string train) 2 3)))))
    (display route)
    (route->queue route q)
    (if (check-direction-is-left from (cadr route))
        (set-speed! train -150)
        (set-speed! train 150))
    (stopat (string->symbol to) train)
    route ))

; A vector to keep track of reservations.
(define dblock-reservations  (make-vector 10 0))

; This function makes reservations in the dblock-reservations vector.
(define (reserve-release-locations q train)
  (if (not (q:empty? q))
      (begin
        (let ((location (q:serve! q)))
          (location-inspector location)
          (remove-reservation location train))
        (reserve-release-locations q train))
      (display " -q=empty- ")))
      
  
; A conditional to evoke functions based on the location, ex: set-sw-position! if location is a switch.
(define (location-inspector location)
  (cond ((equal? (substring location 0 1)"S")
         (set-sw-position! (string->symbol (substring location 0 2)) (string->number (substring location 2 3))))
        ((equal? (substring location 0 1)"D")
         (vector-set! dblock-reservations (string->number (substring location 1 2)) 1))
        ((equal? (substring location 0 1)"T")
         (display "T-ntd"))))
       
; This funtion removes a dblock reservation after the train left the dblock
(define (remove-reservation location train)
  (thread (lambda ()
            (let loop()
              (if (not (equal? (get-train-dblock train) location))
                  (vector-set! dblock-reservations (string->number (substring location 1 2)) 0)
                  (loop))))))

; Turns the received list into a queue
(define (route->queue route q)
  (define location car)
  (if (not (empty? route))
      (begin
        (q:enqueue! q (location route))
        (route->queue (cdr route)q ))
      q))


