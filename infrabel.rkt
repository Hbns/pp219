#lang racket

(require "gui_simulator/interface.rkt") ; Simulator interface
;(require "hw_interface/interface.rkt") ; Hardware interface
(require "graphtrack.rkt")
(require (prefix-in q: "a-d/queue/linked.rkt"))

(provide set-speed! set-sw-position! get-train-dblock stopat set-route add-train trains-positions travel-section)

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
  (vector-set! (vector-ref trains-positions train) 0 (symbol->string previous-pos))
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

; vector to contain list with route sections at index train.
(define all-train-routes (make-vector (+ nr-of-trains 1)'()))

; cuts a route into subroutes, from dblock to dblock
(define (make-route-sections train route indexes)
  (define start caar)
  (define items cdar)
  (if (empty? indexes)
      (display (vector-ref all-train-routes train))
      (begin
        (vector-set! all-train-routes train (cons (slice route (start indexes)(items indexes))(vector-ref all-train-routes train)))
        (make-route-sections train route (cdr indexes))))) 

; calculates the indexes for the routes to be extracted from route in make-route-sections. 
(define (make-indexes route)
  (define return '())
  (define start car)
  (define next cadr)
  (let loop ((indexes  (indexes-where route (lambda (item)(not (equal? (substring  item 0 1)"S"))))))
    (if (> (length indexes) 1) 
        (begin
          (set! return(cons (cons (start indexes) (+ (- (next indexes) (start indexes))1)) return))
          (loop (cdr indexes)))
        return)))

; the actual list slicer to make sublists.
(define (slice list offset n)
  (take (drop list offset) n))

; this funtion is called from gui when send button is pushed
; it generates and prepares the route for a train.
(define (set-route to train)
  (let ((route (route-please (symbol->string (get-train-dblock (string->symbol train))) to))
        (train-nr (string->number(substring train 2 3))))
    (vector-set! all-train-routes train-nr '())
    (display route) ;o remove
    (make-route-sections (string->number(substring train 2 3)) route (make-indexes route))))

; A conditional to evoke functions based on the location, ex: set-sw-position! if location is a switch.
(define (position-inspector position)
  (cond ((equal? (substring position 0 1)"S")
         (set-sw-position! (string->symbol (substring position 0 2)) (string->number (substring position 2 3))))
        ((equal? (substring position 0 1)"D")
         (vector-set! (string->number (substring position 1 2)) 1))))

; Continue here
(define (travel-section train section)
  (let ((start (first section))
        (destination (last section))
        (train (string->symbol(string-append "T-"(number->string train)))))
    (for-each (lambda (position)(position-inspector position))section)
    (if (direction?)
        (set-speed! train 200)
        (set-speed! train -200))))
   