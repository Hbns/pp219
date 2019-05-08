#lang racket

(require "gui_simulator/interface.rkt") ; Simulator interface
;(require "hw_interface/interface.rkt") ; Hardware interface
(require "graphtrack.rkt")

(provide set-speed! set-sw-position! add-train get-train-dblock set-route travel-route reset-route)

; Total number of trains.
(define NR-OF-TRAINS 2)

; Setup and sart the simulator 
(setup-hardware)
(start) 

;; The following functions are provided to ask for or set changes in the simulator.
(define (add-train train previous-pos position)
  (add-loco (string->symbol (string-append "T-"(number->string train))) previous-pos position))

(define (set-speed! train speed)
  (set-loco-speed! train speed))

(define (set-sw-position! switch position)
  (set-switch-position! switch position))

(define (get-train-dblock train)
  (get-loco-detection-block train))

;; stopat detect's train location and set the train speed to zero when train is on dblock
(define (stopat dblock train)
  (let loop ()
    (if (eq? (get-train-dblock train) dblock)
        (set-speed! train 0)
        (loop))))

;; vector to contain list's with route sections at index train.
(define all-train-routes (make-vector (+ NR-OF-TRAINS 1)'()))

(define (add-route train route)
  (vector-set! all-train-routes train route))

(define (get-route train)
  (vector-ref all-train-routes train))

;; cuts a route into subroutes, from dblock to dblock
(define (make-route-sections train route indexes)
  (define start caar)
  (define items cdar)
  (if (empty? indexes)
      (display (get-route train))
      (begin
        (add-route train (cons (slice route (start indexes)(items indexes))(get-route train)))
        (make-route-sections train route (cdr indexes))))) 

;; calculates the indexes for the routes to be extracted from route in make-route-sections. 
(define (make-indexes route)
  (displayln route)
  (define return '())
  (define start car)
  (define next cadr)
  (let loop ((indexes (indexes-where route (lambda (item)(not (equal? (substring  item 0 1)"S"))))))
    (if (> (length indexes) 1) 
        (begin
          (set! return(cons (cons (start indexes) (+ (- (next indexes) (start indexes))1)) return))
          (loop (cdr indexes)))
        return)))

;; the actual list slicer to make sublists.
(define (slice list offset n)
  (take (drop list offset) n))

;; generates a route for a train 
(define (set-route to train)
  (let ((route (route-please (symbol->string (get-train-dblock (string->symbol train))) to))
        (train-nr (string->number(substring train 2 3))))
    (add-route train-nr '())
    (make-route-sections (string->number(substring train 2 3)) route (make-indexes route))))

;; vector to keep the free/occupied status of a dblock. (free = #t)
(define NR-OF-DBLOCKS 16)
(define dblock-status (make-vector (+ NR-OF-DBLOCKS 1) #t))

(define (dblock-occp! dblock)
  (vector-set! dblock-status dblock #f))

(define (dblock-free! dblock)
  (vector-set! dblock-status dblock #t))

(define (dblock-free? dblock)
  (vector-ref dblock-status dblock))

;; to deduce the dblock number from a string in section.
(define (get-dblock-nr position)
  (if (string<? position "2-1")
      (string->number (substring position 2 3))
      (+ (string->number (substring position 2 3)) 8)))

;; vector to keep the free/occupied status of a switch. (free = #t)
(define NR-OF-SWITCHES 28)
(define switch-status (make-vector (+ NR-OF-SWITCHES 1)#t))

(define (switch-occp! switch)
  (vector-set! switch-status switch #f))

(define (switch-free! switch)
  (vector-set! switch-status switch #t))

(define (switch-free? switch)
  (vector-ref switch-status switch))

                                   
;; to deduce the switch number from a string in section.
(define (get-switch-nr position)
  (if (string<? position "S100")
      (substring position 2 3)
      (substring position 1 3)))

;; this position inspector sets (physical) switches and reserves dblocks and switches in their status vectors.
(define (position-inspector! position)
  (cond ((equal? (substring position 0 1)"S")
         (set-sw-position! (string->symbol(string-append "S-" (get-switch-nr position))) (string->number (substring position 3 4)))
         (switch-occp! (string->number(get-switch-nr position))))
        (else (dblock-occp! (get-dblock-nr position)))))

;; this funtion releases the reserved route. 
(define (free-reservations section)
  (if (empty? section)
      'none
      (begin
        (let [(position (first section))]
          (cond ((equal? (substring position 0 1)"S")(switch-free! (string->number(get-switch-nr position)))(free-reservations (cdr section)))
                (else (dblock-free! (get-dblock-nr position))(free-reservations (cdr section))))))))

;; check's the status of a switch or dblock, this funtion helps (section-free?)
(define (check-status position)
  (if (equal? (substring position 0 1)"S")
      (switch-free? (string->number(get-switch-nr position)))
      (dblock-free? (get-dblock-nr position))))

;; runs over a section (list) and only returns #t (free) if all positions of the section are free.
(define (section-free? section)
  (andmap (lambda (position) (check-status position)) section))

;; function to handel a section of a train route, a section goes from dblock to next dblock.
(define (travel-section train section)
  (let ((start (first section))
        (destination (last section))
        (train-symbol (string->symbol(string-append "T-"(number->string train)))))
    (let wait-free ()
      (if (section-free? (cdr section))
          (begin
            (for-each (lambda (position)(position-inspector! position))section)
            (if (direction? start destination)
                (set-speed! train-symbol 200)
                (set-speed! train-symbol -200))
            (stopat (string->symbol destination) train-symbol)
            (free-reservations section)
            (dblock-occp! (get-dblock-nr destination)))
          (wait-free) ))))

;; travel-route call's travel-section for every section of the calculated route for train.
(define (travel-route train)
  (thread (lambda ()
            (define next-section car)
            (let loop ((route (get-route train)))
              (if (empty? route)
                  (add-route train '()) 
                  (begin (travel-section train (next-section route))(loop (cdr route))))))))

;; reset the route of a train.
(define (reset-route train)
  (add-route train '()))
