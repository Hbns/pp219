#lang racket

(require "gui_simulator/interface.rkt") ; Simulator interface
;(require "hw_interface/interface.rkt") ; Hardware interface
(require "graphtrack.rkt")
;(require (prefix-in q: "a-d/queue/linked.rkt"))

(provide set-speed! set-sw-position! get-train-dblock stopat set-route add-train travel-section travel-route)

; Total number of trains.
(define nr-of-trains 2)
(define list-of-trains (list 'T-1 'T-2))

; Setup and sart the simulator 
;(setup-loop-and-switches)
(setup-hardware)
(start) 

; Add train .
(define (add-train train previous-pos position)
  (add-loco (string->symbol (string-append "T-"(number->string train))) previous-pos position))

; The following tree functions are provided to ask for or set changes in the simulator.
(define (set-speed! train speed)
  (set-loco-speed! train speed))

(define (set-sw-position! switch position)
  (set-switch-position! switch position))

(define (get-train-dblock train)
  (get-loco-detection-block train))

; A thread to detect train location and set the train speed to zero when train is on dblock
(define (stopat dblock train position)
  (thread (lambda ()
            (let loop ()
              (if (eq? (get-train-dblock train) dblock)
                  (begin (set-speed! train 0)
                         (vector-set! dblock-status position #t)) 
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

(define NR-OF-DBLOCKS 16)

;; vector for dblock reservations, if vector-ref #t dblock is free.
(define dblock-status (make-vector (+ NR-OF-DBLOCKS 1) #t))

;; extracts index from dblock name.
(define (get-dblock-nr position)
  (if (string<? position "2-1")
      (string->number (substring position 2 3))
      (+ (string->number (substring position 2 3)) 8)))

(define (update-dblocks-status) ; this only sets current location, two trains could drive to same destination!
  (vector-fill! dblock-status #t)
  (let loop ((trains list-of-trains))
    (if (empty? trains)
        'up-to-date
        (begin (vector-set! dblock-status (string->number (get-dblock-nr (symbol->string (get-train-dblock (car trains)))))#f)
               (loop (cdr trains)))))) 

(define NR-OF-SWITCHES 28)
(define switch-status (make-vector (+ NR-OF-SWITCHES 1)#t))
                                   
; A conditional to evoke functions based on the location, ex: set-sw-position! if location is a switch.
(define (get-switch-nr position)
  (if (string<? position "S100")
      (substring position 2 3)
      (substring position 1 3)))

;; this position inspector sets (physical) switches and reserves dblocks and switches in their status vectors.
(define (position-inspector! position)
  (cond ((equal? (substring position 0 1)"S")
         (set-sw-position! (string->symbol(string-append "S-" (get-switch-nr position))) (string->number (substring position 3 4)))
         (vector-set! switch-status (string->number(get-switch-nr position)) #f))
        (else (vector-set! dblock-status (get-dblock-nr position) #f))))

;; check's the status of a switch or dblock, this funtion helps (section-free?)
(define (check-status position)
  (if (equal? (substring position 0 1)"S")
      (vector-ref switch-status (string->number(get-switch-nr position)))
      (vector-ref dblock-status (get-dblock-nr position))))

;; runs over a section (list) and only returns #t (free) if all positions of the section are free.
(define (section-free? section)
  (andmap (lambda (position) (check-status position)) section))

; function to handel a section of a train route, a section goes from dblock to next dblock.
(define (travel-section train section)
  (let ((start (first section))
        (destination (last section))
        (train-symbol (string->symbol(string-append "T-"(number->string train)))))
    (if (section-free? section)
        (begin
          (for-each (lambda (position)(position-inspector! position))section)
          (if (direction? start destination)
              (set-speed! train-symbol 200)
              (set-speed! train-symbol -200))
          (stopat (string->symbol destination) train-symbol (get-dblock-nr start)))
        'dest-not-free )))

(define (travel-route train)
  (define next-section car)
  
  (let loop ((route (vector-ref all-train-routes train)))
    (if (empty? route)
        'traveled
        (begin (travel-section train (next-section route))(loop (cdr route))))))

;(define (run)
  
  
  
; Some function to ittirate over trains and there sections. like in every ittireation handel next section for train x
;some value chnages when train finishe section, only then next section can start.
;a new route can only be introduced on destiation.
; list to vector vector length gives # of sections and each itteration handle section index +1..?
; when reserving also need to block switches!

; vector-ref all-train-routes index is train start first section for all trains.
; handle next only if first is finished this can be done by asking the train location must equal destination, then start next section

;(define (handle-all-train-routes)
; (define


  