#lang racket

(require racket/gui)
(require "communication.rkt")

(provide make-switch-gui make-dblock-gui make-train-gui set-dblock-free! set-dblock-occp! show-train-location)

; Make a frame, panel and columns to hold the gui in place
(define frame (new frame% [label "NMBS"][width 800]))
(define desti (new horizontal-panel% [parent frame]))
(define panel (new horizontal-panel% [parent frame]))

(define col1 (new vertical-panel% [parent panel]))
(define col2 (new vertical-panel% [parent panel]))
(define col3 (new vertical-panel% [parent panel]))

; startvalues for variables used in the dropdown lists when using send.
(define train "T-1")
(define dest "1-1")

(define list-of-trains (list "T-1" "T-2"))
(define list-of-dblocks  (list "1-1" "1-2" "1-3" "1-4" "1-5" "1-6" "1-7" "1-8" "2-1" "2-2" "2-3" "2-4" "2-5" "2-6" "2-7" "2-8"))

; Makes the dropdown lists to select train and destination and a button send to call set-route in infrabel.
(new choice% [parent desti][label "Train: "][choices list-of-trains]
     [callback (lambda (choice event)(set! train (send choice get-string-selection)))])
(new choice% [parent desti][label "Location: "][choices list-of-dblocks]
     [callback (lambda (choice event)(set! dest (send choice get-string-selection)))])
(new button% [parent desti][label "Send"]
     [callback (lambda (button event)(set-route dest train))])
                                               
; The size of vectors 
(define gui-dblocks-size 17)
(define gui-trains-size 5)

; Function to build a switch, shown in col1 on the gui.
(define (make-switch-gui id state)
  (new radio-box% [parent (new horizontal-panel% [parent col1])]
       [label (string-append "S-"(number->string id))]
       [style '(horizontal)]
       [choices (list "swPos 1" "swPos 2")]
       [selection state]
       [callback (lambda (radio event)(let ((id (string->symbol(send radio get-label)))
                                            (value (send radio get-selection)))
                                        (set-sw-position! id (+ 1 value))))]))

; Vector to hold the detecion blocks. 
(define gui-dblocks(make-vector gui-dblocks-size 0))

; Function to build detection blocks, shown in col2 on the gui.
(define (make-dblock-gui id name)
  (let ((dblock 
         (new gauge% [parent (new horizontal-panel% [parent col2])]
              [label name]
              [range 1] )      )
        (free 1))
    (send dblock set-value free)
    (vector-set! gui-dblocks id dblock)))

; Function to alter free and occupied visualization of a detection block.
(define (set-dblock-free! id)
  (send (vector-ref gui-dblocks id) set-value 1))

(define (set-dblock-occp! id)
  (send (vector-ref gui-dblocks id) set-value 0))  

; Vector to hold the trains.
(define gui-train-locations(make-vector gui-trains-size 0))

; Functie to build a train, shown in col3 on the gui.
(define (make-train-gui id)
  (let ((location (new message% [parent col3]
                       [label "Off-tracks, no location!"])))
    (vector-set! gui-train-locations id location))
  (new slider% [parent (new horizontal-panel% [parent col3])]
       [label (string-append "T-"(number->string id))]
       [min-value -200]
       [max-value 200]
       [init-value 0]
       [callback (lambda (slider event)(let ((id (string->symbol (send slider get-label)))
                                             (value (send slider get-value)))
                                         (set-speed! id value)
                                         ))]))
 
; Functie voor het in beeld brengen van de locatie van de trein.
(define (show-train-location id location)
  (send (vector-ref gui-dblocks (string->number (substring (symbol->string  location )1 2)))set-value 1)
  (send (vector-ref gui-train-locations id) set-label (symbol->string location))
  (send (vector-ref gui-dblocks (string->number (substring (symbol->string  location )1 2)))set-value 0))

; We vragen aan frame zich te toonen op het scherm.
(send frame show #t) 

