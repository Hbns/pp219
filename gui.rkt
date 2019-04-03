#lang racket

(require racket/gui)
(require "infrabel.rkt")

(provide make-switch-gui make-dblock-gui make-train-gui set-dblock-free! set-dblock-occp! show-train-location)

; Make a frame, panel and columns to hold the gui in place
(define frame (new frame% [label "NMBS"][width 1000]))
(define desti (new horizontal-panel% [parent frame]))
(define panel (new horizontal-panel% [parent frame]))

(define col1 (new vertical-panel% [parent panel]))
(define col2 (new vertical-panel% [parent panel]))
(define col3 (new vertical-panel% [parent panel]))

; Variables used in the dropdown lists
(define train 1)
(define dest 1)

; Makes the dropdown lists to select train and destination and a button send to call set-route in infrabel.
(new choice% [parent desti][label "Train: "][choices (list "T1" "T2")]
     [callback (lambda (choice event)(set! train (+ 1 (send choice get-selection))))])
(new choice% [parent desti][label "Location: "][choices (list "D1" "D2" "D3" "D4" "D5" "D6" "D7" "D8" "D9")]
     [callback (lambda (choice event)(set! dest (+ 1 (send choice get-selection))))])
(new button% [parent desti][label "Send"]
     [callback (lambda (button event)(set-route (string-append "D"(number->string dest))
                                                (string->symbol(string-append "T-"(number->string train)))))])

; The size of vectors 
(define gui-dblocks-size 11)
(define gui-trains-size 5)

; Function to build a switch, shown in col1 on the gui.
(define (make-switch-gui id state)
  (new radio-box% [parent (new horizontal-panel% [parent col1])]
       [label (string-append "S"(number->string id))]
       [style '(horizontal)]
       [choices (list "swPos 1" "swPos 2")]
       [selection state]
       [callback (lambda (radio event)(let ((id (string->symbol(send radio get-label)))
                                            (value (send radio get-selection)))
                                        (set-sw-position! id (+ 1 value)))          
                   )]))

; Vector to hold the detecion blocks. 
(define gui-dblocks(make-vector gui-dblocks-size 0))

; Function to build detection blocks, shown in col2 on the gui.
(define (make-dblock-gui id)
  (let ((dblock 
         (new gauge% [parent (new horizontal-panel% [parent col2])]
              [label (string-append "D"(number->string id))]
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
                                         (set-speed! id (- value))
                                         ))]
       ))
 
; Functie voor het in beeld brengen van de locatie van de trein.
(define (show-train-location id location)
  (send (vector-ref gui-dblocks (string->number (substring (symbol->string  location )1 2)))set-value 1)
  (send (vector-ref gui-train-locations id) set-label (symbol->string location))
  (send (vector-ref gui-dblocks (string->number (substring (symbol->string  location )1 2)))set-value 0))

; We vragen aan frame zich te toonen op het scherm.
(send frame show #t) 

