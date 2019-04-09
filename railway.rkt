#lang racket

(require "gui.rkt")
(require "infrabel.rkt")

(provide set-gui-train-location!)

; This file contains the adt's for building the required components
; At the end of the file the track configuration is loaded.

; Defines the length of the vectors.
(define NR-OF-DBLOCKS 16)
(define NR-OF-TRAINS 2)

;;;;;;;;;;;;;;;;
; switches-adt ;
;;;;;;;;;;;;;;;;

(define (make-switches-adt)

  ; List to contain switch-id, switch-position pairs.
  (define sw-list '())

  ; Function to add switches to sw-list.
  (define (make-switch id state)
    (set! sw-list (cons (cons id state)sw-list)))

  ; This function is called to build the gui.
  (define (build-gui)
    (define switch-id car)
    (define switch-position cdr)
    (for-each (lambda (switch) (make-switch-gui (switch-id switch) (switch-position switch))) sw-list))                       
          
  ; Dispatch funtion 
  (define (dispatch-switch msg)
    (cond ((eq? msg 'make-switch) make-switch)
          ((eq? msg 'build-gui) build-gui)))
                   
  dispatch-switch)

;;;;;;;;;;;;;;;;;;;;;;;;
; Detection-Blocks-adt ;
;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-detectionblock-adt)

  ; Constant definitions
  (define number-of-dblocks 0)
  (define db-vector(make-vector (+ NR-OF-DBLOCKS 1) 0))

  ; Function to add detection blocks to the detection blocks vector.
  (define (make-detection-block id state)
    (set! number-of-dblocks (+ number-of-dblocks 1))
    (vector-set! db-vector id state))

  ; This function is called to build the gui by looping over the detection bock-vector.
  (define (build-gui)
    (define (loop id) 
      (if (<= id number-of-dblocks)
          (begin
            (make-dblock-gui id)
            (loop (+ 1 id)))
          (display 'No-dblocks?)))
    (loop 1))

  ; Dispatch funtion
  (define (dispatch-detection-block msg)
    (cond ((eq? msg 'make-detection-block) make-detection-block)
          ((eq? msg 'build-gui) build-gui)))
           
  dispatch-detection-block)

;;;;;;;;;;;;;;
; train-adt  ;
;;;;;;;;;;;;;;

(define (make-train-adt)

  ; Define constants
  (define number-of-trains 0)
  (define train-vector(make-vector NR-OF-TRAINS 0))

  ; Function to make and add trains.
  (define (make-train id previous-pos position)
    (set! number-of-trains (+ number-of-trains 1))    
    (add-train id previous-pos position))

  ; This function is called to build the gui by looping over the detection bock-vector.
  (define (build-gui)
    (define (loop id) 
      (if (<= id number-of-trains)
          (begin
            (make-train-gui id)
            (loop (+ 1 id)))
          (display 'No-Trains?)))
    (loop 1))

  ; Dispath functie
  (define (dispatch-trains msg)
    (cond ((eq? msg 'make-train) make-train)
          ((eq? msg 'build-gui) build-gui)))
          
  dispatch-trains)

; Function to alter the displayed loaction of the train, provided to NMBS.
 
(define (set-gui-train-location! id location last-location)
  (set-dblock-free! last-location)
  (show-train-location id location))

;;;;;;;;;;;;;;;;;;
; Track creation ;
;;;;;;;;;;;;;;;;;;

; Each part of the track is added to it's vector.
; After the track exists in the vectors the buil-gui
; function for each part is called to build the gui

; Definition of the ADT's
(define switches (make-switches-adt))
(define detection-blocks (make-detectionblock-adt))

(define trains (make-train-adt))

; Creation of all parts (loop-and-switches)

; switch id's from big to small
((switches 'make-switch) 28 0)
((switches 'make-switch) 7 0)
((switches 'make-switch) 3 0)
((switches 'make-switch) 2 0)
((switches 'make-switch) 1 0)

((detection-blocks ' make-detection-block) 1 'safe)
((detection-blocks ' make-detection-block) 2 'safe)
((detection-blocks ' make-detection-block) 3 'safe)
((detection-blocks ' make-detection-block) 4 'safe)
((detection-blocks ' make-detection-block) 5 'safe)
((detection-blocks ' make-detection-block) 6 'safe)
((detection-blocks ' make-detection-block) 7 'safe)
((detection-blocks ' make-detection-block) 8 'safe)
((detection-blocks ' make-detection-block) 9 'safe)

((trains 'make-train) 1 'T2 'D1) 
((trains 'make-train) 2 'D3 'D4)

; Initiation of the gui
((detection-blocks 'build-gui))
((switches 'build-gui))
((trains 'build-gui))
