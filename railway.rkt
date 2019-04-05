#lang racket


(require "gui.rkt")
(require "infrabel.rkt")

(provide set-gui-train-location!)

; This file contains the adt's for building the required components
; At the end of the file the track configuration is loaded.

; Defines the length of the vectors.
(define MAXTRAINS 10)
(define MAXSWITCH 10)
(define MAXDBLOCK 10)

;;;;;;;;;;;;;;;;
; switches-adt ;
;;;;;;;;;;;;;;;;

(define (make-switches-adt)

  ; Constant definitions
  (define number-of-switches 0)
  (define sw-vector(make-vector MAXSWITCH 0))

  ; Function to make and add switches to the switch vector.
  ; This is a vector containing vectors, one per switch.
  (define (make-switch id in out1 out2 state)
    (set! number-of-switches (+ number-of-switches 1))
    (define switch(make-vector 4 0))
    (vector-set! switch 0 state)
    (vector-set! switch 1 in)
    (vector-set! switch 2 out1)
    (vector-set! switch 3 out2)
    (vector-set! sw-vector id switch))

  (define (set-switch-state! id state)
    (vector-set! (vector-ref sw-vector id) 0 state))

  ; This function is called to build the gui by looping over the switch-vector.
  (define (build-gui)
    (define (loop id) 
      (if (<= id number-of-switches)
          (begin
            (make-switch-gui id (vector-ref (vector-ref sw-vector id) 0))
            (loop (+ 1 id)))
          (display 'No-switches?)))
    (loop 1))

  ; Dispatch funtion 
  (define (dispatch-switch msg)
    (cond ((eq? msg 'make-switch) make-switch)
          ((eq? msg 'set-switch-state!) set-switch-state!)
          ((eq? msg 'build-gui) build-gui)))
                   
  dispatch-switch)

;;;;;;;;;;;;;;;;;;;;;;;;
; Detection-Blocks-adt ;
;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-detectionblock-adt)

  ; Constant definitions
  (define number-of-dblocks 0)
  (define db-vector(make-vector MAXDBLOCK 0))

  ; Function to make and add detection blocks to the detection blocks vector.
  ; This is a vector containing vectors, one per detection blocks.
  (define (make-detection-block id in out state)
    (set! number-of-dblocks (+ number-of-dblocks 1))
    (define dblock(make-vector 3 0))
    (vector-set! dblock 0 state)
    (vector-set! dblock 1 in)
    (vector-set! dblock 2 out)
    (vector-set! db-vector id dblock))

  ; Returns the state of a detection block, can be free(1) or occupied(0)
  (define state 3)
  (define (get-db-state? id)
    (vector-ref (vector-ref db-vector id)state))

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
          ((eq? msg 'get-db-state?) get-db-state?)
          ((eq? msg 'build-gui) build-gui)))
           
  dispatch-detection-block)

;;;;;;;;;;;;;;
; tracks-adt ;
;;;;;;;;;;;;;;

(define (make-tracks-adt)
  
  ; Here we create the tracks that are not detection blocks.
  (define track-vector(make-vector 10 0))

  (define (make-track id in out)
    (define track(make-vector 2 0))
    (vector-set! track 0 in)
    (vector-set! track 1 out)
    (vector-set! track-vector id track))

  (define (dispatch-tracks msg)
    (cond ((eq? msg 'make-track) make-track)))
  
  dispatch-tracks)

;;;;;;;;;;;;;;
; train-adt  ;
;;;;;;;;;;;;;;

(define (make-train-adt)

  ; Define constants
  (define number-of-trains 0)
  (define train-vector(make-vector MAXTRAINS 0))

  ; Function to make and add trains to the trains vector.
  ; This is a vector containing vectors, one per train.
  (define (make-train id position previous-pos speed)
    (set! number-of-trains (+ number-of-trains 1))
    (define train (make-vector 4 0))
    (vector-set! train 0 speed)
    (vector-set! train 1 position)
    (vector-set! train 2 previous-pos)
    (vector-set! train-vector id train)
    (add-train id previous-pos position))

  (define (set-train-speed id speed)
    (vector-set! (vector-ref train-vector id ) 0 speed))

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
          ((eq? msg 'set-train-speed) set-train-speed)
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
(define tracks (make-tracks-adt))
(define trains (make-train-adt))

; Creation of all parts (loop-and-switches)
((switches 'make-switch) 1 'D-4 'T-1 'T-3 0)
((switches 'make-switch) 2 'T 'D-7 'D-6 0)
((switches 'make-switch) 3 'T 'D-7 'U-1 0)

((detection-blocks ' make-detection-block) 1 'T-2 'D-2 'safe)
((detection-blocks ' make-detection-block) 2 'D-1 'D-3 'safe)
((detection-blocks ' make-detection-block) 3 'D-2 'D-4 'safe)
((detection-blocks ' make-detection-block) 4 'D-3 'S-1 'safe)
((detection-blocks ' make-detection-block) 5 'T-1 'D-6 'safe)
((detection-blocks ' make-detection-block) 6 'D-5 'S-2.2 'safe)
((detection-blocks ' make-detection-block) 7 'S-3.1 'S-2.1 'safe)
((detection-blocks ' make-detection-block) 8 'S-3.2 'D-9 'safe)
((detection-blocks ' make-detection-block) 9 'D-9 'END 'safe)

((tracks 'make-track) 1 'S-1.1 'D-5)
((tracks 'make-track) 2 'S-2 'D-1)
((tracks 'make-track) 3 'S-1.2 'S-3)

((trains 'make-train) 1 'D1 'T2 80) 
;((trains 'make-train) 2 'D3 'D2 80)

; Initiation of the gui
((detection-blocks 'build-gui))
((switches 'build-gui))
((trains 'build-gui))
