#lang racket
;(require "infrabel.rkt") 

(provide set-speed! set-sw-position! add-train get-train-dblock set-route travel-route reset-route NR-OF-TRAINS NR-OF-DBLOCKS close-ports)

(define NR-OF-TRAINS 2)
(define NR-OF-DBLOCKS 16)

;; funtion that send arg as a scheme list over tcp.
(define (send-msg . arg)
  (write arg out)
  (flush-output out))

;; funtion that send and receive arg as a scheme list over tcp.
(define (send-receive-msg . arg)
  (write arg out)
  (flush-output out)
  (first (read in)))

;; collection of functions sending a tcp message from a function call.
(define (set-speed! id value)
  (send-msg 'speed! id value))
   
(define (set-sw-position! id value)
  (send-msg 'switch! id value)) 
  
(define (add-train id previous-pos position)
  (send-msg 'train! id previous-pos position)) 

(define (get-train-dblock train)
  (send-receive-msg 'dblock train))

(define (set-route dest train)
  (send-msg 'route! dest train))

(define (travel-route train)
  (send-msg 'travel train))

(define (reset-route train)
  (send-msg 'reset train))
  
;; the values neccesary to make tcp communication possible.
(define-values (in out) (tcp-connect "localhost" 9883))

;; to close the tcp communication ports.
(define (close-ports)
  (close-input-port in)
  (close-output-port out))
