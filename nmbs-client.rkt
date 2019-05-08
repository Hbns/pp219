#lang racket
;(require "infrabel.rkt") 

(provide set-speed! set-sw-position! add-train get-train-dblock set-route travel-route)

;; the following functions forward the function call as a scheme list to the tcp server.
;; get-train-dblock receives and returns the value asked for.

(define (set-speed! id value)
  (write (list 'speed! id value) out)
  (flush-output out))
  
(define (set-sw-position! id value)
  (write (list 'switch! id value) out)
  (flush-output out))
  
(define (add-train id previous-pos position)
  (write (list 'train! id previous-pos position) out)
  (flush-output out))

(define (get-train-dblock train)
  (write (list 'dblock train) out)
  (flush-output out)
  (first (read in)))

(define (set-route dest train)
  (write (list 'route! dest train) out)
  (flush-output out))

(define (travel-route train)
  (write (list 'travel train) out)
  (flush-output out))
  
;; the values neccesary to make tcp communication possible.
(define-values (in out) (tcp-connect "localhost" 9883))

