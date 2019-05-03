#lang racket
(require racket/tcp)
(require "infrabel.rkt")

(provide set-route set-speed! set-sw-position! add-train get-train-dblock travel-route)

;(define MESSAGE "none")
;
;(define (client host port)
;  (define-values (c-in c-out) (tcp-connect host port))
; 
;  (write MESSAGE c-out)
;  (close-output-port c-out))
;
;(thread (lambda () (client "localhost" 1444)))


;(define (set-routeT dest train)
;  (cons 'route! '(set-route dest train)))

;(define (set-speed!T id value)
;  (cons 'speed! '(set-speed! id value)))

(define (set-sw-position!T id value)
  (set-sw-position! id value))

(define (add-trainT id previous-pos position)
  (add-train id previous-pos position))