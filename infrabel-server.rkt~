#lang racket

(require "infrabel.rkt")

;; this thread starts the tcp server and thread to read the in port.
;; the in port receives a scheme list and (server-translate) breaks
;; the list down into a function call for infrabel.rkt. return-msg
;; contains the list with values to return.
(thread (lambda ()
          (define listener (tcp-listen 9883 4 #t))
          (define-values (in out) (tcp-accept listener))
          (define return-msg '())
          (define (send m)
            (write m out)
            (flush-output out))
          (define (server-translate list)
            (displayln list);prints log 
            (let [(proc (first list))]
              (cond [(eq? proc 'speed!)(set-speed! (list-ref list 1)(list-ref list 2))]
                    [(eq? proc 'switch!)(set-sw-position! (list-ref list 1)(list-ref list 2))]
                    [(eq? proc 'train!)(add-train (list-ref list 1)(list-ref list 2)(list-ref list 3))]
                    [(eq? proc 'dblock)(send (cons (get-train-dblock (list-ref list 1))return-msg))]
                    [(eq? proc 'route!)(set-route (list-ref list 1)(list-ref list 2))]
                    [(eq? proc 'travel)(travel-route (list-ref list 1))]
                    [(eq? proc 'reset)(reset-route (list-ref list 1))]
                    [else (display "tcp-translate-error")])))
          (thread (lambda ()(let loop ()
                              (server-translate (read in))
                              (loop))))))

;(tcp-close listener)
;(close-input-port in)
;(close-output-port out)
        


