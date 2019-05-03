#lang racket

(require racket/tcp)
(require "infrabel.rkt")

;(define (serve port-no)
;  (define listener (tcp-listen port-no 5 #t))
;  (define (loop)
;    (accept-and-handle listener)
;    (loop))
;  (define t (thread loop))
;  (lambda ()
;    (kill-thread t)
;    (tcp-close listener)))
;
;(define (accept-and-handle listener)
;  (define cust (make-custodian))
;  (parameterize ([current-custodian cust])
;    (define-values (in out) (tcp-accept listener))
;    (thread (lambda ()
;              (handle in out)
;              (close-input-port in)
;              (close-output-port out))))
;  ; Watcher thread:
;  (thread (lambda ()
;            (sleep 10)
;            (custodian-shutdown-all cust))))

(define (perform-request pair)
  (define type car)
  (define fcall cdr)
  (cond [(eq? (type pair) 'speed!)(fcall pair)]
        [(eq? (type pair) 'switc!)(fcall pair)]
        [(eq? (type pair) 'dblock)(fcall pair)]
        [(eq? (type pair) 'route!)(fcall pair)]
        [(eq? (type pair) 'train!)(fcall pair)]
        [(eq? (type pair) 'travel)(fcall pair)]))
         
;(serve 1444)