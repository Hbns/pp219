#lang racket
(require rackunit "infrabel.rkt")

;(check-equal? (get-dblock-nr "1-8") 8 "dblock idx")
(check-equal? (section-free? (list "1-1" "1-2")))