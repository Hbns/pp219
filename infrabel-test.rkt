#lang racket
(require rackunit "infrabel.rkt")

(check-equal? (get-dblock-nr "1-8") 8 "dblock idx")