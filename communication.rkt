#lang racket
(require "infrabel.rkt")

(provide set-route set-speed! set-sw-position! add-train get-train-dblock)

(define (set-routeT dest train)
  (set-route dest train))

(define (set-speed!T id value)
  (set-speed! id value))

(define (set-sw-position!T id value)
  (set-sw-position! id value))

(define (add-trainT id previous-pos position)
  (add-train id previous-pos position))