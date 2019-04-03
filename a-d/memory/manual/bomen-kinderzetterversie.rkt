#lang racket

(define (insert-free addr siz)
  (define (insert-address curr child!)
    (cond ((null? curr)
           (child! addr))
          ((<= addr curr)
           (insert-address (address-left curr)
                           (lambda (child) (address-left! child))))
          (else
           (insert-address (address-right curr)
                           (lambda (child) (address-right! child))))))
  (define (insert-size curr child!)
    (cond ((null? curr)
           (child! addr))
          ((< siz (size curr))
           (insert-size (size-left curr)
                        (lambda (child) (size-left! child))))
          (else
           (insert-size (size-right curr)
                        (lambda (child) (size-right! child))))))
  (size! addr siz)
  (address-left!  addr null)
  (address-right! addr null)
  (size-left!     addr null)
  (size-right!    addr null)
  (insert-address address-tree (lambda (child) 
                                 (set! address-tree child)))
  (insert-size size-tree (lambda (child) 
                           (set! size-tree child))))