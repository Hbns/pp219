#lang racket

(require "a-d/graph/labeled/adjacency-matrix.rkt")
(require (prefix-in ug: "a-d/graph/unweighted/adjacency-list.rkt"))

(require "a-d/graph-traversing/bft-labeled.rkt")
(require (prefix-in ugt: "a-d/graph-traversing/bft.rkt"))
(require compatibility/mlist)
(require racket/vector)

(provide route-please check-direction-is-left)
(define order 61)

; graphtrack is the tree that represents the railway.
(define graphtrack (ug:new #f order))

; Nodes 
(define 1-5 0)(define 1-4 1)(define S262 2)(define S272 3)(define 1-3 4)(define T2 5)(define S231 6)(define 2-4 7)
(define S201 8)(define S271 9)(define 1-2 10)(define S092 11)(define S241 12)(define S091 13)(define S111 14)
(define S121 15)(define S232 16)(define S122 17)(define 2-3 18)(define S261 19)(define T1 20)(define S282 21)
(define 1-1 22)(define S101 23)(define S112 24)(define 2-8 25)(define S162 26)(define T7 27)(define S161 28)
(define S102 29)(define S281 30)(define 1-7 31)(define 1-6 32)(define S051 33)(define S061 34)(define S202 35)
(define 1-8 36)(define S252 37)(define T5 38)(define S071 39)(define S052 40)(define S062 41)(define S021 42)
(define S072 43)(define S041 44)(define 2-6 45)(define 2-1 46)(define S011 47)(define T6 48)(define S022 49)
(define S032 50)(define S081 51)(define S042 52)(define 2-7 53)(define 2-2 54)(define S082 55)(define 2-5 56)
(define S012 57)(define S031 58)(define S242 59)(define S251 60)

; Vector and hash for nodenr->nodelabel and nodelabel->nodenr translation.
(define nodenames (make-vector order 0))
(define nodehash (hash "1-1" 22 "1-2" 10 "1-3" 4 "1-4" 1 "1-5" 0 "1-6" 32 "1-7" 31 "1-8" 36
                       "2-1" 46 "2-2" 54 "2-3" 18 "2-4" 7 "2-5" 56 "2-6" 45 "2-7" 53 "2-8" 25
                       "S011" 47 "S012" 57 "S021" 42 "S031" 58 "S032" 50 "S041" 44 "S042" 52
                       "S051" 33 "S052" 40 "S061" 34 "S062" 41 "S071" 39 "S072" 43 "S081" 51
                       "S082" 55 "S091" 13 "S092" 11 "S101" 23 "S102" 29 "S111" 14 "S112" 24
                       "S121" 15 "S122" 17 "S161" 28 "S162" 26 "S201" 8 "S202" 35 "S231" 6
                       "S232" 16 "S241" 12 "S242" 59 "S251" 60 "S252" 25 "S261" 19 "S262" 2
                       "S271" 9 "S272" 3 "S281" 30 "S282" 21))

; Dblocks
(vector-set! nodenames 22 '1-1)
(vector-set! nodenames 10 '1-2)
(vector-set! nodenames 4 '1-3)
(vector-set! nodenames 1 '1-4)
(vector-set! nodenames 0 '1-5)
(vector-set! nodenames 32 '1-6)
(vector-set! nodenames 31 '1-7)
(vector-set! nodenames 36 '1-8)
(vector-set! nodenames 46 '2-1)
(vector-set! nodenames 54 '2-2)
(vector-set! nodenames 18 '2-3)
(vector-set! nodenames 7 '2-4)
(vector-set! nodenames 56 '2-5)
(vector-set! nodenames 45 '2-6)
(vector-set! nodenames 53 '2-7)
(vector-set! nodenames 25 '2-8)
 
; Switches
(vector-set! nodenames 47 "S011")
(vector-set! nodenames 57 "S012")
(vector-set! nodenames 42 "S021")
(vector-set! nodenames 49 "S022")
(vector-set! nodenames 58 "S031")
(vector-set! nodenames 50 "S032")
(vector-set! nodenames 44 "S041")
(vector-set! nodenames 52 "S042")
(vector-set! nodenames 33 "S051")
(vector-set! nodenames 40 "S052")
(vector-set! nodenames 34 "S061")
(vector-set! nodenames 41 "S062")
(vector-set! nodenames 39 "S071")
(vector-set! nodenames 43 "S072")
(vector-set! nodenames 51 "S081")
(vector-set! nodenames 55 "S082")
(vector-set! nodenames 13 "S091")
(vector-set! nodenames 11 "S092")
(vector-set! nodenames 23 "S101")
(vector-set! nodenames 29 "S102")
(vector-set! nodenames 14 "S111")
(vector-set! nodenames 24 "S112")
(vector-set! nodenames 15 "S121")
(vector-set! nodenames 17 "S122")
(vector-set! nodenames 28 "S161")
(vector-set! nodenames 26 "S162")
(vector-set! nodenames 8 "S201")
(vector-set! nodenames 35 "S202")
(vector-set! nodenames 6 "S231")
(vector-set! nodenames 16 "S232")
(vector-set! nodenames 12 "S241")
(vector-set! nodenames 59 "S242")
(vector-set! nodenames 60 "S251")
(vector-set! nodenames 25 "S252")
(vector-set! nodenames 19 "S261")
(vector-set! nodenames 2 "S262")
(vector-set! nodenames 9 "S271")
(vector-set! nodenames 3 "S272")
(vector-set! nodenames 30 "S281")
(vector-set! nodenames 21 "S282")

; Building the actual tree
(ug:add-edge! graphtrack 1-5 1-4)(ug:add-edge! graphtrack 1-4 S262)(ug:add-edge! graphtrack 1-4 S261)(ug:add-edge! graphtrack S262 S272)
(ug:add-edge! graphtrack S262 S271)(ug:add-edge! graphtrack S272 1-3)(ug:add-edge! graphtrack 1-3 T2)(ug:add-edge! graphtrack T2 S242)
(ug:add-edge! graphtrack S242 S231)(ug:add-edge! graphtrack S231 2-4)(ug:add-edge! graphtrack S231 S241)(ug:add-edge! graphtrack 2-4 S201)
(ug:add-edge! graphtrack 2-4 S202)(ug:add-edge! graphtrack 2-4 S232)(ug:add-edge! graphtrack S201 1-5)(ug:add-edge! graphtrack S271 1-2)
(ug:add-edge! graphtrack 1-2 S092)(ug:add-edge! graphtrack 1-2 S091)(ug:add-edge! graphtrack S092 S241)(ug:add-edge! graphtrack S091 S111)
(ug:add-edge! graphtrack S111 S121)(ug:add-edge! graphtrack S111 S122)(ug:add-edge! graphtrack S122 2-3)(ug:add-edge! graphtrack S122 S112)
(ug:add-edge! graphtrack 2-3 S062)(ug:add-edge! graphtrack S261 T1)(ug:add-edge! graphtrack T1 S282)(ug:add-edge! graphtrack S282 1-1)
(ug:add-edge! graphtrack 1-1 S281)(ug:add-edge! graphtrack 1-1 S101)(ug:add-edge! graphtrack S101 S112)(ug:add-edge! graphtrack S112 S102)
(ug:add-edge! graphtrack 2-8 S162)(ug:add-edge! graphtrack S162 T7)(ug:add-edge! graphtrack T7 S161)(ug:add-edge! graphtrack S161 S102)
(ug:add-edge! graphtrack S281 1-7)(ug:add-edge! graphtrack 1-7 1-6)(ug:add-edge! graphtrack 1-6 S051)(ug:add-edge! graphtrack S051 S061)
(ug:add-edge! graphtrack S051 S062)(ug:add-edge! graphtrack S061 S202)(ug:add-edge! graphtrack 1-8 S252)(ug:add-edge! graphtrack 1-8 S251)
(ug:add-edge! graphtrack S252 T5)(ug:add-edge! graphtrack T5 S071)(ug:add-edge! graphtrack S071 S052)(ug:add-edge! graphtrack S052 S062)
(ug:add-edge! graphtrack S052 S072)(ug:add-edge! graphtrack S072 S021)(ug:add-edge! graphtrack S021 T6)(ug:add-edge! graphtrack S251 S012)
(ug:add-edge! graphtrack S012 T6)(ug:add-edge! graphtrack 2-1 S011)(ug:add-edge! graphtrack S011 T6)(ug:add-edge! graphtrack T6 S022)
(ug:add-edge! graphtrack S022 S031)(ug:add-edge! graphtrack S031 2-2)(ug:add-edge! graphtrack S022 S032)(ug:add-edge! graphtrack S032 S081)
(ug:add-edge! graphtrack S032 S082)(ug:add-edge! graphtrack S082 2-5)(ug:add-edge! graphtrack S081 S041)(ug:add-edge! graphtrack S081 S042)
(ug:add-edge! graphtrack S041 2-6)(ug:add-edge! graphtrack S042 2-7)


(define (find-node-number nodename)
  (vector-member nodename nodenames))


; Function to determainate what direction the train should start it's path.
(define (check-direction-is-left location next-location)
  (let ((location-to-check-nr  (find-node-number location))
        (next-location-nr (find-node-number next-location)))
    (if (< location-to-check-nr next-location-nr)
        #t
        #f
          )))

; This functions returns a list with the labels corresponding to the calculated route. 
(define (route-please from to)
  (route->labels (shortest-path graphtrack (hash-ref nodehash from) (hash-ref nodehash to))'()))
                               
; This function calculates the path to destination, copy from a-d with small adaptions.
(define (shortest-path g from to)
  (define paths (make-vector (ug:order g) '()))
  (vector-set! paths from (list from))
  (ugt:bft g 
           ugt:root-nop
           (lambda (node) ;node discovered
             (not (eq? node to)))
           (lambda (from to);edge discovered
             (vector-set! paths to (cons to (vector-ref paths from))))
           ugt:edge-nop
           (list->mlist (list from)))
  (vector-ref paths to))

 
; Converts nodenumbers to nodenames
(define (route->labels list res)
  (if (empty? list)
      res
      (route->labels (cdr list) (cons (vector-ref nodenames (car list))res))))
