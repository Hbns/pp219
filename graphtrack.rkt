#lang racket

(require (prefix-in ug: "a-d/graph/unweighted/adjacency-list.rkt"))
(require (prefix-in ugt: "a-d/graph-traversing/bft.rkt"))
(require compatibility/mlist)
(require racket/vector)

(provide route-please direction?)

(define ORDER 102)

;; railwaygraph is the tree that represents the railway.
(define railwaygraph (ug:new #f ORDER))

;; defines the nodes for railwaygraph. 
(define 1-5 0)(define 1-4 1)(define S262 2)(define S272 3)(define 1-3 4)(define T2 5)(define S231 6)(define 2-4 7)
(define S201 8)(define S271 9)(define 1-2 10)(define S092 11)(define S241 12)(define S091 13)(define S111 14)
(define S121 15)(define S232 16)(define S122 17)(define 2-3 18)(define S261 19)(define T1 20)(define S282 21)
(define 1-1 22)(define S101 23)(define S112 24)(define 2-8 25)(define S162 26)(define T7 27)(define S161 28)
(define S102 29)(define S281 30)(define 1-7 31)(define 1-6 32)(define S051 33)(define S061 34)(define S202 35)
(define 1-8 36)(define S252 37)(define T5 38)(define S071 39)(define S052 40)(define S062 41)(define S021 42)
(define S072 43)(define S041 44)(define 2-6 45)(define 2-1 46)(define S011 47)(define T6 48)(define S022 49)
(define S032 50)(define S081 51)(define S042 52)(define 2-7 53)(define 2-2 54)(define S082 55)(define 2-5 56)
(define S012 57)(define S031 58)(define S242 59)(define S251 60)

;; vector and hash for nodenr->nodelabel and nodelabel->nodenr translation.
(define nodelabels (make-vector ORDER 0))
(define nodehash (hash "1-1" 0 "1-2" 22 "1-3" 31 "1-4" 17 "1-5" 18 "1-6" 8 "1-7" 7 "1-8" 67
                       "2-1" 34 "2-2" 55 "2-3" 11 "2-4" 5 "2-5" 51 "2-6" 46 "2-7" 40))

;; finding the start direction uses this hash.
(define directionhash (hash "1-12-4" #t "1-12-3" #t "1-11-7" #f "1-11-4" #f "1-22-3" #t "1-22-4" #t "1-21-4" #f "1-32-4" #t "1-31-4" #f
                            "1-41-1" #t "1-41-2" #t "1-41-3" #t "1-41-5" #f "1-51-4" #t "1-52-4" #f "1-61-7" #t "1-62-3" #f "1-62-4" #f
                            "1-71-1" #t "1-71-6" #f "1-82-2" #f "1-82-3" #f "1-82-4" #f "1-82-5" #f "1-82-6" #f "1-82-7" #f "2-12-2" #f
                            "2-12-3" #f "2-12-4" #f "2-12-5" #f "2-12-6" #f "2-12-7" #f "2-31-6" #t "2-31-8" #t "2-32-1" #t "2-31-2" #t
                            "2-31-1" #f "2-41-5" #t "2-41-8" #t "2-42-1" #t "2-41-1" #f "2-41-2" #f "2-41-3" #f "2-22-1" #t "2-21-8" #t
                            "2-52-1" #t "2-51-8" #t "2-62-1" #t "2-61-8" #t "2-72-1" #t "2-71-8" #t))

;; filling the vector nodelabels to be used for node/number translations.
;; dblocks
(vector-set! nodelabels 0 "1-1")(vector-set! nodelabels 22 "1-2")(vector-set! nodelabels 31 "1-3")
(vector-set! nodelabels 17 "1-4")(vector-set! nodelabels 18 "1-5")(vector-set! nodelabels 8 "1-6")
(vector-set! nodelabels 7 "1-7")(vector-set! nodelabels 67 "1-8")(vector-set! nodelabels 34 "2-1")
(vector-set! nodelabels 55 "2-2")(vector-set! nodelabels 11 "2-3")(vector-set! nodelabels 5 "2-4")
(vector-set! nodelabels 51 "2-5")(vector-set! nodelabels 46 "2-6")(vector-set! nodelabels 40 "2-7")
 
;; switches
(vector-set! nodelabels 35 "S011")(vector-set! nodelabels 41 "S011")(vector-set! nodelabels 47 "S011")(vector-set! nodelabels 52 "S011")
(vector-set! nodelabels 56 "S011")(vector-set! nodelabels 61 "S011")(vector-set! nodelabels 69 "S012")(vector-set! nodelabels 76 "S012")
(vector-set! nodelabels 83 "S012")(vector-set! nodelabels 89 "S012")(vector-set! nodelabels 57 "S021")(vector-set! nodelabels 62 "S021")
(vector-set! nodelabels 36 "S022")(vector-set! nodelabels 42 "S022")(vector-set! nodelabels 48 "S022")(vector-set! nodelabels 53 "S022")
(vector-set! nodelabels 70 "S022")(vector-set! nodelabels 77 "S022")(vector-set! nodelabels 84 "S022")(vector-set! nodelabels 90 "S022")
(vector-set! nodelabels 54 "S031")(vector-set! nodelabels 91 "S031")(vector-set! nodelabels 37 "S032")(vector-set! nodelabels 43 "S032")
(vector-set! nodelabels 49 "S032")(vector-set! nodelabels 71 "S032")(vector-set! nodelabels 78 "S032")(vector-set! nodelabels 85 "S032")
(vector-set! nodelabels 45 "S041")(vector-set! nodelabels 80 "S041")(vector-set! nodelabels 39 "S042")(vector-set! nodelabels 73 "S042")
(vector-set! nodelabels 9 "S051")(vector-set! nodelabels 59 "S052")(vector-set! nodelabels 64 "S052")(vector-set! nodelabels 95 "S052")
(vector-set! nodelabels 99 "S052")(vector-set! nodelabels 65 "S061")(vector-set! nodelabels 100 "S061")(vector-set! nodelabels 60 "S062")
(vector-set! nodelabels 96 "S062")(vector-set! nodelabels 10 "S062")(vector-set! nodelabels 94 "S071")(vector-set! nodelabels 98 "S071")
(vector-set! nodelabels 58 "S072")(vector-set! nodelabels 63 "S072")(vector-set! nodelabels 38 "S081")(vector-set! nodelabels 44 "S081")
(vector-set! nodelabels 72 "S081")(vector-set! nodelabels 79 "S081")(vector-set! nodelabels 50 "S082")(vector-set! nodelabels 86 "S082")
(vector-set! nodelabels 23 "S091")(vector-set! nodelabels 26 "S092")(vector-set! nodelabels 1 "S101")(vector-set! nodelabels 12 "S101")
(vector-set! nodelabels 29 "S102")(vector-set! nodelabels 24 "S111")(vector-set! nodelabels 2 "S112")(vector-set! nodelabels 13 "S112")
(vector-set! nodelabels 3 "S121")(vector-set! nodelabels 14 "S122")(vector-set! nodelabels 25 "S122")(vector-set! nodelabels 28 "S161")
(vector-set! nodelabels 26 "S162")(vector-set! nodelabels 19 "S201")(vector-set! nodelabels 66 "S202")(vector-set! nodelabels 101 "S202")
(vector-set! nodelabels 6 "S231")(vector-set! nodelabels 33 "S231")
(vector-set! nodelabels 4 "S232")(vector-set! nodelabels 27 "S241")(vector-set! nodelabels 32 "S242")(vector-set! nodelabels 68 "S251")
(vector-set! nodelabels 75 "S251")(vector-set! nodelabels 82 "S251")(vector-set! nodelabels 88 "S251")(vector-set! nodelabels 93 "S252")
(vector-set! nodelabels 97 "S252")(vector-set! nodelabels 16 "S261")(vector-set! nodelabels 20 "S262")(vector-set! nodelabels 29 "S262")
(vector-set! nodelabels 21 "S271")(vector-set! nodelabels 30 "S272")(vector-set! nodelabels 6 "S281")(vector-set! nodelabels 15 "S282")

;; building railwaygraph.
(ug:add-edge! railwaygraph 0 1)(ug:add-edge! railwaygraph 1 2)(ug:add-edge! railwaygraph 2 3)(ug:add-edge! railwaygraph 3 4)(ug:add-edge! railwaygraph 4 5)
(ug:add-edge! railwaygraph 0 6)(ug:add-edge! railwaygraph 6 7)(ug:add-edge! railwaygraph 7 8)(ug:add-edge! railwaygraph 8 9)(ug:add-edge! railwaygraph 9 10)
(ug:add-edge! railwaygraph 10 11)(ug:add-edge! railwaygraph 0 12)(ug:add-edge! railwaygraph 12 13)(ug:add-edge! railwaygraph 13 14)(ug:add-edge! railwaygraph 14 11)
(ug:add-edge! railwaygraph 0 15)(ug:add-edge! railwaygraph 15 16)(ug:add-edge! railwaygraph 16 17)(ug:add-edge! railwaygraph 17 20)(ug:add-edge! railwaygraph 17 18)
(ug:add-edge! railwaygraph 18 19)(ug:add-edge! railwaygraph 19 5)(ug:add-edge! railwaygraph 20 21)(ug:add-edge! railwaygraph 21 22)(ug:add-edge! railwaygraph 22 23)
(ug:add-edge! railwaygraph 23 24)(ug:add-edge! railwaygraph 24 25)(ug:add-edge! railwaygraph 25 11)(ug:add-edge! railwaygraph 22 26)(ug:add-edge! railwaygraph 26 27)
(ug:add-edge! railwaygraph 27 28)(ug:add-edge! railwaygraph 28 5)(ug:add-edge! railwaygraph 17 29)(ug:add-edge! railwaygraph 29 30)(ug:add-edge! railwaygraph 30 31)
(ug:add-edge! railwaygraph 31 32)(ug:add-edge! railwaygraph 32 33)(ug:add-edge! railwaygraph 33 5)(ug:add-edge! railwaygraph 34 35)(ug:add-edge! railwaygraph 35 36)
(ug:add-edge! railwaygraph 36 37)(ug:add-edge! railwaygraph 37 38)(ug:add-edge! railwaygraph 38 39)(ug:add-edge! railwaygraph 39 40)(ug:add-edge! railwaygraph 34 41)
(ug:add-edge! railwaygraph 41 42)(ug:add-edge! railwaygraph 42 43)(ug:add-edge! railwaygraph 43 44)(ug:add-edge! railwaygraph 44 45)(ug:add-edge! railwaygraph 45 46)
(ug:add-edge! railwaygraph 34 47)(ug:add-edge! railwaygraph 47 48)(ug:add-edge! railwaygraph 48 49)(ug:add-edge! railwaygraph 49 50)(ug:add-edge! railwaygraph 50 51)
(ug:add-edge! railwaygraph 34 52)(ug:add-edge! railwaygraph 52 53)(ug:add-edge! railwaygraph 53 54)(ug:add-edge! railwaygraph 54 55)(ug:add-edge! railwaygraph 34 56)
(ug:add-edge! railwaygraph 56 57)(ug:add-edge! railwaygraph 57 58)(ug:add-edge! railwaygraph 58 59)(ug:add-edge! railwaygraph 59 60)(ug:add-edge! railwaygraph 60 11)
(ug:add-edge! railwaygraph 34 61)(ug:add-edge! railwaygraph 61 62)(ug:add-edge! railwaygraph 62 63)(ug:add-edge! railwaygraph 63 64)(ug:add-edge! railwaygraph 64 65)
(ug:add-edge! railwaygraph 65 66)(ug:add-edge! railwaygraph 66 5)(ug:add-edge! railwaygraph 67 68)(ug:add-edge! railwaygraph 68 69)(ug:add-edge! railwaygraph 69 70)
(ug:add-edge! railwaygraph 70 71)(ug:add-edge! railwaygraph 71 72)(ug:add-edge! railwaygraph 72 73)(ug:add-edge! railwaygraph 73 40)(ug:add-edge! railwaygraph 67 75)
(ug:add-edge! railwaygraph 75 76)(ug:add-edge! railwaygraph 76 77)(ug:add-edge! railwaygraph 77 78)(ug:add-edge! railwaygraph 78 79)(ug:add-edge! railwaygraph 79 80)
(ug:add-edge! railwaygraph 80 46)(ug:add-edge! railwaygraph 67 82)(ug:add-edge! railwaygraph 82 83)(ug:add-edge! railwaygraph 83 84)(ug:add-edge! railwaygraph 84 85)
(ug:add-edge! railwaygraph 85 86)(ug:add-edge! railwaygraph 86 51)(ug:add-edge! railwaygraph 67 88)(ug:add-edge! railwaygraph 88 89)(ug:add-edge! railwaygraph 89 90)
(ug:add-edge! railwaygraph 90 91)(ug:add-edge! railwaygraph 91 55)(ug:add-edge! railwaygraph 67 93)(ug:add-edge! railwaygraph 93 94)(ug:add-edge! railwaygraph 94 95)
(ug:add-edge! railwaygraph 95 96)(ug:add-edge! railwaygraph 96 11)(ug:add-edge! railwaygraph 67 97)(ug:add-edge! railwaygraph 97 98)(ug:add-edge! railwaygraph 98 99)
(ug:add-edge! railwaygraph 99 100)(ug:add-edge! railwaygraph 100 101)(ug:add-edge! railwaygraph 101 5)

;; function to determainate what direction the train should start it's path.
(define (direction? position destination)
  (hash-ref directionhash (string-append position destination)))
                               
;; this function calculates the path to destination, copy from a-d with small adaptions.
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

;; converts nodenumbers to nodenames
(define (route->labels list res)
  (if (empty? list)
      res
      (route->labels (cdr list) (cons (vector-ref nodelabels (car list))res))))

;; returns a list with the labels corresponding to the calculated route. 
(define (route-please from to)
  (route->labels (shortest-path railwaygraph (hash-ref nodehash from) (hash-ref nodehash to))'()))