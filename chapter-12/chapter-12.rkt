#lang racket
;; 

(module+ test
  (require rackunit))

(require malt)

(define layer1
  (block relu
         (list
          (list 64 32)
          (list 64))))

(define layer2
  (block relu
         (list
          (list 45 64)
          (list 45))))

(define layer3
  (block relu
         (list
          (list 26 45)
          (list 26))))

(define 3-layer-network
  (stack-blocks
   (list
    layer1
    layer2
    layer3)))