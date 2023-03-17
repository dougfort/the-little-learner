#lang racket
;; 

(module+ test
  (require rackunit))

(require malt)
(require malt/examples/iris)

(define dense-block
  (λ (n m)
    (block relu
           (list
            (list m n)
            (list m)))))

(define iris-network
  (stack-blocks
   (list
    (dense-block 4 6)
    (dense-block 6 3))))
         
(define iris-classifier
  (block-fn iris-network))

(define iris-θ-shapes
  (block-ls iris-network))

(define init-θ
  (λ (shapes)
    (map init-shape shapes)))

(define init-shape
  (λ (s)
    (println s)
    (println (ref s 1))
    (cond
      [(= (len s) 1) (zero-tensor s)]
      [(= (len s) 2) (random-tensor 0.0 (/ 2.0 (ref s 1)) s)])))

; (random-tensor 0.0 (/ 2.0 (ref s 1)) s)
; fails due too their weird internal representation,
; so I'm just going to use  tll-iris-initial-theta

(define iris-θ
  (with-hypers
      ([revs 2000]
       [alpha 0.0002]
       [batch-size 8])
    (naked-gradient-descent
     (sampling-obj
      (l2-loss iris-classifier)
      iris-train-xs iris-train-ys)
;     (init-θ iris-θ-shapes))))
     tll-iris-initial-theta)))

#|
(define iris-model
  (λ (t)
    ((iris-classifier t) iris-θ)))
|#

(define model
  (λ (target θ)
    (λ (t)
      ((target t) θ))))

(define iris-model
  (model iris-classifier iris-θ))