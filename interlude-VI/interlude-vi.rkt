#lang racket
;; 

(module+ test
  (require rackunit))

(require malt)
(require malt/examples/iris)

(define argmax-1
  (λ (t)
    (let ([i (sub1 (tlen t))])
      (argmaxed t i i))))

(define argmaxed
  (λ (t i a)
    (let ([a-hat (next-a t i a)])
      (cond
        [(zero? i) a-hat]
        [else (argmaxed t (sub1 i) a-hat)]))))

(define next-a
  (λ (t i a)
    (cond
      [(> (tref t i) (tref t a)) i]
      [else a])))

(define class=-1
  (λ (t u)
    (cond
      [(= (argmax-1 t) (argmax-1 u)) 1.0]
      [else 0.0])))

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

(define accurate-enough-iris-θ?
  (λ (θ)
    (>= (accuracy
         (model iris-classifier θ)
         iris-test-xs iris-test-ys)
        0.9)))

(grid-search
 accurate-enough-iris-θ?
 ([revs 500 1000 2000 4000]
  [alpha 0.0001 0.0002 0.0005]
  [batch-size 4 8 16])
 (naked-gradient-descent
  (sampling-obj
   (l2-loss iris-classifier)
   iris-train-xs iris-train-ys)
  tll-iris-initial-theta))
