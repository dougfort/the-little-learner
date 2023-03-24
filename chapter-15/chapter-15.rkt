#lang racket
;; 

(module+ test
  (require rackunit))

(require malt)
(require malt/examples/morse)

#|
(define corr
  (λ (t)
    (λ (θ)
      (+ (correlate (ref θ 0) t) (ref θ 1)))))

(define recu
  (λ (t)
    (λ (θ)
      (rectify ((corr t) θ)))))

(define recu-block
  (λ (b m d)
    (block recu
           (list
            (list b m d)
            (list b)))))

(define sum-2 sum-1)

(define train-morse
  (λ (network)
    (with-hypers
        ([alpha 0.0005]
         [revs 20000]
         [batch-size 8]
         [mu 0.9]
         [beta 0.999])
      (trained-morse
       (block-fn network)
       (block-ls network)))))

(define trained-morse
  (λ (classifier θ-shapes)
    (model classifier
           (adam-gradient-descent
            (sampling-obj
             (l2-loss classifier)
             morse-train-xs
             morse-train-ys)
            (init-theta θ-shapes)))))

(accuracy
 (train-morse morse-fcn)
 morse-test-xs morse-test-ys)
|#

(define fcn-model
  (train-morse morse-residual))

(accuracy fcn-model
          morse-test-xs morse-test-ys)
