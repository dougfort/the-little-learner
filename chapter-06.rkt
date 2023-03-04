#lang racket

;; 

(require (except-in malt gradient-descent alpha set-revs! set-alpha! revs))

(require "chapter-03.rkt")
(require "chapter-05.rkt")


(declare-hyper batch-size)

(define samples
  (λ (n s)
    (sampled n s (list))))

(define sampled
  (λ (n i a)
    (cond
      [(zero? i) a]
      [else (sampled n (sub1 i) (cons (random n) a))])))

(define sampling-obj
  (λ (expectant xs ys)
    (let ([n (tlen xs)])
      (λ (theta)
        (let ([b (samples n batch-size)])
          ((expectant (trefs xs b) (trefs ys b)) theta))))))

(with-hypers
    ([revs 1000]
     [alpha 0.01]
     [batch-size 6])
  (gradient-descent
   (sampling-obj
    (l2-loss line) line-xs line-ys)
   (list 0.0 0.0)))

(with-hypers
    ([revs 15000]
     [alpha 0.001]
     [batch-size 4])
  (gradient-descent
   (sampling-obj (l2-loss plane) plane-xs plane-ys)
   (list (tensor 0.0 0.0) 0.0)))