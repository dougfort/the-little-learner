#lang racket

;; 

(require malt)

(require "chapter-03.rkt")

(define revise
  (λ (f revs theta)
    (cond
      [(zero? revs) theta]
      [else (revise f (sub1 revs) (f theta))])))

(define revs 1000)
(define alpha 0.01)

(define gradient-descent
  (λ (obj theta)
    (let ([f (λ (big-theta)
               (map (λ (p g)
                      (- p (* alpha g)))
                    big-theta
                    (gradient-of obj big-theta)))])
      (revise f revs theta))))

(gradient-descent ((l2-loss line) line-xs line-ys) (list 0.0 0.0))  