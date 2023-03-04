#lang racket

;; 

(require malt)

(define line
  (λ (x)
    (λ (θ)
      (+ (* (ref θ 0) x) (ref θ 1)))))