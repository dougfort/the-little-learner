#lang racket

;; 

(require malt)

(define line
  (λ (x)
    (λ (θ)
      (+ (* (ref θ 0) x) (ref θ 1)))))

(define line-xs (tensor 2.0 1.0 4.0 3.0))
(define line-ys (tensor 1.8 1.2 4.2 3.3))

(define l2-loss
  (λ (target)
    (λ (xs ys)
      (λ (theta)
        (let ([pred-ys ((target xs) theta)])
          (sum
           (sqr
            (- ys pred-ys))))))))

