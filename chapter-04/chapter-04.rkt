#lang racket

;; 

(module+ test
  (require rackunit))

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

(module+ test
  (test-begin
   (let ([result (gradient-descent ((l2-loss line) line-xs line-ys) (list 0.0 0.0))])
     (check-true (< (- 1.05 (first result)) 0.1))
     (check-true (< (- 1.9e-6 (second result)) 0.1e-6)))))