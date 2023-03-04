#lang racket

;; 

(module+ test
  (require rackunit))

(require malt)

(declare-hyper revs)
(declare-hyper alpha)

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
   (with-hypers
       ([revs 1000]
        [alpha 0.01])
     (let ([result (gradient-descent ((l2-loss line) line-xs line-ys) (list 0.0 0.0))])
       (check-true (< (- 1.05 (first result)) 0.1))
       (check-true (< (- 1.9e-6 (second result)) 0.1e-6))))))

(define quad
  (λ (t)
    (λ (theta)
      (+ (* (ref theta 0) (sqr t))
         (+ (* (ref theta 1) t) (ref theta 2))))))

(define quad-xs (tensor -1.0 0.0 1.0 2.0 3.0))
(define quad-ys (tensor 2.55 2.1 4.35 10.2 18.25))

(module+ test
  (test-begin
   (with-hypers
       ([revs 1000]
        [alpha 0.001])
     (let ([result (gradient-descent ((l2-loss quad) quad-xs quad-ys) (list 0.0 0.0 0.0))])
       (check-true (< (- 1.05 (first result)) 0.1))
       (check-true (< (- 1.9e-6 (second result)) 0.1e-6))))))

(define dot-product-1-1
  (λ (w t)
    (sum-1
     (* w t))))

(module+ test
  (test-begin
   (check-true (< (- 41.0 (dot-product-1-1 (tensor 2.0 1.0 7.0) (tensor 8.0 4.0 3.0))) 0.1))))

(define plane
  (λ (t)
    (λ (theta)
      (+ (dot-product-1-1 (ref theta 0) t) (ref theta 1)))))

(define plane-xs
  (tensor
   (tensor 1.0 2.05)
   (tensor 1.0 3.0)
   (tensor 2.0 2.0)
   (tensor 2.0 3.91)
   (tensor 3.0 6.13)
   (tensor 4.0 8.09)))

(define plane-ys
  (tensor 13.99
          15.99
          18.0
          22.4
          30.2
          37.94))

(define plane-ys-2
  (tensor 13.99
          15.99))
;   18.0
;   22.4
;   30.2
;   37.94))

(module+ test
  (test-begin
   (with-hypers
       ([revs 1000]
        [alpha 0.001])
     (let ([result (gradient-descent ((l2-loss plane) plane-xs plane-ys)
                                     (list (tensor 0.0 0.0) 0.0))])
       (println result)))))
