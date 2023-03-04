#lang racket

;; 

(require (except-in malt ))

(require "chapter-03.rkt")

(provide quad-xs quad-ys plane-xs plane-ys gradient-descent)

(declare-hyper revs)
(declare-hyper alpha)

(define quad-xs (tensor -1.0 0.0 1.0 2.0 3.0))
(define quad-ys (tensor 2.55 2.1 4.35 10.2 18.25))

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

(with-hypers
    ([revs 1000]
     [alpha 0.01])
  (gradient-descent
   ((l2-loss line) line-xs line-ys)
   (list 0.0 0.0))  )

(define quad
  (λ (t)
    (λ (theta)
      (+ (* (ref theta 0) (sqr t))
         (+ (* (ref theta 1) t) (ref theta 2))))))

(with-hypers
    ([revs 1000]
     [alpha 0.001])
  (gradient-descent
   ((l2-loss quad) quad-xs quad-ys)
   (list 0.0 0.0 0.0)))

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

(define dot-product-1-1
  (λ (w t)
    (sum-1
     (* w t))))

(dot-product-1-1 (tensor 2.0 1.0 7.0) (tensor 8.0 4.0 3.0))

(define plane
  (λ (t)
    (λ (theta)
      (+ (dot-product-1-1 (ref theta 0) t) (ref theta 1)))))

(with-hypers
    ([revs 1000]
     [alpha 0.001])
  ((l2-loss plane) plane-xs plane-ys)
  (list (tensor 0.0 0.0) 0.0))

(with-hypers
    ([revs 1000]
     [alpha 0.001])
  (gradient-descent
   ((l2-loss plane) plane-xs plane-ys-2)
   (list (tensor 0.0 0.0) 0.0)))
