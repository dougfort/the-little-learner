#lang racket

;; 

(module+ test
  (require rackunit))

(require malt)

(define linear-1-1
  (λ (t)
    (λ (theta)
      (+ (dot-product (ref theta 0) t) (ref theta 1)))))

(define relu-1-1
  (λ (t)
    (λ (theta)
      (rectify ((linear-1-1 t) theta)))))

(module+ test
  (test-begin
   ((relu-1-1 (tensor 2.0 1.0 3.0)) (list (tensor 7.1 4.3 -6.4) 0.6))))

(define k-relu
  (λ (k)
    (λ (t)
      (λ (θ)
        (cond
          [(zero? k) t]
          [else (((k-relu (sub1 k))
                  ((relu t) θ)
                  (refr θ 2)))])))))