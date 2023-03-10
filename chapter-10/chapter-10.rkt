#lang racket

;; 

(module+ test
  (require rackunit))

(require malt)

#|
(define dot-product
  (ext2 dot-product-1-1 1 1))
|#

(define rectify-0
  (λ (s)
    (cond
      [(< s 0.0) 0.0]
      [else s])))

#|
(define rectify
  (ext1 rectify-0 0))
|#

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

(define half-strip
  (λ (x theta)
    (- ((relu-1-1 (tensor x)) (list (ref theta 0) (ref theta 1)))
       ((relu-1-1 (tensor x)) (list (ref theta 0) (ref theta 2))))))

(module+ test
  (test-begin
   (half-strip 0.0 (list (tensor 1.0) -1.0 -1.5)))
  (test-begin
   (half-strip 1.5 (list (tensor 1.0) -1.0 -1.5)))
  (test-begin
   (half-strip 2.0 (list (tensor 1.0) -1.0 -1.5))))

(define full-strip
  (λ (x theta)
    (- (half-strip x (list (ref theta 0) (ref theta 1) (ref theta 2)))
       (half-strip x (list (ref theta 3) (ref theta 4) (ref theta 5))))))

(module+ test
  (test-begin
   (full-strip 0.1 (list (tensor 1.0) -1.0 -1.5 (tensor 1.0) -3.0 -3.5)))
  (test-begin
   (full-strip 2.0 (list (tensor 1.0) -1.0 -1.5 (tensor 1.0) -3.0 -3.5)))
  (test-begin
   (full-strip 4.0 (list (tensor 1.0) -1.0 -1.5 (tensor 1.0) -3.0 -3.5))))
