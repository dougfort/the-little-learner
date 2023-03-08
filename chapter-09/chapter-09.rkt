#lang racket

;; 

(module+ test
  (require rackunit))

(require malt)

(declare-hyper revs)
(declare-hyper alpha)
(declare-hyper batch-size)
(declare-hyper mu)
(declare-hyper beta)

(define line
  (λ (x)
    (λ (θ)
      (+ (* (ref θ 0) x) (ref θ 1)))))

(define line-xs (tensor 2.0 1.0 4.0 3.0))
(define line-ys (tensor 1.8 1.2 4.2 3.3))

; using the malt dot-product, can't get dot-product-1-1 to work
(define plane
  (λ (t)
    (λ (theta)
      (+ (dot-product (ref theta 0) t) (ref theta 1)))))

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

(define smooth
  (λ (decay-rate average g)
    (+ (* decay-rate average) (* (- 1.0 decay-rate) g)))) 

(define l2-loss
  (λ (target)
    (λ (xs ys)
      (λ (theta)
        (let ([pred-ys ((target xs) theta)])
          (sum
           (sqr
            (- ys pred-ys))))))))

(define lonely-i
  (λ (p)
    (list p)))

(define lonely-d
  (λ (p)
    (ref p 0)))

(define lonely-u
  (λ (p g)
    (list (- (ref p 0) (* alpha g)))))

(define naked-i
  (λ (p)
    (let ([p p])
      p)))

(define naked-d
  (λ (p)
    (let ([p p])
      p)))

(define naked-u
  (λ (p g)
    (- p (* alpha g))))

(define velocity-i
  (λ (p)
    (list p (zeroes p))))

(define velocity-d
  (λ (p)
    (ref p 0)))

(define velocity-u
  (λ (p g)
    (let ([v (- (* mu (ref p 1)) (* alpha g))])
      (list (+ (ref p 0) v) v))))

(define rms-i
  (λ (p)
    (list p (zeroes p))))

(define rms-d
  (λ (r)
    (ref r 0)))

(define epsilon 1e-8)
(define rms-u
  (λ (p g)
    (let ([r (smooth beta (ref p 1) (sqr g))])
      (let ([alpha-hat (/ alpha (+ (sqrt r) epsilon))])
        (list (- (ref p 0) (* alpha-hat g)) r)))))

(define adam-i
  (λ (p)
    (let ([v (zeroes p)])
      (let ([r v])
        (list p v r)))))

(define adam-d
  (λ (p)
    (ref p 0)))

(define adam-u
  (λ (p g)
    (let ([r (smooth beta (ref p 2) (sqr g))])
      (let ([alpha-hat (/ alpha (+ (sqrt r) epsilon))]
            [v (smooth mu (ref p 1) g)])
        (list (- (ref p 0) (* alpha-hat v)) v r)))))

(define revise
  (λ (f revs theta)
    (cond
      [(zero? revs) theta]
      [else (revise f (sub1 revs) (f theta))])))

(define gradient-descent
  (λ (inflate deflate update)
    (λ (obj theta)
      (let ([f (λ (big-theta)
                 (map update
                      big-theta
                      (gradient-of obj (map deflate big-theta))))])
        (map deflate (revise f revs (map inflate theta)))))))

(define lonely-gradient-descent
  (gradient-descent lonely-i lonely-d lonely-u))

(define naked-gradient-descent
  (gradient-descent naked-i naked-d naked-u))

(define velocity-gradient-descent
  (gradient-descent velocity-i velocity-d velocity-u))

(define rms-gradient-descent
  (gradient-descent rms-i rms-d rms-u))

(define adam-gradient-descent
  (gradient-descent adam-i adam-d adam-u))

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

(define try-line
  (λ (a-gradient-descent a-revs)
    (with-hypers
        ([revs a-revs]
         [alpha 0.001]
         [batch-size 4])
      (a-gradient-descent
       (sampling-obj (l2-loss line) line-xs line-ys) (list 0.0 0.0)))))

(module+ test
  (test-begin
   (with-hypers
       ([mu 0.9])        
     (try-line velocity-gradient-descent 5000))))

(define try-plane
  (λ (a-gradient-descent a-revs an-alpha)
    (with-hypers
        ([revs a-revs]
         [alpha an-alpha]
         [batch-size 4])
      (a-gradient-descent
       (sampling-obj (l2-loss plane) plane-xs plane-ys) (list (tensor 0.0 0.0) 0.0)))))

(module+ test
  (test-begin
   (with-hypers
       ([mu 0.9])
     (try-plane velocity-gradient-descent 5000 0.001))))

(module+ test
  (test-begin
   (with-hypers
       ([beta 0.9])
     (try-plane rms-gradient-descent 3000 0.01))))

(module+ test
  (test-begin
   (with-hypers
       ([mu 0.85]
        [beta 0.9])
     (try-plane adam-gradient-descent 1500 0.01))))
