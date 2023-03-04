#lang racket

;; 

(module+ test
  (require rackunit))

(require malt)

(declare-hyper revs)
(declare-hyper alpha)
(declare-hyper batch-size)

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

(module+ test
  (test-begin
   (with-hypers
       ([revs 1000]
        [alpha 0.01]
        [batch-size 4])
     (let ([result (gradient-descent (sampling-obj (l2-loss line) line-xs line-ys) (list 0.0 0.0))])
       (println result)))))

;(with-hypers
;    ([revs 15000]
;     [alpha 0.001]
;     [batch-size 4])
;  (gradient-descent
;   (sampling-obj (l2-loss plane) plane-xs plane-ys)
;   (list (tensor 0.0 0.0) 0.0)))