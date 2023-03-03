#lang racket

;; 

(require malt)

(define rank-old
  (λ (t)
    (cond
      [(scalar? t) 0]
      [else (add1 (rank (tref t 0)))])))

(define rank
  (λ (t)
    (ranked t 0)))

(define ranked
  (λ (t a)
    (cond
      [(scalar? t) a]
      [else (ranked (tref t 0) (add1 a))])))

(define shape
  (λ (t)
    (cond
      [(scalar? t) (list)]
      [else (cons (tlen t) (shape (tref t 0)))])))

(define sum-1
  (λ (t)
    (summed t (sub1 (tlen t)) 0.0)))

(define summed
  (λ (t i a)
    (cond
      [(zero? i) (+ (tref t 0) a)]
      [else (summed t (sub1 i) (+ (tref t i) a))])))