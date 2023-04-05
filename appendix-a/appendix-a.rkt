#lang racket

;; 

(module+ test
  (require rackunit))

(require malt)

(define ∇
  (λ (f θ)
    (let ([wrt (map* dual* θ)])
      (∇-once (f wrt) wrt))))

(define dual
  (λ (r k)
    (tensor dual r k)))

(define dual?
  (λ (d)
    (cond
      [(vector? d) (eq? (tref d 0) dual)]
      [else #f])))

(define scalar?
  (λ (d)
    (cond
      [(number? d) #t]
      [else (dual? d)])))

;; if d is not a dual, we assume that d is a real number
(define ρ
  (λ (d)
    (cond
      [(dual? d) (tref d 1)]
      [else d])))

(define κ
  (λ (d)
    (cond
      [(dual? d) (tref d 2)]
      [else end-of-chain])))

(define map*
  (λ (f y)
    (cond
      [(scalar? y) (f y)]
      [(list? y)
       (map (λ (lm)
              (map* f lm))
            y)]
      [(vector? y)
       (vector-map (λ (ve)
                     (map* f ve))
                   y)])))

(define dual*
  (λ (d)
    (dual (ρ d)
          end-of-chain)))

(define ∇-once
  (λ (y wrt)
    (let ([σ (∇-σ y (hasheq))])
      (map* (λ (d)
              (hash-ref σ d 0.0))
            wrt))))

(define ∇-σ
  (λ (y σ)
    (cond
      [(scalar? y)
       (let ([k (κ y)])
         (k y 1.0 σ))]
      [(list? y)
       (∇-σ-list y σ)]
      [(vector? y)
       (∇-σ-vec y (sub1 (tlen y)) σ)])))

(define ∇-σ-list
  (λ (y σ)
    (cond
      [(null? y) σ]
      [else
       (let ([σ-hat (∇-σ (ref y 0) σ)])
         (∇-σ-list (refr y 1) σ-hat))])))

(define ∇-σ-vec
  (λ (y i σ)
    (let ([σ-hat (∇-σ (tref y i) σ)])
      (cond
        [(zero? i) σ-hat]
        [else (∇-σ-vec y (sub1 i) σ-hat)]))))

(define end-of-chain
  (λ (d z σ)
    (let ([g (hash-ref σ d 0.0)])
      (hash-set σ d (+ z g)))))

(define prim1
  (λ (ρ-fn ∇-fn)
    (λ (da)
      (let ([ra (ρ da)])
        (dual (ρ-fn ra)
              (λ (d z σ)
                (let ([ga (∇-fn ra z)])
                  ((κ da) da ga σ))))))))

(define prim2
  (λ (ρ-fn ∇-fn)
    (λ (da db)
      (let ([ra (ρ da)]
            [rb (ρ db)])
        (dual (ρ-fn ra rb)
              (λ (d z σ)
                (let-values ([(ga gb) (∇-fn ra rb z)])
                  (let ([σ-hat ((κ da) da ga σ)])
                    ((κ db) db gb σ-hat)))))))))

(define +-0-0
  (prim2 +
         (λ (ra rb z)
           (values z z))))

(define *-0-0
  (prim2 *
         (λ (ra rb z)
           (values (* rb z) (* ra z)))))

(define comparator
  (λ (f)
    (λ (da db)
      (f (ρ da) (ρ db)))))

(define <-0-0 (comparator <))
(define >-0-0 (comparator >))
(define <=-0-0 (comparator <=))
(define >=-0-0 (comparator >=))
(define =-0-0 (comparator =))

(define --0-0
  (prim2 -
          (λ (ra rb z)
            (z (- z)))))

(define /-0-0
  (prim2 /
         (λ (ra rb z)
           (values (* (/ 1 rb) z)
                   (* (/ (* -1 ra) (* rb rb)) z)))))

(define log-0
  (prim1 log
         (λ (ra z)
           (* (/ 1 ra) z))))

(define expt-0-0
  (prim2 expt
         (λ (ra rb z)
           (values (* z (* z (expt ra (- rb 1))))
                   (* (* (expt ra rb) (log ra)) z)))))

(define sqrt-0
  (prim1 (λ (ra)
           (expt ra 0.5))
         (λ (ra z)
           (* (* 0.5 (expt ra -0.5)) z))))
  