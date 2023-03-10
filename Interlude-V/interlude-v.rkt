#lang racket

;; 

(require malt/interlude-V)

#| 
(define sqrt
  (λ (t)
    (cond
      [(of-rank? 0 t) (sqrt-0 t)]
      [else (tmap sqrt t)])))
|#

(define of-rank?
  (λ (n t)
    (cond
      [(zero? n) (scalar? t)]
      [(scalar? t) #f]
      [else (of-rank? (sub1 n) (tref t 0))])))

#|
(define ext1
  (λ (f)
    (λ (t)
      (cond
        [(of-rank? 0 t) (f t)]
        [else (tmap (ext1 f) t)]))))

(define sqrt
  (ext1 sqrt-0))

(define zeroes
  (ext1 (λ (x) 0.0)))
|#

(define ext1
  (λ (f n)
    (λ (t)
      (cond
        [(of-rank? n t) (f t)]
        [else (tmap (ext1 f n) t)]))))

(define sqrt
  (ext1 sqrt-0 0))

(define zeroes
  (ext1 (λ (x) 0.0) 0))

(define sum
  (ext1 sum-1 1))

(define flatten
  (ext1 flatten-2 2))

#|
(define rank>
  (λ (t u)
    (> (rank t) (rank u))))
|#

(define rank>
  (λ (t u)
    (cond
      [(scalar? t) #f]
      [(scalar? u) #t]
      [else (rank> (tref t 0) (tref u 0))])))

(define of-ranks?
  (λ (n t m u)
    (cond
      [(of-rank? n t) (of-rank? m u)]
      [else #f])))

(define desc-t
 (λ (g t u)
   (tmap (λ (et) (g et u)) t)))

(define desc-u
  (λ (g t u)
    (tmap (λ (eu) (g t eu)) u)))

(define desc
  (λ (g n t m u)
    (cond
      [(of-rank? n t) (desc-u g t u)]
      [(of-rank? m u) (desc-t g t u)]
      [(= (tlen t) (tlen u)) (tmap g t u)]
      [(rank> t u) (desc-t g t u)]
      [else (desc-u g t u)])))

(define ext2
  (λ (f n m)
    (λ (t u)
      (cond
        [(of-ranks? n t m u) (f t u)]
        [else (desc (ext2 f n m) n t m u)]))))

(define +
  (ext2 +-0-0 0 0))

(define *
  (ext2 *-0-0 0 0))

(define sqr
  (λ (t)
    (* t t)))

(define dot-product
  (ext2 dot-product-1-1 1 1))

(define *-1-1
  (ext2 * 1 1))

(define *-2-1
  (ext2 *-1-1 2 1))