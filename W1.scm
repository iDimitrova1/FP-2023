#lang racket

;;zad 0
(*(+ 10 5.16 19 9.712361) (- 20 (- 16 4)))

(+ (/ 1 4) (/ 2 5) (/ 3 8) (* 6 (- 5.1 1.6) (- (/ 9 3) (/ 7 4)) ) ) 

(+ (expt 3 (/ 60 7))  (/ (expt 2 10) 179.) )

(expt 1-i 21)

;;zad 1
(define (iseven? n)
  (= (remainder n 2) 0) )

(define (signum n)
  (cond
    ((< n 0) -1)
    ((> n 0) 1)
    ((= n 0) 0) ) )

(define (root? x)
  (= 0 (+ (* 3 (* x x)) (* 2 x) -1)) )

(define (triangle? a b c)
  (and (> (+ a b) c) (> (+ b c) a) (> (+ a c) b) #t) )

(define (fibonacci n)
  (if (<= n 2)
      1
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (sum-interval a b)
  (if (= a b)
      b
      (+  a (sum-interval (+ 1 a) b)) ) )

(define (PBE a b)
  (if (> b 0)
      (if (= b 1)
          a
          (* a (PBE a (- b 1))) )
      (if (= b -1)
          (/ 1. a)
      (* (/ 1. a) (PBE a (+ b 1))) ) ) )

(define (reverse-digits n)
  (define (digits m)
    (if (= 0 (quotient m 10))
           1
           (+ 1 (digits (quotient m 10))) ) )
   (if (= 0 (quotient n 10))
       n
      (+ (* (expt 10 (- (digits n) 1)) (remainder n 10)) (reverse-digits (quotient n 10)) ) ) )