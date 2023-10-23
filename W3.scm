#lang racket

(define (apply2 func)
  (lambda (a b)
    (func a b)))

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b)
      nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))
;zad 1
(define (1+ x) (+ 1 x))
(define comp (lambda (f g x) (f (g x)) ) ) ;vru6ta st.
(define (comp1 f g) (lambda (x) (f (g x)) ) ) ;vru6ta fn.
;zad 3.1
(define (repeat n f)
  (define (iter n f-new)
    (if (> n 1)
        (iter (- n 1) (comp1 f-new f))
        f-new))
  (iter n  f))
;zad 3.2
(define (id x) x)

(define (repeat2 n f)
  (if (<= 1 n)
      f
      (comp1 f (repeat2 (- n 1) f))))
;zad 2
(define (repeated n f x) ((repeat n f) x))

;zad n!
(define (n! n)
  (accumulate * 1 1 n id 1+))
;zad x^n
(define (x^n x n)
  (accumulate * 1 1 n id 1+))