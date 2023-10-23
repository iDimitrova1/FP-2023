#lang racket

;zad 1
(define (sum-digits n)
  (if (= n 0)
      0
      (+ (remainder n 10)
         (sum-digits (quotient n 10)))))

(define (sum-digits-iter n)
  (define (iter i sum)
    (if (= i 0)
        sum
        (iter (quotient i 10)
              (+ sum (remainder i 10)))))
(iter n 0))

;zad 2
(define (count-dividers-iter n)
  (define (iter n i div)
    (if (> i (quotient n 2))
           div
           (if (= 0 (remainder n i))
                (iter n (+ i 1) (+ 1 div))
                (iter n (+ i 1) div))))
  (iter n 2 2))

;zad 3
(define (prime? n)
  (define (iter n i sw)
    (if (> (+ sw (* i 6)) (sqrt n))
           "prime"
           [if (= 0 (remainder n (+ (* i 6) sw)))
                "not prime"
                {if [= sw 1]
                    (iter n (+ i 1) 5)
                (iter n (+ i 1) 1)}]))
  {if (= 0 (remainder n 2))
       "not prime"
       {if (= 0 (remainder n 3))
       "not prime"
       (iter n 0 5)}})

(define (prime1? n)
  (define (iter n i sw)
    (if (> (+ sw (* i 30)) (sqrt n))
           "prime"
           [cond {(= 0 (remainder n (+ (* i 30) sw))) "not prime"}
                {[= sw 1] (iter n i 7)}
                {[= sw 7] (iter n i 11)}
                {[= sw 11] (iter n i 13)}
                {[= sw 13] (iter n i 17)}
                {[= sw 17] (iter n i 19)}
                {[= sw 19] (iter n i 23)}
                {[= sw 23] (iter n i 29)}
                {[= sw 29] (iter n (+ i 1) 1)}]))
  {cond {(= 0 (remainder n 2)) "not prime"}
        {(= 0 (remainder n 3)) "not prime"}
        {(= 0 (remainder n 5)) "not prime"}
       {else (iter n 0 7)}})

(define (prime2? n)
  (define (iter n i)
    (if (> i (sqrt n))
           "prime"
           [if (= 0 (remainder n i))
                "not prime"
                (iter n (+ i 1))]))
       (iter n 2))

;zad 4
(define (increasing-digits? n)
  (if (= 0 (quotient n 10))
      #t
      (if (< (remainder n 10) (remainder (quotient n 10) 10) )
          #f
          (increasing-digits? (quotient n 10)) )))

;zad 5
(define (ends-with? n k)
  (if (< n k)
      #f
      (if (= (remainder n 10) (remainder k 10))
          (if (= 0(quotient k 10))
              #t
          (ends-with? (quotient n 10) (quotient k 10)))
          #f)))

(define (automorphic? n)
  (ends-with? (* n n) n))
  
;zad 6
(define (perfect? n)
  (define (divisors-sum i sum)
    (if (> i (quotient n 2))
        sum
        (if (= 0 (remainder n i))
          (divisors-sum (+ i 1) (+ sum i))
          (divisors-sum (+ i 1) sum))))
  (= n (divisors-sum 2 1)) )

;zad 7
(define (binary-to-decimal n)
  (define (iter i n)
    (if (> n 0)
        (if (= (remainder n 10) 1)
          (+ (expt 2 i) (iter (+ 1 i) (quotient n 10)))
          (iter (+ 1 i) (quotient n 10)))
        0))
  (iter 0 n))

(define (binary-to-decimal n)
  (define (iter i n num)
    (if (> n 0)
        (if (= (remainder n 10) 1)
           (iter (+ 1 i) (quotient n 10) (+ (expt 2 i) num))
          (iter (+ 1 i) (quotient n 10) num))
        num))
  (iter 0 n 0))

;zad 8
(define (decimal-to-binary n)
  (define (iter i n)
    (if (> n 0)
        (if (= (remainder n 2) 1)
          (+ (expt 10 i) (iter (+ 1 i) (quotient n 2)))
          (iter (+ 1 i) (quotient n 2)))
        0))
  (iter 0 n))