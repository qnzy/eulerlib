; some function for projecteuler problems
; don't forget lcm, gcd

(require-extension numbers srfi-1)

; check for prime, simple version
; test:
; (= 76127 (reduce + 0 (filter prime? (iota 1000 1))))
(define (prime? n)
  (cond
    ((< n 2) #f)
    ((= n 2) #t)
    (else (not (any (lambda (x) (= 0 (modulo n x)))
                    (cons 2 (iota (inexact->exact (floor (/ (sqrt n) 2))) 3 2)))))))

; return primes up to n (inclusive)
(define (primes-to n)
  (if (< n 2)
    '()
    (filter prime? 
            (cons 2 (iota (inexact->exact (ceiling (/ n 2))) 1 2)))))

; returns n-th prime
(define (nth-prime n)
  (define (nth-prime-aux n from primes-found)
    (if (= n primes-found)
      (- from 1)
      (if (prime? from)
        (nth-prime-aux n (+ from 1) (+ primes-found 1))
        (nth-prime-aux n (+ from 1) primes-found))))
  (nth-prime-aux n 0 0))

; returns all prime factors of n (with their multiplicity)
; test with following line:
; (equal? (map (lambda (x) (apply * x)) (map prime-factors (iota 10000 1))) (iota 10000 1))
(define (prime-factors n)
  (define (prime-factors-aux n primes-to-test factors)
    (if (null? primes-to-test)
      (if (> n 1) (cons n factors) factors)
      (let ((candidate (car primes-to-test)) (rest-of-primes (cdr primes-to-test)))
        (if (= 0 (modulo n candidate))
          (let ((n-divided (quotient n candidate)))
            (prime-factors-aux n-divided (filter (lambda (x) (<= x (sqrt n-divided))) primes-to-test) (cons candidate factors)))
          (prime-factors-aux n rest-of-primes factors)))))
  (prime-factors-aux n (primes-to (inexact->exact (ceiling (sqrt n)))) '()))

; return n-th Fibonacci number (1 1 2 3 5 8 ...)
; return the prime factors of n
(define (fib n)
  (if (< n 3)
    1
    (+ (fib (- n 1)) (fib (- n 2)))))

; checks if a string is a palindrome
; calls number->string for non-strings
(define (palindrome? str)
  (if (string? str)
    (if (< (string-length str) 2) 
      #t
      (and (char=? (string-ref str 0) (string-ref str (- (string-length str) 1)))
           (palindrome? (substring str 1 (- (string-length str) 1)))))
    (palindrome? (number->string str))))

; checks if n is divisible by all elements of lst
(define (divisible-by-all lst n)
  (not (any (lambda (x) (not (zero? x)))
            (map (lambda (x) (modulo n x)) lst))))

(define (triangle n)
  (/ (* n (+ n 1)) 2))

(define (triangle? n)
  (and (> n 0) (integer? (/ (+ 1 (sqrt (+ 1 (* 8 n)))) 2))))

(define (pentagonal n)
  (/ (* (- (* n 3) 1) n) 2))

(define (pentagonal? n)
  (and (> n 0) (integer? (/ (+ 1 (sqrt (+ 1 (* 24 n)))) 6))))

(define (hexagonal n)
  (* (- (* n 2) 1) n))

(define (hexagonal? n)
  (and (> n 0) (integer? (/ (+ 1 (sqrt (+ 1 (* 8 n)))) 4))))

; check if y is a leap year
(define (leap-year? y)
  (and (zero? (modulo y 4)) (or (not (zero? (modulo y 100))) (zero? (modulo y 400)))))

; generate all palindromes width d or fewer digits
; TODO: does not generate odd length palindromes yet
(define (gen-palindromes n) 
  (map string->number
       (append '("1" "2" "3" "4" "5" "6" "7" "8" "9") 
               (map (lambda (x) (string-append x (string-reverse x))) 
                    (map number->string (iota (inexact->exact n) 1))))))

; log to base 10
(define (log10 x)
  (/ (log x) (log 10)))

; define a repunit (repeating 1) of length n)
(define (repunit n)
 (/ (- (expt 10 n) 1) 9))

; check if n is a lychrel number
; (if repeatedly adding it to its mirror
; does not produce a palindrome)
(define (lychrel? n)
  (define (transform  n)
    (+ n (string->number (string-reverse (number->string n)))))
  (define  (lychrel?-aux n i) 
    (if (palindrome? n) #f 
      (if (<= i 50) (lychrel?-aux (transform n) (+ i 1)) #t)))
  (lychrel?-aux (transform n) 1))

; find numbers n where (fun n) is true; start
; at nstart and search for nsols numbers.
; all integers starting at nstart are tried.
; eg (find-where prime? 1 5)
(define (find-where fun nstart nsols)
  (define (find-where-aux fun n nsols lst)
    (if (>= (length lst) nsols)
      lst
      (if (fun n)
        (find-where-aux fun (+ n 1) nsols (cons n lst)) 
        (find-where-aux fun (+ n 1) nsols lst))))
  (find-where-aux fun nstart nsols '()))

