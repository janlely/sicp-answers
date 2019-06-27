;sum recursion version
(define (sum1 f a next b)
  (if (> a b)
    0
    (+ (f a) (sum1 f (next a) next b))))

(sum1 (lambda (x) x) 1 (lambda (x) (+ 1 x)) 10)

;sum iteration version
(define (sum2 f a next b)
  (define (iter a res)
    (if (> a b)
      res
      (iter (next a) (+ (f a) res))))
  (iter a 0))

;计算定积分的近似值
(define (integral f a b dx)
  (define (next x)
    (+ x dx))
  (* dx (sum2 f (+ a (/ dx 2)) next b)))

(define (integral2 f a b n)
  (define h (/ (- b a) n))
  (define (fk k)
    (if (= k 0)
      (f a)
      (if (= k n)
        (f (+ a (* k h)))
        (if (odd? k)
          (* 4 (f (+ a (* k h))))
          (* 2 (f (+ a (* k h))))))))
  (* (/ h 3) (sum2 fk 0 (lambda (x) (+ 1 x)) n)))

;product recursion version
(define (product f next a b)
  (if (> a b)
    1
    (* (f a) (product f next (next a) b))))

;product iteration version
(define (product2 f next a b)
  (define (iter cur res)
    (if (> cur b)
      res
      (iter (next cur) (* res (f cur)))))
  (iter a 1))

;exercise 1.31
;(define pi_4 (/ (product2 (lambda (x) (* 4.0 x (+ x 1))) (lambda (x) (+ 1 x)) 1 50)
                ;(product2 (lambda (x) (* (+ (* 2 x) 1) (+ (* 2 x) 1))) (lambda (x) (+ 1 x)) 1 50)))
(define pi_4 (product2 (lambda (n) (/ (* 4.0 n (+ n 1)) (* (+ 1 (* 2 n)) (+ 1 (* 2 n))))) (lambda (n) (+ 1 n)) 1 1000))

;exercise 1.32
(define (accumulate combiner null-value f next a b)
  (define (iter cur res)
    (if (> cur b)
      res
      (iter (next cur) (combiner res (f cur)))))
  (iter a null-value))

;exercise 1.33
(define (filtered-accumulate combiner the-filter? null-value f next a b)
  (define (iter cur-idx res)
    (define cur-value (f cur-idx))
    (if (> cur-idx b)
      res
      (iter (next cur-idx) (combiner res (if (the-filter? cur-value) cur-value null-value)))))
  (iter a null-value))


;a)
(define (is-prime? n)
  (define (square n) (* n n))
  (define (iter cur n)
    (cond ((> (square cur) n) #t)
          ((= (modulo n cur) 0) #f)
          (else (iter (+ 1 cur) n))))
  (iter 2 n))

(define (prime-sum a b)
  (define (add x y) (+ x y))
  (define (id x) x)
  (define (next x) (+ 1 x))
  (filtered-accumulate add is-prime? 0 id next a b))

;b)
(define (is-prime2? m n) (= (gcd m n) 1))

(define (prime-sum2 n)
  (define (add x y) (+ x y))
  (define (id x) x)
  (define (next x) (+ 1 x))
  (filtered-accumulate add (lambda (x) (is-prime2? x n)) 0 id next 1 n))

;exercise 1.34
(define (f g)
  (g 2))

;(f square) => (square 2) => 4
;(f f) => (2 2) => error

;exercise 1.35 1.36
(define (fixed-point f first-guess)
  (define tolerance 0.0001)
  (define (closs-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((guess2 (f guess)))
      (display "trace: ")
      (display guess)
      (display " -> ")
      (display guess2)
      (newline)
      (if (closs-enough? guess guess2)
        guess
        (try guess2))))
  (try first-guess))

(define golden-cut (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

(define xx (fixed-point (lambda (x) (/ (log 1000) (log x))) 1.5))

;exercise 1.37
;recursion version
(define (cont-frac-recursion n d k)
  (define (cont-frac n d i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (cont-frac n d (+ i 1))))))
  (cont-frac n d 1))
;iteration version
(define (cont-frac-iteration n d k)
  (define (cont-frac i res)
    (if (< i 1)
      res
      (cont-frac (- i 1) (/ (n i) (+ (d i) res)))))
  (cont-frac k 0))

;execirse 1.38
(define (ebase k)
  (define (ebase-d i)
    (cond ((= i 1) 1)
          ((= i 2) 2)
          ((= (modulo  (- i 2) 3) 0) (/ (* 2 (+ i 1)) 3))
          (else 1)))
  (+ 2 (cont-frac-iteration (lambda (i) 1.0) ebase-d k)))

;exercise 1.39
(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
      x
      (* x x -1.0)))
  (define (d i)
    (- (* 2 i) 1))
  (cont-frac-iteration n d k))

;newton transform
(define dx 0.0001)
(define (deriv f)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (fixed-point-transform g transform guess)
  (fixed-point (transform g) guess))

(define (my-sqrt1 n guess)
  (fixed-point-transform (lambda (x) (- (* x x) n))
                         newton-transform
                         guess))

(define (my-sqrt2 n guess)
  (define (average x y)
    (/ (+ x y) 2))
  (define (average-damp f)
    (lambda (x) (average (f x) x)))
  (fixed-point-transform (lambda (x) (/ n x))
                         average-damp
                         guess))

;exercise 1.40
(define (cubic a b c)
  (define (cube x) (* x x x))
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

;exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))
(define (inc x)
  (+ 1 x))

;(((double (double double)) inc) 5) => 21
;double: => f -> f^2 -> ff -> 2xf
;(double double): ff -> ff^2 -> 4xf
;(double (double double)): ffff -> ffff^2 -> 16xf
;double alias d
;d => \f -> f . f => 2f
;d d => d . d => 2f x 2f = 4f
;d (d d) => (d d) . (d d) => d . d . d . d => 2f x 2f x 2f x 2f = 16f

;exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;exercise 1.43
(define (repeat f n)
  (lambda (x)
    (if (= 0 n)
      x
      (f ((repeat f (- n 1)) x)))))

;exercise 1.44
(define dx 0.0001)
(define (smooth f)
  (lambda (x)
    (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3)))

(define (n-smooth f n)
  (lambda (x)
    ((repeat smooth n) x)))

;exercise 1.45
(define (average-damp f)
  (define (average x y)
    (/ (+ x y) 2))
  (lambda (x) (average (f x) x)))

(define (fixed-point-n f n guess)
  (fixed-point ((repeat average-damp n) f) guess))

;exercise 1.46
(define (iterative-improve good-enough? improve-method)
  (lambda (guess)
    (if (good-enough? guess)
      guess
      ((iterative-improve good-enough? improve-method) (improve-method guess)))))

(define (iter-sqrt x guess)
  (define (average x y)
    (/ (+ x y) 2))
  (define tolerance 0.0001)
  ((iterative-improve (lambda (guess)
                       (< (abs (- (square guess) x)) tolerance))
                      (lambda (guess)
                        (average guess (/ x guess)))) guess))

(define (fixed-point-iter f guess)
  (define tolerance 0.0001)
  ((iterative-improve (lambda (guess)
                        (let ((next (f guess)))
                          (< (abs (- guess next)) tolerance)))
                      f) guess))


