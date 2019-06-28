;2D table
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key1 key2)
      (let ((subtable (assoc key1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key2 (cdr subtable))))
            (if record
              (cdr record)
              false))
          false)))
    (define (insert! key1 key2 value)
      (let ((subtable (assoc key1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key2 value)
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key1
                                (cons key2 value))
                          (cdr local-table))))
        'ok))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc))

(define (attach-tag type-tag contents)
  (if (exact-integer? contents)
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  ;(display datum)
  ;(newline)
  (cond ((exact-integer? datum) 'scheme-number)
         ((pair? datum) (car datum))
         (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  ;(display datum)
  ;(newline)
  (cond ((exact-integer? datum) datum)
          ((pair? datum) (cdr datum))
          (else (error "Bad tagged datum -- CONTENT" datum))))

;complex packge
(define (install-rectangule-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag
       'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  'done)

(define (install-polar-package)
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-mag-ang x y) (cons x y))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-mag-ang
       'polar
       (lambda (x y) (tag (make-from-mag-ang x y))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error "No method for these types -- APPLY-GENERIC" (list op type-tags))))))

(install-rectangule-package)
(install-polar-package)
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang x y)
  ((get 'make-from-mag-ang 'polar) x y))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;common arithmetic
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (exp x y) (apply-generic 'exp x y))
(define (equ? x y) (apply-generic 'equ? x y))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)
(install-scheme-number-package)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-real-number-package)
  (define (tag x) (attach-tag 'real-number x))
  (put 'add '(real-number real-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real-number real-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real-number real-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real-number real-number)
       (lambda (x y) (tag (/ x y))))
  (put 'exp '(real-number real-number)
       (lambda (x y) (tag (expt x y))))
  (put 'equ? '(real-number real-number)
       (lambda (x y) (= x y)))
  (put 'make 'real-number
       (lambda (x) (tag x)))
  'done)
(install-real-number-package)
(define (make-real-number n)
  ((get 'make 'real-number) n))


(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons(/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom y) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (numer y) (denom x))))
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (and (eq? (numer x) (numer y))
                          (eq? (denom x) (denom y)))))
  (put 'denom '(rational) denom)
  (put 'numer '(rational) numer)
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(install-rational-package)
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex)
       (lambda (x y) (or (and (= (imag-part x) (imag-part y))
                              (= (real-part x) (real-part y)))
                         (and (= (magnitude x) (magnitude y))
                              (= (angle x) (angle y))))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (x y) (tag (make-from-mag-ang x y)))))

(install-complex-package)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang x y)
  ((get 'make-from-mag-ang 'complex) x y))


(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)



(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (let ((t1->t2 (get-coercion type1 type2))
                  (t2->t1 (get-coercion type2 type1)))
              (cond (t1->t2
                      (apply-generic op (t1->t2 a1) a2))
                    (t2->t1
                      (apply-generic op a1 (t2->t1 a2)))
                    (else (error "No method for these types"
                          (list op type-tags))))))
          (error "No method for these types"
                 (list op type-tags)))))))


;exercise 2.81
;a)
;当找不到对应运算的实现，并且两个参数的类型相同时会试图做强制。但此时t1->t2和t2->t1一定是false，因此最终的逻辑会走到error
;b)
;如果安装了同类型的强制，会出现死循环
;c)
;没有，不太明白为什么要纠正同类型的强制，同类型的强制时t1->t2和t2->t1都会返回false，这样本身就是合理的。

;exercise 2.82
;多参数运算如果找不到实现，强制的时候也不能都强制成统一的类型，显而易见的, (type1 type2 type3)对应的实现找不到，还有可能是(type2 type2 type3)或者其他的

;exercise 2.83
(define (install-raise-package)
  (define (denom x) (apply-generic 'denom x))
  (define (numer x) (apply-generic 'numer x))
  (put 'raise 'scheme-number
       (lambda (x) (make-rational x 1)))
  (put 'raise 'rational
       (lambda (x) (make-real-number (/ (+ (car x) 0.0) (cdr x)))))
  (put 'raise 'real-number
       (lambda (x) (make-complex-from-real-imag x 0)))
  'done)

(install-raise-package)
(define (raise x) ((get 'raise (type-tag x)) (contents x)))

;exercise 2.84
(define (higher? t1 t2)
  (define (type-int t)
    (cond ((eq? t 'complex) 1)
          ((eq? t 'real-number) 2)
          ((eq? t 'rational) 3)
          ((eq? t 'scheme-number) 4)))
  (< (type-int t1) (type-int t2)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (cond ((higher? type1 type2)
                     (apply-generic op a1 (raise a2)))
                  ((higher? type2 type1)
                     (apply-generic op (raise a1) a2))
                  (else (error "No method for these types" (list op type-tags)))))
          (error "No method for these types"
                 (list op type-tags)))))))

;exercise 2.85
(define (install-project-package)
  (put 'project 'complex
       (lambda (x) (make-real-number (real-part x))))
  (put 'project 'real-number
       (lambda (x) (round->exact x)))
  (put 'project 'rational
       (lambda (x) (round->exact (/ (car x) (cdr x))))))
(install-project-package)

(define (project x)
  (if (eq? 'scheme-number (type-tag x))
    #f
    ((get 'project (type-tag x)) (contents x))))

(define (drop x1)
  (let ((pro (project x1)))
    (if pro
      (let ((x2 (raise pro)))
        (if (equ? x1 x2)
          (drop pro)
          x1))
      x1)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        ;(if (eq? op 'equ?)
        (apply proc (map contents args))
          ;(drop (apply proc (map contents args))))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (cond ((higher? type1 type2)
                     (apply-generic op a1 (raise a2)))
                  ((higher? type2 type1)
                     (apply-generic op (raise a1) a2))
                  (else (error "No method for these types" (list op type-tags)))))
          (error "No method for these types"
                 (list op type-tags)))))))

;exercise 2.86
;把复数包里面的+ - * /操作替换成能用的add sub mul div
