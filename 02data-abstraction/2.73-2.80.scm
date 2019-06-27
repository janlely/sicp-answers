;exercise 2.73
(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr) (if (same-variable? expr var) 1 0))
        (else ((get 'deriv (operator expr)) (operands expr) var))))

;a)
;我们的目标是根据操作类型来选择求导的过程, 对于number和variable来说operands expr的结果将是nil, 把nil作为参数应用在求导过程上没意义

;b)
(define (operator expr) (car expr))
(define (operands expr) (cdr expr))
(define install-add-package
  (define (make-sum a1 a2)
    (list '+ a1 a2))
  (define (addend s) (caddr s))
  (define (augend s) (cadr s))
  (define (deriv-rule expr var)
    (make-sum (deriv (addend expr) var)
              (deriv (augend expr) var)))
  (put 'deriv '+ deriv-rule))

(define install-product-package
  (define (make-product m1 m2)
    (list '* m1 m2))
  (define (make-sum a1 a2)
    (list '+ a1 a2))
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (car p))
  (define (deriv-rule expr var)
    (make-sum
      (make-product (multiplier exp)
                    (deriv (multiplicand exp) var))
      (make-product (multiplicand exp)
                    (deriv (multiplier exp) var))))
  (put 'deriv '* deriv-rule))

;c)
(define install-exponent-package
  (define (exponent expr) (cadr expr))
  (define (base expr) (car expr))
  (define (deriv-rule expr var)
    (make-product
      (make-product (exponent expr)
                    (make-exponentiation (base expr) (- (exponent expr) 1)))
      (deriv (base expr) var)))
  (put 'deriv '** deriv-rule))

;d)
;把put后面的参数的顺序改变一下

;exercise 2.74
;a)
(define (get-record employee branch)
  ((get 'get-record branch) employee))

;b)
(define (get-salary employee branch)
  ((get 'get-salary branch) employee))

;c)
(define (find-employee-record employee branchs)
  (if (null? branchs)
    '()
    (cons ((get 'find-employee-record (car branchs)) employee)
          (find-employee-record employee (cdr branchs)))))
;d)
;1: 定义各种操作，如：get-record get-salary等
;2: 把操作put到表中

;exercise 2.75
(define (make-from-mag-ang r a)
  (define (real-part z) (* r (cos a)))
  (define (imag-part z) (* r (sin a)))
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unkonw op"))))
  dispatch)

;exercise 2.76
;显示分派:
    ;1，实现新对象类型下的操作过程
    ;2，修改通用过程，添加新的类型判断逻辑
;数据导向:
    ;1，实现新对象类型下的操作过程
    ;2，把新的对象类型的操作put到操作表中
;消息传递:
    ;系统不需要做更改

;消息传递风格:
;数据对象包含了操作，和面向对象的思想一样
;real-part, imag-part等相当于接口;
;apply-generic op arg 相当于arg.op()

;exercise 2.77
;(magnitude z) 对应的是 (apply-generic 'magnitude z) 显然类型不匹配complex 和 magnitude
;执行put之后把magnitude添加到表格中，前面有两次put magnitude，都是put的内部的函数，这一次put是外部函数



;exercise 2.78
(define (attach-tag type-tag contents)
  (if (number? contents)
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
         ((pair? datum) (car datum))
         (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
          ((pair? datum) (cdr datum))
          (else (error "Bad tagged datum -- CONTENT" datum))))

;exercise 2.79
(define install-scheme-number-package
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (eq? x y))))
(define install-rational-package
  (put 'equ? '(rational rational)
       (lambda (x y) (and (eq? (numer x) (numer y)) (eq? (denom x) (denom y))))))
(define install-complex-package
  (define tolarent 0.0001)
  (define (almost-eq? x y) (< (abs (- x y)) tolarent)
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (and (almost-eq? (real-part z1) (real-part z2)) (almost-eq? (imag-part z1) (imag-part z2))))))


;exercise 2.80
(define install-scheme-number-package
  (put '=zero? '(scheme-bumber)
       (lambda (x) (= 0 x))))

(define install-rational-package
  (put '=zero? '(ratinal)
       (lambda (x) (and (= 0 (numer x)) (= 0 (denom x))))))
(define install-complex-package
  (put '=zero? '(complex)
       (lambda (x) (and (= 0 (real-part x)) (= 0 (imag-part x))))))


