;exercise 2.53
;(list 'a 'b 'c)                         => (a b c)
;(list (list 'george))                   => ((george))
;(cdr '((x1 x2) (y1 y2)))                => ((x1 x2))
;(cadr '((x1 x2) (y1 y2)))               => (y1 y2)
;(pair? (car '(a short list)))           => #f
;(memq 'red '((red shoes) (blue shoes))) => #f
;(memq 'red '(red shoes blue socks))     => (red shoes blue socks)

;exercise 2.54
(define (equall? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ((and (null? l1) (not (null? l2))) #f)
        ((and (null? l2) (not (null? l1))) #f)
        ((and (not (pair? l1)) (not (pair? l2))) (eq? l1 l2))
        (else (let ((item1 (car l1))
                    (item2 (car l2))
                    (rest1 (cdr l1))
                    (rest2 (cdr l2)))
                (cond ((and (pair? item1) (not (pair? item2))) #f)
                      ((and (pair? item2) (not (pair? item1))) #f)
                      ((pair? item1) (and (equall? item1 item2) (equall? rest1 rest2)))
                      (else (and (eq? item1 item2) (equall? rest1 rest2))))))))

;exercise 2.55
;'foo is same as (quote foo)
;''abc is same as (quote (quote abc))

;exercise 2.56
(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr)
         (if (same-variable? expr var) 1 0))
        ((sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var)))
        ((product? expr)
         (make-sum
           (make-product (multiplier expr)
                         (deriv (multilicand expr) var))
           (make-product (deriv (multiplier expr) var)
                         (multilicand expr))))
        ((exponentiation? expr)
         (make-product
           (make-product (exponent expr)
                         (make-exponentiation (base expr) (- (exponent expr) 1)))
           (deriv (base expr) var)))
        (else
          (error "unknown expression"))))

(define (variable? expr) (symbol? expr))
(define (sum? expr) (and (pair? expr) (eq? (car expr) '+)))
(define (addend expr) (cadr expr))
(define (augend expr) (caddr expr))
(define (product? expr) (and (pair? expr) (eq? (car expr) '*)))
(define (multiplier expr) (cadr expr))
(define (multilicand expr) (caddr expr))
(define (exponentiation? expr) (and (pair? expr) (eq? (car expr) '**)))
(define (make-sum a b)
  (cond ((and (number? a) (= a 0)) b)
        ((and (number? b) (= b 0)) a)
        ((and (number? a) (number? b)) (+ a b))
        (else (list '+ a b))))
(define (make-product a b)
  (cond ((and (number? a) (= a 0)) 0)
        ((and (number? b) (= b 0)) 0)
        ((and (number? a) (= a 1)) b)
        ((and (number? b) (= b 1)) a)
        ((and (number? a) (number? b)) (* a b))
        (else (list '* a b))))
(define (make-exponentiation b e)
  (cond ((= e 0) 1)
        ((= e 1) b)
        (else (list '** b e))))
(define (base expr) (cadr expr))
(define (exponent expr) (caddr expr))
(define (same-variable? a b) (and (variable? a) (variable? b) (eq? a b)))

;exercise 2.57
(define (augend expr)
  (if (null? (cdddr expr))
    (caddr expr)
    (cons '+ (cddr expr))))

(define (multilicand expr)
  (if (null? (cdddr expr))
    (caddr expr)
    (cons '* (cddr expr))))

;exercise 2.58
;a)
(define (sum? expr) (and (pair? expr) (eq? '+ (cadr expr))))
(define (addend expr) (car expr))
(define (augend expr) (caddr expr))
(define (make-sum a b)
  (cond ((and (number? a) (= a 0)) b)
        ((and (number? b) (= b 0)) a)
        ((and (number? a) (number? b)) (+ a b))
        (else (list a '+ b))))

(define (product? expr) (and (pair? expr) (eq? '* (cadr expr))))
(define (multiplier expr) (car expr))
(define (multilicand expr) (caddr expr))
(define (make-product a b)
  (cond ((and (number? a) (= a 0)) 0)
        ((and (number? b) (= b 0)) 0)
        ((and (number? a) (= a 1)) b)
        ((and (number? b) (= b 1)) a)
        ((and (number? a) (number? b)) (* a b))
        (else (list a '* b))))

;b)
(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr)
         (if (same-variable? expr var) 1 0))
        ((sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var)))
        ;是乘法的时候，判断乘法后面是不是加法，如果是的话就(make-sum 左边的导数 右边的导数)
        ((product? expr)
         (cond ((and (pair? (cdddr expr)) (eq? '+ (cadddr expr)))
                 (make-sum (deriv (sublist expr 0 3) var)
                           (deriv (cddddr expr) var)))
                (else
                 (make-sum
                   (make-product (multiplier expr)
                                 (deriv (multilicand expr) var))
                   (make-product (deriv (multiplier expr) var)
                                 (multilicand expr))))))
        ((exponentiation? expr)
         (make-product
           (make-product (exponent expr)
                         (make-exponentiation (base expr) (- (exponent expr) 1)))
           (deriv (base expr) var)))
        (else
          (error "unknown expression"))))

;exercise 2.59
(define (element-of-set? item set)
  (cond ((null? set) #f)
        ((eq? (car set) item) #t)
        (else (element-of-set? item (cdr set)))))
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

;exercise 2.60
;element-of-set intersection-set 不变
;union-set 和 adjoin-set 直接append两个set

;exercise 2.61
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

;exercise 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x (car set1))
                (y (car set2)))
            (cond ((= x y) (union-set (cdr set1) set2))
                  ((< x y) (cons x (union-set (cdr set1) set2)))
                  (else (cons y (union-set set1 (cdr set2)))))))))

;exercise 2.63
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (element-of-tree? x tree)
  (cond ((null? tree) #f)
        ((= x (entry tree)) #t)
        ((< x (entry tree)) (element-of-tree? (left-branch tree)))
        ((> x (entry tree)) (element-of-tree? (right-branch tree)))
        (else (error "error: wrong argument!"))))

(define (adjoin-tree x tree)
  (cond ((null? tree) (make-tree x nil nil))
        ((= x (entry tree)) tree)
        ((< x (entry tree)) (make-tree (entry tree)
                                       (adjoin-tree x (left-branch tree))
                                       (right-branch tree)))
        ((> x (entry tree)) (make-tree (entry-tree)
                                       (left-branch tree)
                                       (adjoin-tree x (right-branch tree))))
        (else (error "error: wrong argument!"))))

(define (tree-to-list1 tree)
  (if (null? tree)
    '()
    (append (tree-to-list1 (left-branch tree))
            (cons (entry tree) (tree-to-list1 (right-branch tree))))))

(define (tree-to-list2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))

(define tree1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define tree2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define tree3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))
;a)
;一样的，都是中序
;b)由于append本身的复杂度是O(n), 所以tree-to-lis1的复杂度更高一些O(nlogn), tree-to-list2的复杂度为O(n)

;exercise 2.64
(define (list-to-tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= 0 n)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))

;a) partial-tree elts n: 把序列etls的左边n个元素转换成二叉树，然后拼接上右边的元素
;显然: 当n == length(elts)时, partial-tree把整个序列转换成二叉树

;b) partial-tree的时间复杂度是O(n)的，因为每个元素都会遍历一次

;exercise 2.65
;1st: tree-to-list2 把树转换成有序表
;2nd: union-set-sorted intersection-set-sorted 有序表的操作
;3rd: list-to-tree 有序表转换成平衡树

;exercise 2.66
(define (lookup key key-tree)
  (cond ((null? key-tree) #f)
        (else
          (let ((root (entry key-tree)))
            (cond ((< key root) (lookup (left-branch key-tree)))
                  ((> key root) (lookup (right-branch key-tree))))))))


