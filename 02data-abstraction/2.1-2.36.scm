;exercise 2.1
(define (make-rat n d)
  (cond ((and (positive? n) (positive? d))
         (let ((g (gcd n d)))
           (cons (/ n g) (/ d g))))
        ((and (negative? n) (negative? d))
         (let ((g (gcd (* -1 n) (* -1 d))))
           (cons (/ n g -1) (/ d g -1))))
        ((and (positive? n) (negative? d))
         (let ((g (gcd n (* -1 d))))
           (cons (/ n g -1) (/ d g -1))))
        (else
         (let ((g (gcd n (* -1 d))))
          (cons (/ n g) (/ d g))))))

;exercise 2.2
(define make-point cons)
(define x-point car)
(define y-point cdr)

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment seg)
  (define (average x y)
    (/ (+ x y) 2))
  (make-point (average (x-point (start-segment seg))
                       (x-point (end-segment seg)))
              (average (y-point (start-segment seg))
                       (y-point (end-segment seg)))))

;exercise 2.3
(define (perimiter rect)
  (* 2 (+ (rect-height rect)
          (rect-width rect))))
(define (area rect)
  (* (rect-height rect)
     (rect-width rect)))

;first representation
(define (make-rectangle left-segment bottom-segment)
  (cons left-segment bottom-segment))

(define (make-rectangle bottom-left top-right)
  (cons bottom-left top-right))


;exercise 2.4
(define (my-cons x y)
  (lambda (m) (m x y)))

(define (my-car z)
  (z (lambda (p q) p)))

(define (my-cdr z)
  (z (lambda (p q) q)))

;exercise 2.5
(define (my-cons2 x y)
  (* (expt 2 x) (expt 3 y)))
(define (log-reduce n base)
  (if (not (zero? (remainder n base)))
    0
    (+ (log-reduce (/ n base) base) 1)))
(define (my-car2 n)
  (log-reduce n 2))
(define (my-cdr2 n)
  (log-reduce n 3))

;exercise 2.6
(define one (lambda f (lambda (x) (f x))))
(define two (lambda f (lambda (x) (f (f x)))))
(define (add m n)
  (lambda f (lambda (x) ((m f) ((n f) x)))))

;exercise 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

;exercise 2.8
(define (sub-interval a b)
  (cons (- (car a) (cdr b)) (- (cdr a) (car b))))

;exercise 2.9
;+: A:[x, y], B:[z, w], widthA: (y - x) / 2, widthB: (w - z) / 2, A+B: [x + z, y + w], witdhA+B: (y + w - x - z) / 2, widthA+B = widthA + widthB
;-: A:[x, y], B:[z, w], widthA: (y - x) / 2, widthB: (w - z) / 2, A-B: [x - w, y - z], witdhA-B: (y - z - x + w) / 2, widthA-B = widthA + widthB

;exercise 2.10
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (upper-bound y)))
        (p4 (* (upper-bound x) (lower-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (define (spans-zero? k)
    (and (< (car k) 0) (> (cdr k) 0)))
  (if (spans-zero? y)
    (error "div-interval cannot divide by an interval that spans 0")
    (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;exercise 2.11
(define (mul-interval x y)
  (define (pos-pos? k)
    (and (> (car k) 0) (> (cdr k) 0)))
  (define (neg-neg? k)
    (and (< (car k) 0) (< (cdr k) 0)))
  (define (neg-pos? k)
    (and (< (car k) 0) (> (cdr k) 0)))
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond ((and (pos-pos? x) (pos-pos? y)) (make-interval (* lx ly) (* ux uy)))
          ((and (neg-neg? x) (neg-neg? y)) (make-interval (* lx ly) (* ux uy)))
          ((and (pos-pos? x) (neg-neg? y)) (make-interval (* ux ly) (* lx uy)))
          ((and (pos-pos? x) (neg-pos? y)) (make-interval (* ux ly) (* ux uy)))
          ((and (neg-neg? x) (pos-pos? y)) (make-interval (* lx uy) (* yx ly)))
          ((and (neg-neg? x) (neg-pos? y)) (make-interval (* lx uy) (* lx ly)))
          ((and (neg-pos? x) (pos-pos? y)) (make-interval (* ux uy) (* lx uy)))
          ((and (neg-pos? x) (neg-pos? y)) (make-interval (min (* lx uy) (* ux ly)) (max (* ux uy) (* lx ly))))
          ((and (neg-pos? x) (neg-neg? y)) (make-interval (* ux ly) (* lx ly))))))

;exercise 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(define (make-interval-percent c p)
  (make-center-width c (* c p)))
(define (percent i)
  (/ (width i) (center i)))

;exercise 2.13
;区间1：C1, P1, [C1 - C1P1, C1 + C1P1]
;区间2：C2, P2, [C2 - C2P2, C2 + C2P2]
;相乘：[C1C2 - C1C2P2 - C1C2P1 + C1C2P1P2, C1C2 + C1C2P1 + C1C2P2 + C1C2P1P1]
;当P1，P2很小时，P1P2接近于零, 因此相乘之后的区间的百分数误差约等于P1 + P2

;exercise 2.14
(define (add-interval i1 i2)
  (make-interval (+ (lower-bound i1) (lower-bound i2))
                 (+ (upper-bound i1) (upper-bound i2))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;exercise 2.15
;可以证明par2是正确的，当R1和R2都取最小值时，并联之后的阻值最小，反之阻值最大。
;而par1的结果是错的, R1R2 / (R1+R2),以R1[1,2], R2[3,4]为例, 最终的结果是[2, 8]/[4,6],最小值是分子取3，分母取6，
;最大值是分子取8分母取4，2是R1,R2的最小值之积，6是R1,R2的最大值之和，显然R1，R2不可能同时取到最大值和最小值，因此这个结果是不准确的。

;exercise 2.16
;区间运算问题，有兴趣可以研究一下


(define nil '())
;exercise 2.17
(define (last-pair l)
  (if (null? (cdr (cdr l)))
    (cdr l)
    (last-pair (cdr l))))

;exercise 2.18
(define (reverse l)
  (define (iter ll res)
    (if (null? ll)
      res
      (iter (cdr ll) (cons (car ll) res))))
  (iter l nil))

;exercise 2.19
(define (cc amount coins-list)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (null? coins-list)) 0)
        (else
          (+ (cc amount (cdr coins-list))
             (cc (- amount (car coins-list)) coins-list)))))

;exercise 2.20
(define (same-parity x . y)
  (if (odd? x)
    (cons x (filter odd? y))
    (cons x (filter even? y))))


;exercise 2.21
;(define (square-list items)
  ;(if (null? items)
    ;nil
    ;(cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;exercise 2.22
;第一种方式显然顺序正好相反：
;以(1 2 3)为例，answer每一次迭代的值为: nil => (1) => (4 1) => (9 4 1)
;第二种方式顺序是正确的，但结果却不是一个列表
;以(1 2 3)为例，answer每一次迭代的值为: nil => (nil 1) => ((nil 1) 4) => ((nil 1) 4) 9)

;exercise 2.23
(define (my-for-each proc items)
  (if (null? items)
    #t
    (begin (proc (car items))
           (my-for-each proc (cdr items)))))

;exercise 2.24
;(1 (2 (3 4)))

;exercise 2.25
;omitted

;exercise 2.26
;append (1 2 3 4 5 6)
;cons ((1 2 3) 4 5 6)
;list ((1 2 3) (4 5 6))

;exercise 2.27
(define (deep-reverse l)
  (define (iter ll res)
    (if (null? ll)
      res
      (let ((head (car ll)))
        (if (not (pair? head))
          (iter (cdr ll) (cons head res))
          (iter (cdr ll) (cons (deep-reverse head) res))))))
  (iter l nil))

;exercise 2.28
(define (fringe t)
  ;(define (concat l1 l2)
    ;(if (null? l1)
      ;l2
      ;(cons (car l1) (concat (cdr l1) l2))))
  (cond ((null? t) nil)
        ((not (pair? t)) (cons t nil))
        (else (append (fringe (car t)) (fringe (cdr t))))))

;exercise 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch len struct)
  (list len struct))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))

(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (left-branch mobile))
                 (total-weight (right-branch mobile))))))
;exercise 2.30
(define (square-tree tr)
  (map (lambda (subtree)
         (if (pair? subtree)
           (square-tree subtree)
           (square subtree)))
       tr))


;exercise 2.31
(define (tree-map f tr)
  (map (lambda (subtree)
         (if (pair? subtree)
           (tree-map f subtree)
           (f subtree))) tr))

;exercise 2.32
(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x)
                          (cons (car s) x))
                        rest)))))

;exercise 2.33
(define (accmulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accmulate op initial (cdr sequence)))))
(define (map p sequence)
  (accmulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accmulate cons seq2 seq1))

(define (length sequence)
  (accmulate (lambda (x y) (+ 1 y)) 0 sequence))

;exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accmulate (lambda (this-coeff higher-terms)
               (+ this-coeff (* higher-terms x)))
             0
             coefficient-sequence))

;exercise 2.35
(define (count-leaves t)
  (accmulate (lambda (x y) (+ x y)) 0 (map (lambda (x) 1) (fringe t))))

;exercise 2.36
(define (accmulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accmulate op init (map car seqs))
          (accmulate-n op init (map cdr seqs)))))


