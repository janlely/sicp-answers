;exercies 2.37
(define nil '())
(define (accmulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accmulate op initial (cdr sequence)))))

(define (accmulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accmulate op init (map car seqs))
          (accmulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accmulate (lambda (x y) (+ x y))
             0
             (map (lambda (a b) (* a b)) v w)))

(define (matrix-mul-vector m v)
  (map (lambda (x) (dot-product x v)) m))


(define (transpose mat)
  (accmulate-n cons nil mat))

(define (matrix-mul-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (map (lambda (col) (dot-product row col)) cols)) m)))

;exercise 2.38
;同时满足交换侓和结合侓

;exercise 2.39
(define (reverse-r sequence)
  (fold-right (lambda (x y) (append y (cons x nil)))
              nil
              sequence))

(define (reverse-l sequence)
  (fold-left (lambda (x y) (cons y x))
             nil
             sequence))

;permutation
(define (flatmap proc seq)
  (accmulate append nil (map proc seq)))
(define (permutation s)
  (if (null? s)
    (list nil)
    (flatmap (lambda (x) (map (lambda (p) (cons x p))
                              (permutation (remove (lambda (k) (= k x)) s))))
             s)))

;exercise 2.40
(define (enumerate-interval i j)
  (if (> i j)
    nil
    (append (enumerate-interval i (- j 1)) (list j))))
(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (cons j i))
                            (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;exercise 2.41
(define (diff-sum-same n s)
  (define (triple-sum t)
    (+ (car t) (cadr t) (caddr t)))
  (filter (lambda (triple) (= s (triple-sum triple)))
          (flatmap (lambda (k) (flatmap (lambda (j) (map (lambda (i) (list i j k))
                                                         (enumerate-interval 1 (- j 1))))
                                        (enumerate-interval 1 (- k 1))))
                   (enumerate-interval 1 n))))

;(flatmap (lambda (k) (flatmap (lambda (j) (map (lambda (i) (list i j k))
                                               ;(enumerate-interval 1 (- j 1))))
                              ;(enumerate-interval 1 (- k 1))))
         ;(enumerate-interval 1 6))

;(flatmap (lambda (j) (map (lambda (i) (cons (i j)))
                      ;(enumerate-interval 1 (- j 1))))
     ;(enumerate-interval 1 n))

;exercise 2.42
(define (n-queens size)
  (define (safe? i k l)
    (null? (filter (lambda (x)
                     (if (null? x)
                       #f
                       (let ((row (car x))
                             (col (cdr x)))
                         (or (= row i)
                             (and (= (- k col) (- row i)))
                             (and (= (- k col) (- i row))))))) l)))
  (define (k-queens k)
    (if (= 0 k)
      (list nil)
      (let ((last (k-queens (- k 1))))
        (flatmap (lambda (i) (map (lambda (v) (cons (cons i k) v))
                                  (filter (lambda (l) (safe? i k l)) last)))
                 (enumerate-interval 1 size)))))
  (for-each (lambda (x)
              (display x)
              (newline))
            (k-queens size)))

;exercise 2.43
;递归调用太多了，对每一个new-row都要调用一次递归

;exercise 2.44
(define (up-splite painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below (beside smaller smaller) painter))))

;exercise 2.45
(define (split s1 s2)
  (lambda (painter n)
    (if (= n 0)
      painter
      (let ((smaller ((split s1 s2) painter (- n 1))))
        (s1 painter (s2 smaller smaller))))))

;exercise 2.46
(define (make-vect x y)
  (cons x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;exercise 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

;exercise 2.48
(define (make-segment v1 v2)
  (cons v1 v2))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

;exercise 2.49
;a)
(define (border-painter frame)
  (let ((origin (origin-frame frame))
        (vx (edge1-frame frame))
        (vy (edge2-frame frame))
        (vxy (add-vect vx vy)))
    (segments-painter (list (make-segment origin (add-vect vx origin))
                            (make-segment origin (add-vect vy origin))
                            (make-segment (add-vect vx origin) (add-vect vxy origin))
                            (make-segment (add-vect vy origin) (add-vect vxy origin)))))

;b)
(define (cross-line-painter frame)
  (let ((a (origin-frame frame))
        (b (edge1-frame frame))
        (d (edge2-frame frame))
        (c (add-vector b d)))
    (segment-painter (list (make-segment a (add-vect a c))
                           (make-segment (add-vect a b) (add-vect a d))))))
;c)
(define (diamond-painter frame)
  (let ((a (origin-frame frame))
        (b (edge1-frame frame))
        (d (edge2-frame frame))
        (c (add-vect b d)))
    (segments-painter (list (make-segment (add-vect a (scale-vect 0.5 b))
                                          (add-vect a (scale-vect 0.5 d)))
                            (make-segment (add-vect a (scale-vect 0.5 (add-vect b c)))
                                          (add-vect a (scale-vect 0.5 (add-vect d c))))
                            (make-segment (add-vect a (scale-vect 0.5 b))
                                          (add-vect a (scale-vect 0.5 (add-vect b c))))
                            (make-segment (add-vect a (scale-vect 0.5 d))
                                          (add-vect a (scale-vect 0.5 (add-vect d c))))))))
;d)
;ommited

;exercise 2.50
(define (flip-horize painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;exercise 2.51
(define (below painter1 painter2)
  (let ((paint-up (transform-painter painter1
                                     (make-vect 0.0 0.5)
                                     (make-vect 1.0 0.5)
                                     (make-vect 1.0 0.0)))
        (paint-down (transform-painter painter2
                                       (make-vect 0.0 0.0)
                                       (make-vect 1.0 0.5)
                                       (make-vect 0.0 0.5))))
    (lambda (frame)
      (paint-up frame)
      (paint-down frame))))

(define (below painter1 painter2)
  (rotate270 (beside painter1 painter2)))

;exercise 2.52
;omited


