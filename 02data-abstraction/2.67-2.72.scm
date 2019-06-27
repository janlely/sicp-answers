(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? node)
  (eq? (car node) 'leaf))
(define (symbol-leaf node) (cadr node))
(define (weight-leaf node) (caddr node))
(define (make-code-tree left right)
  (list right
        left
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (decode bits tree)
  (define (decode-l bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-l (cdr bits) tree))
          (decode-l (cdr bits) next-branch)))))
  (decode-l bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit!"))))

(define (adjoin-set-sorted x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
               (adjoin-set-sorted x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set-sorted (make-leaf (car pair)
                                    (cadr pair))
                         (make-leaf-set (cdr pairs))))))
;exercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'C 1)
                                                  (make-leaf 'D 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;exercise 2.68
(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (scan symbol tree result)
    (cond ((and (leaf? tree) (eq? (symbol-leaf tree) symbol)) result)
          ((and (leaf? tree) (not (eq? (symbol-leaf tree) symbol))) '())
          (else
            (let ((left-result (scan symbol (left-branch tree) (cons 0 result))))
              (if (not (null? left-result))
                   left-result
                   (let ((right-result (scan symbol (right-branch tree) (cons 1 result))))
                     (if (not (null? right-result))
                       right-result
                       (error "invalid symbol!"))))))))
  (reverse (scan symbol tree '())))

;exercise 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge leafs)
  (if (null? (cdr leafs))
    leafs
    (let ((left (car leafs))
          (right (cadr leafs)))
      (successive-merge (cons (make-code-tree left right)
            (cddr leafs))))))
(define sample-pairs '((A 4) (B 2) (C 1) (D 1)))

;exercise 2.70
(define words '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))
(define word-tree (generate-huffman-tree words))
(define encoded-song (encode '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM) word-tree))

;exercise 2.71
depth of the huffman tree is: n - 1, 可以知道树的深度是n-1
most Frequently: 1 bit
least Frequently: n -1 bits

;exercise 2.72
most Frequently steps: 1次，最频繁的在最上面，一次就找到了
least Frequently steps: n, 最不频繁的在最下面，需要遍历整个树
