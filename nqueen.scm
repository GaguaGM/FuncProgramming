(define (nqueen n)
  (solution n (do ((i n (- i 1)) (ls '() (cons `(,i . X) ls ))) ((= i 0) ls))))

(define (solution n start)
  (if (null? start)
    '(())
    (do ((sl (solution n (cdr start)) (cdr sl))
         (ls '() (do ((i n (- i 1))
                      (ls ls
                          (if (cantkill `(,(caar start) . ,i) (car sl))
                            (cons (cons `(,(caar start) . ,i) (car sl)) ls)
                            ls)))
                     ((= i 0) ls))))
        ((null? sl) ls))(newline)))
        

(define (cantkill a b)
  (let ((x (car a)) (y (cdr a)))
    (if (null? b)
      #t
      (let ((x1 (caar b)) (y1 (cdar b)))
        (cond
          ((= y y1) #f)
          ((= (- y1 y) (- x1 x)) #f)
          ((= (- y1 y) (- x x1)) #f)
          (else (cantkill a (cdr b))))))))

(nqueen 8)
