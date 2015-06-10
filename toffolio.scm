;;  toffolio.scm
;;  jpt4
;;  UTC20150610

(define xor
  (lambda (p q)
    (not (and (not (and (not q) p)) (not (and (not p) q))))))

(define toffoli
  (lambda (a b c)
    `(,a ,b ,(xor c (and a b)))))

(define (ando a b c)
  (conde
    [(== a b) (== #t c)]
    [(=/= a b) (== #f c)]))

(define (and?o a b)
  (conde
    [(== a b) succeed]))    

(define (xoro a b c)
  (conde
    [(== a b) (== #f c)]
    [(=/= a b) (== #t c)]))

(define xor?o
  (lambda (a b)
    (conde
      [(=/= a b) succeed])))

(define (toffolio a b c d e f)
  (fresh (ra rx)
    (== a d) (== b e) 
    (ando a b ra) (xoro c ra rx)
    (== rx f)))
     
