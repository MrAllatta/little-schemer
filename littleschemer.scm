#lang scheme/base
(require schemeunit)

(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

(define add1
  (lambda (x)
    (+ x 1)))

(define sub1
  (lambda (x)
    (- x 1)))

; a lat is a list of atoms
; define a function lat? which use some
; but not necessarily all of
; car cdr cons null? atom? eq?
; lat? : List -> Boolean
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((equal? a (car lat)) (cdr lat))
      (else (cons (car lat)
                  (rember a
                          (cdr lat)))))))

(define firsts
  (lambda (lol)
    (cond
      ((null? lol) '())
      (else
       (cons (car (car lol)) (firsts (cdr lol)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((eq? (car lat) old) (cons (car lat) (cons new (cdr lat))))
         (else (cons (car lat) (insertR new old (cdr lat)))))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((eq? (car lat) old) (cons new lat))
         (else (cons (car lat) (insertL new old (cdr lat)))))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((eq? (car lat) old) (cons new (cdr lat)))
         (else                (cons (car lat) (subst new old (cdr lat)))))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((or (eq? (car lat) o1)
              (eq? (car lat) o2)) (cons new (cdr lat)))
         (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((eq? (car lat) a) (multirember a (cdr lat)))
         (else (cons (car lat) (multirember a (cdr lat)))))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((eq? (car lat) old)
          (cons (car lat)
                (cons new
                      (multiinsertR new old (cdr lat)))))
         (else
          (cons (car lat)
                (multiinsertR new old (cdr lat)))))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((eq? (car lat) old)
          (cons new
                (cons (car lat)
                      (multiinsertL new old
                                    (cdr lat)))))
         (else (cons (car lat)
                     (multiinsertL new old
                                   (cdr lat)))))))))

(define alt-my-+
  (lambda (a b)
    (cond
      ((zero? a) b)
      (else (alt-my-+ (sub1 a)
                      (add1 b))))))

(define my-+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (my-+ n (sub1 m)))))))

(define mysub
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (mysub n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (alt-my-+ (car tup)
                      (addtup (cdr tup)))))))

(define times
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (alt-my-+ n (times n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (my-+ (car tup1)
                        (car tup2))
                  (tup+ (cdr tup1)
                        (cdr tup2)))))))

(define gt
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (gt (sub1 n) (sub1 m))))))

(define lt
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (lt (sub1 n) (sub1 m))))))

(define is-eq?
  (lambda (n m)
    (cond
      ((gt n m) #f)
      ((lt n m) #f)
      (else #t))))

(define pwr
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (times n (pwr n (sub1 m)))))))

(define div
  (lambda (n m)
    (cond
      ((lt n m) 0)
      (else (add1 (div (mysub n m) m))))))

(define leng
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (leng (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((eq? 1 n) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat)
                  (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((number? (car lat)) (no-nums (cdr lat)))
         (else (cons (car lat) (no-nums (cdr lat)))))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((number? (car lat)) (cons (car lat)
                                    (all-nums (cdr lat))))
         (else (all-nums (cdr lat))))))))

(define eqan?
  (lambda (n m)
    (cond
      ((and (number? n)
            (number? m)) (is-eq? n m))
      ((or (number? n)
           (number? m)) #f)
      (else (eq? n m)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else
       (cond
         ((eqan? (car lat) a) (add1 (occur a (cdr lat))))
         (else (occur a (cdr lat))))))))

(define one?
  (lambda (n)
    (cond
      ((zero? (sub1 n)) #t)
      (else #f))))

(define alt-rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat)
                  (alt-rempick (sub1 n)
                               (cdr lat)))))))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eqan? (car l) a) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

; insertR* : New Old List -> List
(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eqan? (car l) old)
          (cons old
                (cons new
                      (insertR* new old (cdr l)))))
         (else (cons (car l)
                     (insertR* new old (cdr l))))))
      (else
       (cons (insertR* new old (car l))
             (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eqan? (car l) a) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else
       (my-+ (occur* a (car l))
             (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eqan? (car l) old) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eqan? (car l) old)
          (cons new
                (cons (car l)
                      (insertL* new old (cdr l)))))
         (else (cons (car l)
                     (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l))
                  (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (cond
         ((eqan? a (car l)) #t)
         (else (member* a (cdr l)))))
      (else (or (member* a (car l))
                (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (and (equal? (car l1) (car l2))
                 (eqlist? (cdr l1 (cdr l2))))))))
  
(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2))
       (eqan? s1 s2))
      ((or (atom? s1) (atom? s2))
       #f)
      (else (eqlist? s1 s2)))))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car aexp))
                 (numbered? (car (cdr (cdr aexp)))))))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) '+) (+ (value (car nexp))
                                    (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) '*) (* (value (car nexp))
                                    (value (car (cdr (cdr nexp))))))
      (else (pwr (value (car nexp))
                 (value (car (cdr (cdr nexp)))))))))

(check-equal? (value 13) 13)
(check-equal? (value '(1 + 3)) 4)
(check-equal? (value '(1 + (3 pwr 4))) 82)

#|
;; Tests ;;
(check-equal? (numbered? '(3 + (4 * 5))) #t)
(let ((n 2))
  (check-equal? (numbered? (cons n '(+ 3))) #t))
(check-equal? (numbered? '3) #t)
(check-equal? (numbered? '(sausage + 3)) #f)

(let ((l1 '(strawberry ice cream))
      (l2 '(srawberry cream ice)))
  (check-equal? (eqlist? l1 l2) #f))
(let ((l1 '(banana ((split))))
      (l2 '((banana) (split))))
  (check-equal? (eqlist? l1 l2) #f))
(let ((l1 '(beef ((sausage)) (and (soda))))
      (l2 '(beef ((salami)) (and (soda)))))
  (check-equal? (eqlist? l1 l2) #f))
(let ((l1 '(a b c))
      (l2 '(a b c)))
  (check-equal? (eqlist? l1 l2) #t))

(check-equal? (leftmost '(a b c)) 'a)
(check-equal? (leftmost '((a) b c)) 'a)
(check-equal? (leftmost '(((b)))) 'b)

(check-equal? (member* 'a '(b b ((b a)))) #t)

(check-equal? (insertL* 'b 'a '(a (c a) d b a)) '(b a (c b a) d b b a))
(check-equal? (subst* 'dog 'cat '(the (((cat))) in the hat)) '(the (((dog))) in the hat))
(check-equal? (subst* 'c 'a '(a (ab a) (((b dog cat))))) '(c (ab c) (((b dog cat)))))
(check-equal? (occur* 'a '(a b (a b) ((a a)) a)) 5)
(check-equal? (occur* 'dog '(cat dog (((((cat))))))) 1)
(check-equal? (insertR* 'a 'b '(c b (c d b) b c)) '(c b a (c d b a) b a c))
(check-equal? (insertR* 'roast 'chuck
                        '((how much (wood))
                               could
                               ((a (wood) chuck))
                               (((chuck)))
                               (if (a) ((wood chuck)))
                               could chuck wood))
              '((how much (wood))
                could
                ((a (wood) chuck roast))
                (((chuck roast)))
                (if (a) ((wood chuck roast)))
                could chuck roast wood))

(check-equal? (rember* 'cat '((dog cat) cat (dog cat cat))) '((dog) (dog)))
(check-equal? (rember* 'a '(a b a b a b a)) '(b b b))
(check-equal? (alt-rempick 1 '(a b c d)) '(b c d))
(check-equal? (alt-rempick 2 '(cat dog fish)) '(cat fish))
(check-equal? (one? 1) #t)
(check-equal? (one? 2) #f)
(check-equal? (one? (add1 0)) #t)
(check-equal? (one? (sub1 1)) #f)
(check-equal? (occur 'a '(a b c a)) 2)
(check-equal? (occur 'a '(bear dog cat)) 0)
(check-equal? (occur 'dog '(bear dog dog dog)) 3)
(check-equal? (eqan? 'a 5) #f)
(check-equal? (eqan? 5 5) #t)
(check-equal? (eqan? 'a 'a) #t)
(check-equal? (all-nums '(a 5 b 6 c 7)) '(5 6 7))
(check-equal? (all-nums '(77 77 dog cat 53)) '(77 77 53))
(check-equal? (no-nums '(5 a 6 b 7 c)) '(a b c))
(check-equal? (no-nums '(a 5 b 6 c)) '(a b c))
(check-equal? (rempick 3 '(a b c d e)) '(a b d e))
(check-equal? (rempick 2 '(remove this)) '(remove))
(check-equal? (pick 2 '(a b)) 'b)
(check-equal? (pick 4 '(a b c d e)) 'd)
(check-equal? (pick 3 '(cat and dog)) 'dog)
(check-equal? (leng '(a b c d e)) 5)
(check-equal? (leng '()) 0)
(check-equal? (div 8 2) 4)
(check-equal? (div 7 2) 3)
(check-equal? (is-eq? 0 0) #t)
(check-equal? (pwr 1 1) 1)
(check-equal? (pwr 2 3) 8)
(check-equal? (pwr 5 3) 125)

(check-equal? (is-eq? 3 3) #t)
(check-equal? (is-eq? 4 3) #f)

(check-equal? (lt 4 6) #t)
(check-equal? (lt 8 3) #f)
(check-equal? (lt 6 6) #f)

(check-equal? (tup+ '(3 4 5 6 7) '(8 7 6 5 4))
(check-equal? (gt 2 3) #f)
(check-equal? (gt 4 3) #t)
(check-equal? (gt 3 3) #f)


              '(11 11 11 11 11))
(check-equal? (tup+ '(2 3) '(4 6))
              '(6 9))
(check-equal? (times 3 4) 12)
(check-equal? (times 13 4) (* 13 4))
(check-equal? (addtup '(3 5 2 8)) 18)
(check-equal? (my-+ 1 2) 3)
(check-equal? (mysub 2 1) 1)
;(check-equal? (my- 5 2) 3)

(check-equal? (multiinsertL 'a 'b '(b c b c d b))
              '(a b c a b c d a b))
(check-equal? (multiinsertR 'a 'b '(b c b c b c))
           '(b a c b a c b a c))
(check-equal? (my-+ 2 3) 5)
(check-equal? (multirember 'cup '(coffee cup tea cup and hick cup))
              '(coffee tea and hick))
(check-equal? (subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
              '(vanilla ice cream with chocolate topping))
(check-equal? (subst 'b 'a '(a c d)) '(b c d))
(check-equal? (subst 'topping 'fudge '(ice cream with fudge for dessert))
              '(ice cream with topping for dessert))
(check-equal? (insertL 'c 'd '(a b d)) '(a b c d))
(check-equal?
 (insertR 'topping 'fudge '(ice cream with fudge for dessert))
 '(ice cream with fudge topping for dessert))
(check-equal?
 (insertR 'jalapeno 'and '(tacos tamales and salsa))
 '(tacos tamales and jalapeno salsa))
(check-equal? (firsts '((a b c) (d e f) (g h i))) '(a d g))
(check-equal? (sub1 5) 4)
(check-equal? (member? 'meat '(meat and potatos)) #t)
(check-equal? (member? 'meat '()) #f)
(check-equal? (member? 'liver '(bagels and lox)) #f)
(check-equal? (lat? '(this is a test)) #t)
(check-equal? (lat? '(this (test) should fail)) #f)
(check-equal? (rember 'mint '(lamb chops and mint jelly))
              '(lamb chops and jelly))
(check-equal? (rember 'toast '(bacon lettuce and tomato))
              '(bacon lettuce and tomato))
(check-equal? (rember 'bacon '(bacon lettuce and tomato)) '(lettuce and tomato))
|#