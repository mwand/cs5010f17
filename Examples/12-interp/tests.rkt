#lang racket

(require "datatypes.rkt")
(require "interp.rkt")   ; choose one of:  interp.rkt,
                         ; interp-tuesday.rkt, or interp-wednesday.rkt
(require rackunit)
(require "extras.rkt")

(begin-for-test

  ;; constants
  
  ;; 3 => 3

  (check-equal?
    (pgm-value 
      (make-program empty (make-const 3)))
    3)

  ;; difference expressions
  
  ;; (21 - 3) => 18

  (check-equal?
    (pgm-value
      (make-program empty
        (make-diff (make-const 21) (make-const 3))))
    18)

  (check-equal?
    (pgm-value
      (make-program empty
        (make-diff 
          (make-const 20)
          (make-diff 
            (make-const 6)
            (make-const 4)))))
      18)                    ;; 20 - (6 - 4) = 20 - 2 = 18

 ;; (27 - 7) - (6 - 4) => 20 - 2 = 18  
  (check-equal?
    (pgm-value
      (make-program empty
        (make-diff 
          (make-diff (make-const 27) (make-const 7))
          (make-diff 
            (make-const 6)
            (make-const 4)))))
      18)

  ;; if-expressions. 

  (check-equal?
    (pgm-value
      (make-program empty
        (make-ifzero
          (make-diff (make-const 6) (make-const 4))
          (make-diff (make-const 20) (make-const 1))
          (make-diff (make-const 20) (make-const 3)))))
    17)

  (check-equal?
    (pgm-value
      (make-program empty
        (make-ifzero
          (make-diff (make-const 6) (make-const 6))
          (make-diff (make-const 20) (make-const 1))
          (make-diff (make-const 20) (make-const 3)))))
    19)

  ;; variables

  (check-equal?
    (pgm-value
      (make-program 
        (list
          (make-const-decl 'x 14)
          (make-const-decl 'y 4))
        (make-variable 'x)))
    14)

  (check-equal?
    (pgm-value
      (make-program 
        (list
          (make-const-decl 'x 14)
          (make-const-decl 'y 4))
        (make-variable 'y)))
    4)
        
  (check-equal?
    (pgm-value
      (make-program 
        (list
          (make-const-decl 'x 14)
          (make-const-decl 'y 4))
        (make-diff
          (make-variable 'x)
          (make-variable 'y))))
    10)
  
 

  ;; single functions

  (check-equal?
    (pgm-value
      (make-program 
        (list
          (make-const-decl 'x 14)
          (make-const-decl 'y 4)
          (make-fcn-decl 'f '(x) (make-diff
                                   (make-variable 'x) 
                                   (make-const 3)))
          (make-fcn-decl 'g '(x y) (make-diff
                                     (make-variable 'x) 
                                     (make-variable 'y))))
        (make-call 'f (list (make-const 10)))))
    7)

    ;; fcn with two arguments

    (check-equal?
      (pgm-value
        (make-program 
          (list
            (make-const-decl 'x 14)
            (make-const-decl 'y 4)
            (make-fcn-decl 'f '(x) (make-diff
                                     (make-variable 'x) 
                                     (make-const 3)))
            (make-fcn-decl 'g '(x y) (make-diff
                                       (make-variable 'x) 
                                       (make-variable 'y))))
          (make-call 'g (list
                          (make-const 10)
                          (make-const 3)))))
        7)
    
    

    ;; one function calls another

   (check-equal?
      (pgm-value
        (make-program 
          (list
            (make-const-decl 'x 14)
            (make-const-decl 'y 4)
            (make-fcn-decl 'f '(x) (make-diff
                                     (make-variable 'x) 
                                     (make-const 3)))
            (make-fcn-decl 'g '(x y) (make-diff
                                       (make-variable 'x) 
                                       (make-variable 'y)))
            (make-fcn-decl 'h '(u v)
              (make-call 'f
                (list (make-call 'g
                        (list 
                         (make-variable 'u)
                         (make-variable 'v)))))))
          (make-call 'h (list
                          (make-const 10)
                          (make-const 3)))))
        4)
   
   ;; (h 10 3) = (f (g 10 3)) = (f 7) = 4
   
   

   ;; let's build a recursive function: prod.  

   ;; First let's do sum:

   (check-equal?
     (pgm-value
       (make-program
         (list
           (make-fcn-decl 'sum '(a b)
             (make-diff
               (make-variable 'a)
               (make-diff
                 (make-const 0)
                 (make-variable 'b)))))
         (make-call
           'sum
           (list
             (make-const 5)
             (make-const 11)))))
     16)

   ;; now we can do prod:

   (check-equal?
     (pgm-value
       (make-program
         (list
           (make-fcn-decl 'sum '(a b)
             (make-diff
               (make-variable 'a)
               (make-diff
                 (make-const 0)
                 (make-variable 'b))))
           (make-fcn-decl 'prod '(x y)
             (make-ifzero
               (make-variable 'x)
               (make-const 0)
               (make-call
                 'sum
                 (list
                   (make-variable 'y)
                   (make-call 'prod
                     (list
                       (make-diff 
                         (make-variable 'x)
                         (make-const 1))
                       (make-variable 'y))))))))
         (make-call 'prod
           (list
             (make-const 3)
             (make-const 4)))))
     12)
             
   ;; example to demonstrate dynamic scoping

   (check-equal?
     (pgm-value
       (make-program
         (list
           (make-const-decl 'x 33)
           (make-fcn-decl 'f '(x) 
             (make-call
               'g
               (list (make-variable 'x))))
           (make-fcn-decl 'g '(y)
             (make-variable 'x)))         
         ;; which x do we get?  Under the usual  static scoping, we'd
         ;; expect the 33, but this interpreter gives us 44 <sigh/>
         (make-call
           'f
           (list
             (make-const 44)))))
     44)


   )



      

