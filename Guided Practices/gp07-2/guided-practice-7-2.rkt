Consider the computation of

(free-vars
  (make-app
    (make-lam
      'x
      (make-app
        'x
        (make-lam
          'y
          (make-app
            'x
            (make-app 'y 'z)))))
    (make-lam
      'y
      (make-app
        'u
        (make-lam
          'z
          (make-app
            'x
            (make-app 'y 'z)))))))


Which of the following will appear as calls to free-vars-in-subexp during
this computation?

1.	(free-vars-in-subexp (list 'z 'y) (make-app 'y 'z))
2.	(free-vars-in-subexp (list 'x 'y) (make-app 'y 'z))
3.	(free-vars-in-subexp (list 'y 'x) (make-app 'y 'z))
4.	(free-vars-in-subexp (list 'x 'z) (make-app 'y 'z))
