(define-struct mothership (name crew daughters))
;; INTERPRETATION:
;;  name is the name of the ship
;;  crew is the list of crewmembers of the ship
;;  daughters is the list of daughter ships

(define-struct drone ())

;; A Spaceship is one of -
;; -- (make-mothership String ListOfMartian ListOfShip)
;; OR
;; -- (make-drone)

;; Template
;; ship-fn : Spaceship -> ??
#|
(define (ship-fn s)
  (cond
    [(mothership? s) 
     (... (mothership-name s)
          (lom-fn (mothership-crew s))
          (los-fn (mothership-daughters s)))]
    [(drone? s) 
     ...]))
|#

;; A ListofShip is either - 
;; -- empty
;; -- (cons Spaceship ListOfShip)

;; Template
;; los-fn : ListOfShip -> ??
#|
(define (los-fn los)
  (cond
    [(empty? los) ...]
    [else (... (ship-fn (first los))
               (los-fn (rest los)))]))
|#

(define-struct martian (name))
;; A Martian is a (make-martian String)
;; Template:
;; martian-fn : Martian -> ??
#|
(define (martian-fn m)
  (... (martian-name m)))
|#

;; A ListOfMartian is one of - 
;; -- empty
;; -- (cons Martian (ListOfMartian))
;; Template
;; lom-fn : ListOfMartian -> ??
#|(define (lom-fn lom)
  (cond 
    [(empty? lom) ...]
    [else (... (martian-fn (first lom))
               (lom-fn (rest lom)))]))
|#

;; an example. 

(define ship1
  (make-mothership "qwerk"
    ;; the crew
    (list
      (make-martian "hork")
      (make-martian "bork")
      (make-martian "quork"))
    ;; the daughters
    (list
      (make-mothership "berk"
        (list
          (make-martian "alfalfa")
          (make-martian "balfalfa")
          (make-martian "mork"))
        (list
          (make-drone)
          (make-mothership "little-berk"
            empty
            empty)
          (make-mothership "little-berk2"
            (list
              (make-martian "jork")
              (make-martian "dork")
              (make-martian "mork"))
            empty)))
      (make-mothership "herk"
        (list
          (make-martian "peon")
          (make-martian "deon"))
        empty))))

        
      