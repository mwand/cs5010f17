;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname scanner-soln) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)

(define-struct suitcase (length width height))

;; A Suitcase is a (make-suitcase PosReal PosReal PosReal)
;; Interp:
;; length, width, and height are the dimensions of the suitcase
;; in cm.

;; template
;; suitcase-fn : Suitcase -> ??
(define (suitcase-fn sc)
  (... (suitcase-length sc)
       (suitcase-width sc)
       (suitcase-height sc)))

;; examples:
(define suitcase1 (make-suitcase 12 35 24))

(define-struct scanner (width height))
;; A Scanner is a (make-scanner PosReal PosReal)
;; Interp:
;; width and height are the width and height of the intake,
;; in cm.

;; template
;; scanner-fn : Scanner -> ??
(define (scanner-fn s)
  (... (scanner-width s)
       (scanner-height s)))

; suitcase-fits-scanner : Suitcase Scanner -> Boolean
; GIVEN: a Suitcase and a Scanner
; RETURNS: true iff the suitcase can fit through the scanner.

; EXAMPLES:
;(suitcase-fits-scanner (make-suitcase 24 12 11) (make-scanner 15 18))
;= true
;(suitcase-fits-scanner (make-suitcase 24 12 21) (make-scanner 15 18))
;= false
;(suitcase-fits-scanner (make-suitcase 24 21 12) (make-scanner 15 18))
;= false
;; STRATEGY: Structural Decomposition on sc : Suitcase
(define (suitcase-fits-scanner sc s)
  (face-fits-scanner
   (min
    (suitcase-length sc)
    (suitcase-width sc)
    (suitcase-height sc))
   (second-smallest
    (suitcase-length sc)
    (suitcase-width sc)
    (suitcase-height sc))
   s))
   
;; EXERCISE: convert these examples into tests

    
;; face-fits-scanner : PosReal PosReal Scanner -> Boolean
;; GIVEN: the dimensions of a suitcase face and a Scanner
;; RETURNS: true iff that face will fit into the scanner's mouth.
;(face-fits-scanner  12 11 (make-scanner 15 18))
;= true
;(face-fits-scanner  11 12 (make-scanner 15 18))
;= true
;(face-fits-scanner  12 19 (make-scanner 15 18))
;= false
;(face-fits-scanner  12 11 (make-scanner 21 18))
;= true
;(face-fits-scanner  12 11 (make-scanner 15 18))
;= true
;; STRATEGY: Structural Decomposition on s : Scanner
(define (face-fits-scanner h w s)
  (rectangle-fits? h w 
       (scanner-width s)
       (scanner-height s)))

;; EXERCISE: convert these examples into tests

;; rectangle-fits? : PosReal PosReal PosReal PosReal -> Boolean
;; GIVEN: the height and width of two rectangles
;; RETURNS: true iff the first rectangle will fit in the second rectangle,
;; either horizontally or vertically.
;; EXAMPLES:
;; (rectangle-fits? 11 20 13 23) = true
;; (rectangle=fits? 20 11 13 23) = true
;; (rectangle-fits? 11 20 13 19) = false
;; (rectangle-fits? 20 11 13 19) = false
;; STRATEGY: function composition
(define (rectangle-fits? h1 w1 h2 w2)
  (or
   (and (< h1 h2) (< w1 w2))
   (and (< h1 w2) (< w1 h2))))  

;; EXERCISE: In my original code, I had (< h1 h2) for the last test.
;; Find an example that reveals this bug.

(check-equal? (rectangle-fits? 11 20 13 23) true)
(check-equal? (rectangle-fits? 20 11 13 23) true)
(check-equal? (rectangle-fits? 11 20 13 19) false)
(check-equal? (rectangle-fits? 20 11 13 19) false)
(check-equal? (rectangle-fits? 20 11 19 13) false)
(check-equal? (rectangle-fits? 3 6 4 5) false)



;; second-smallest : PosReal PosReal PosReal -> PosReal
;; GIVEN: 3 positive reals
;; RETURNS: the second-smallest element
;; EXAMPLES: See tests below.
;; STRATEGY: function composition



(check-equal? (second-smallest 1 2 3) 2)
(check-equal? (second-smallest 1 3 2) 2)
(check-equal? (second-smallest 2 3 1) 2)
(check-equal? (second-smallest 2 1 3) 2)
(check-equal? (second-smallest 3 2 1) 2)
(check-equal? (second-smallest 3 1 2) 2)

;; Why are these tests sufficient?










