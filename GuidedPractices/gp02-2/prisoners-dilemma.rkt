;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname prisoners-dilemma) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")

;;; The Prisoner's Dilemma

;;; Two men are arrested, but the police do not possess enough
;;; information for an arrest. Following the separation of the two
;;; men, the police offer both a similar deal- if one testifies
;;; against his partner (betrays), and the other stays quiet
;;; (doesn't betray), the betrayer goes free and the non-betrayer receives
;;; the full one-year sentence. If both remain silent, both are
;;; sentenced to only one month in jail for a minor charge. If each
;;; 'rats out' the other, each receives a three-month sentence. Each
;;; prisoner must choose to either betray or remain silent; the
;;; decision of each is kept quiet. What should they do?  

;; The outcomes for player 1 are:

;;                           Player 2 move: 
;;                         Betray    Don't Betray
;; Player 1 move:
;; ---------------------------------------------
;; Betray                   -3        0
;; --------------------------------------------
;; Don't Betray            -12       -1       
;; --------------------------------------------

;;; observe that for both player 1 and player 2, the "right" thing to
;;; do is to betray, since that avoids the possibility of -12.  But of
;;; course if nobody betray, they both wind up with only a month in
;;; jail-- a better outcome!

;;; Philosophical discussion ensues.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; A Move is one of 
;;; -- "betray"
;;; -- "don't betray"

;; ;;; move-fn : Move -> ???
;; (define (move-fn m)
;;   (cond
;;     [(string=? m "betray") ...]
;;     [(string=? m "don't betray") ...]))


;;; outcome : Move Move -> Number
;;; Given the moves of player 1 and player 2, produce the outcome for player 1
;;; examples (outcome "betray" "don't betray") = 0.
;;; see table above.

;;; here's how not to do it:

#;(define (outcome move1 move2)
  (cond
    [(string=? move1 "betray")
     (cond
       [(string=? move2 "betray") -3]
       [(string=? move2 "don't betray") 0])]
    [(string=? move1 "don't betray")
     (cond
       [(string=? move2 "betray") -12]
       [(string=? move2 "don't betray") -1])]))

;;; this doesn't follow the template!!

;;; strategy: 


(begin-for-test
  (check-equal? (outcome "betray" "betray") -3)
  (check-equal? (outcome "betray" "don't betray") 0)
  (check-equal? (outcome "don't betray" "betray") -12)
  (check-equal? (outcome "don't betray" "don't betray") -1))

;; STRATEGY: struct decomp on move1
(define (outcome move1 move2)
  (cond
    [(string=? move1 "betray")
     (outcome-after-betray move2)]
    [(string=? move1 "don't betray")
     (outcome-after-dont-betray move2)]))

;; outcome-after-betray : Move -> Number
;; GIVEN: player2's move
;; RETURNS: the outcome if player1 moved "betray"
;; STRATEGY: struct decomp on move2
(define (outcome-after-betray move2)
  (cond
    [(string=? move2 "betray") -3]
    [(string=? move2 "don't betray") 0]))


;; outcome-after-dont-betray : Move -> Number
;; GIVEN: player2's move
;; RETURNS: the outcome if player1 moved "don't betray"
;; STRATEGY: struct decomp on move2
(define (outcome-after-dont-betray move2)
  (cond
    [(string=? move2 "betray") -12]
    [(string=? move2 "don't betray") -1]))
 