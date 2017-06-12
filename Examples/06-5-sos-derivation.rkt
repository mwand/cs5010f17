;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 06-5-sos-and-loss) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt") 

;; S-expressions
;; 
;; An Sexp-of-String (SoS) is either
;; -- a String
;; -- a List-of SoS
;; 
;; A List-of SoS (LoSS) is either
;; -- empty
;; -- (cons SoS LoSS)
;; 
;; SoS's:
;; 
;; "abcd"
;; "def"
;; ("abcd" "def")
;; ("abcd" ("abcd" "def"))
;; ("abc" "def" "ghi")
;; (("abc" "def" "ghi")
;;  ("abcd" ("abcd" "def"))
;;  "def"
;;  ("abcd" "def"))
;; 

#|
Derivation of (("alice" "bob") "carole")

In list notation, this is

(list (list "alice" "bob") "carole")

In cons notation, it becomes
(cons
 (cons "alice" (cons "bob" empty))
 (cons "carole" 
       empty))

This is 

SoS
-> LoSS
-> (cons SoS LoSS)
-> (cons LoSS LoSS)
-> (cons (cons SoS LoSS)
         LoSS)
-> (cons (cons SoS LoSS)
         LoSS)
-> (cons (cons String LoSS)
         LoSS)
-> (cons (cons "alice" LoSS)
         LoSS)
-> (cons (cons "alice" (cons SoS LoSS))
         LoSS)
-> (cons (cons "alice" (cons String LoSS))
         LoSS)
-> (cons (cons "alice" (cons "bob" LoSS))
         LoSS)
-> (cons (cons "alice" (cons "bob" empty))
         LoSS)
-> (cons (cons "alice" (cons "bob" empty))
         (cons SoS LoSS))
-> (cons (cons "alice" (cons "bob" empty))
         (cons String LoSS))
-> (cons (cons "alice" (cons "bob" empty))
         (cons "carole" LoSS))
-> (cons (cons "alice" (cons "bob" empty))
         (cons "carole" empty))

15 steps!
|#
