;;; Program to write email messages informing students of their pairing
;;; for Problem Sets 06 through 08.
;;;
;;; This program is written in Scheme, not in Racket.

(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme file))

(define boilerplate1
  '("For CS 5010 Problem Sets 06 through 08, your partner will be\n"
    "the student whose CCIS ID is\n\n    "))

(define boilerplate2
  '("\n\nYou should be able to contact him or her "
    "by sending email to\n\n    "))

(define boilerplate3
  '("\n\nYou can learn his or her name by running the finger command on\n"
    "any CCIS Linux machine:\n\n    finger "))

(define boilerplate4
  '("\n\nReminder:  You are not allowed to discuss Problem Set 05 with\n"
    "anyone, including your new partner.\n\nWill\n"))

;;; write-message : Symbol Symbol -> void
;;; GIVEN: two CCIS IDs id1, id2
;;; EFFECT: uses id1 to name a newly created file in the current directory
;;;     that tells student id1 his/her partner will be student id2

(define (write-message id1 id2)
  (call-with-output-file
   (symbol->string id1)
   (lambda (q) (write-message-to-port q id1 id2))))

;;; write-message-to-port : TextualOutputPort Symbol Symbol -> void
;;; GIVEN: an output port and two CCIS IDs id1 and id2
;;; EFFECT: writes a message to the port telling student id1 his/her
;;;     partner will be student id2

(define (write-message-to-port out id1 id2)
  (let ((display (lambda (x) (display x out))))
    (for-each display boilerplate1)
    (display id2)
    (for-each display boilerplate2)
    (display id2)
    (display "@ccs.neu.edu")
    (for-each display boilerplate3)
    (display id2)
    (for-each display boilerplate4)))

;;; write-messages : TextualInputPort -> void
;;; Given an input port from which the CCIS IDs of successive pairs
;;; can be read as symbols, creates an email message for each student
;;; in the current directory.

(define (write-messages p)
  (let* ((id1 (read p))
         (id2 (read p)))
    (if (and (symbol? id1)
             (symbol? id2))
        (begin (write-message id1 id2)
               (write-message id2 id1)
               (write-messages p)))))

(call-with-input-file "../pairings1.txt" write-messages)
