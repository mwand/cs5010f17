;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname flight) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(provide make-UTC
         UTC-hour
         UTC-minute
         UTC=?
         make-flight
         flight-name
         departs
         arrives
         departs-at
         arrives-at
         flight=?)

(define UTC-KEY (list 'UTC))          ; key to unlock UTC values
(define FLIGHT-KEY (list 'Flight))    ; key to unlock Flight values

;;; Abstract data types UTC and Flight.

;;; A UTC is a (make-UTC NonNegInt NonNegInt)
;;; where the first argument is less than 24 and the second less than 60
;;; Interpretation:
;;;     the first argument is the hour (in UTC)
;;;     the second argument is the minute (in UTC)
;;; Template:
;;; utc-fn : UTC -> ??
#;
(define (utc-fn u)
  (... (utc-hour u)
       (utc-minute u)))

;;; make-UTC : NonNegInt NonNegInt -> UTC
;;; GIVEN: the hour and minute parts of a time in UTC
;;; WHERE: the hour is less than 24 and the minute is less than 60
;;; RETURNS: the UTC time determined by the arguments
;;; EXAMPLES: see examples for UTC-hour and UTC-minute

(define (make-UTC h m)
  (if (and (integer? h)
           (integer? m)
           (exact? h)
           (exact? m)
           (<= 0 h 23)
           (<= 0 m 59))
      (make-value 'UTC
                  '(hour minute)
                  UTC-KEY
                  (lambda (name)
                    (cond ((eq? name 'hour) h)
                          ((eq? name 'minute) m))))
      (error 'make-UTC "bad arguments: " (list h m))))

;;; UTC-hour   : UTC -> NonNegInt
;;; UTC-minute : UTC -> NonNegInt
;;; GIVEN: a representation of a time in UTC as returned by make-UTC
;;; RETURNS: the hour part or minute part (respectively)
;;; EXAMPLES:
;;;     (UTC-hour   (make-UTC 15 31))  =>  15
;;;     (UTC-minute (make-UTC 15 31))  =>  31

(define (UTC-hour u)
  (get u 'UTC UTC-KEY 'hour))

(define (UTC-minute u)
  (get u 'UTC UTC-KEY 'minute))

;;; UTC=? : UTC UTC -> Boolean
;;; GIVEN: two UTC times
;;; RETURNS: true iff they have the same hour and minute parts
;;; EXAMPLES:
;;;     (UTC=? (make-UTC 15 31) (make-UTC 15 31))  =>  true
;;;     (UTC=? (make-UTC 15 31) (make-UTC 14 31))  =>  false
;;;     (UTC=? (make-UTC 15 31) (make-UTC 15 32))  =>  false

(define (UTC=? u1 u2)
  (and (= (UTC-hour u1) (UTC-hour u2))
       (= (UTC-minute u1) (UTC-minute u2))))

;;; A Flight is a (make-flight String String String UTC UTC)
;;; Interpretation:
;;;     the first argument is the name of the flight
;;;     the second argument is the name of the originating airport
;;;     the third argument is the name of the destination airport
;;;     the fourth argument is the time of departure
;;;     the fifth argument is the time of arrival
;;; Template:
;;; flight-fn : Flight -> ??
#;
(define (flight-fn f)
  (... (flight-name f)
       (departs f)
       (arrives f)
       (departs-at f)
       (arrives-at f)))

;;; make-flight : String String String UTC UTC -> Flight
;;; GIVEN: the name of a flight, the name of the airport from
;;;     which the flight departs, the name of the airport at
;;;     which the flight arrives, the time of departure in UTC,
;;;     and the time of arrival in UTC
;;; RETURNS: a flight value that encapsulates the given information
;;; EXAMPLE:
;;;     (define flt1
;;;       (make-flight "United 448"
;;;                    "BOS" "DEN"
;;;                    (make-UTC 20 03) (make-UTC 00 53)))

(define (make-flight name departs arrives departs-at arrives-at)
  (if (and (string? name)
           (string? departs)
           (string? arrives)
           (UTC? departs-at)
           (UTC? arrives-at))
      (make-value 'Flight
                  '(name departs arrives departs-at arrives-at)
                  FLIGHT-KEY
                  (lambda (msg)
                    (cond ((eq? msg 'name) name)
                          ((eq? msg 'departs) departs)
                          ((eq? msg 'arrives) arrives)
                          ((eq? msg 'departs-at) departs-at)
                          ((eq? msg 'arrives-at) arrives-at))))
      (error 'make-flight
             "bad arguments: "
             (list name departs arrives departs-at arrives-at))))

;;; flight-name : Flight -> String
;;; GIVEN: a flight
;;; RETURNS: the name of the Flight
;;; EXAMPLE:
;;;     (flight-name flt1)  =>  "United 448"

(define (flight-name f)
  (get f 'Flight FLIGHT-KEY 'name))

;;; departs : Flight -> String
;;; GIVEN: a flight
;;; RETURNS: the name of the airport from which the flight departs
;;; EXAMPLE:
;;;     (departs flt1)  =>  "BOS"

(define (departs f)
  (get f 'Flight FLIGHT-KEY 'departs))

;;; arrives : Flight -> String
;;; GIVEN: a flight
;;; RETURNS: the name of the airport at which the flight arrives
;;; EXAMPLE:
;;;     (arrives flt1)  =>  "DEN"

(define (arrives f)
  (get f 'Flight FLIGHT-KEY 'arrives))

;;; departs-at : Flight -> UTC
;;; GIVEN: a flight
;;; RETURNS: the time (in UTC, see above) at which the flight departs
;;; EXAMPLE:
;;;     (departs-at flt1)  =>  (make-UTC 20 03)

(define (departs-at f)
  (get f 'Flight FLIGHT-KEY 'departs-at))

;;; arrives-at : Flight -> UTC
;;; GIVEN: a flight
;;; RETURNS: the time (in UTC, see above) at which the flight arrives
;;; EXAMPLE:
;;;     (arrives-at flt1)  =>  (make-UTC 00 53)

(define (arrives-at f)
  (get f 'Flight FLIGHT-KEY 'arrives-at))

;;; flight=? : Flight Flight -> Boolean
;;; GIVEN: two flights
;;; RETURNS: true if and only if the two flights have the same
;;;     name, depart from the same airport, arrive at the same
;;;     airport, depart at the same time, and arrive at the same time
;;; EXAMPLES:
;;;
;;;     (flight=? flt1 flt1)  =>  true
;;;
;;;     (flight=? (make-flight "United 448"
;;;                            "BOS" "DEN"
;;;                            (make-UTC 20 00) (make-UTC 00 55))
;;;               flt1)
;;; =>  false

(define (flight=? f1 f2)
  (and (string=? (flight-name f1) (flight-name f2))
       (string=? (departs f1) (departs f2))
       (string=? (arrives f1) (arrives f2))
       (UTC=? (departs-at f1) (departs-at f2))
       (UTC=? (arrives-at f1) (arrives-at f2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Help functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; An ADTvalue is a
;;; (make-value Symbol ListOfSymbol ListOfSymbol (Symbol -> Any))
;;;
;;; Interpretation:
;;;     the first argument is the name of ADT
;;;     the second argument lists operations of that ADT
;;;     the third argument is a key that unlocks the fourth argument
;;;     the fourth argument is a function that takes the name of
;;;         a unary operation and returns the value of that operation
;;;         the value of the ADT represented by the result of calling
;;;         make-value

;;; make-value : Symbol ListOfSymbol Any (Symbol -> Any) -> Any
;;; GIVEN: a symbol sym that identifies an ADT, a list of symbols
;;;     that name the operations of that ADT, an arbitrary object
;;;     to use as a key, and a function that accepts the name of
;;;     an operation from the given list and returns the value to
;;;     be returned by that operation
;;; RETURNS: a representation of a value of the ADT
;;; EXAMPLES: see make-UTC and make-flight
;;; STRATEGY: obfuscation and a lot of run-time error checking

(define (make-value id ops key0 fn)
  (let ((locked-fn (lambda (key op)
                     (if (and (eq? key key0)
                              (memq op ops))
                         (fn op)
                         (error id
                                "violation of abstraction barrier "
                                (list key op))))))
    (list id locked-fn)))

;;; get : ADTvalue Symbol ListOfSymbol Symbol -> Any
;;; GIVEN: a value x returned by make-value,
;;;     the name of the ADT to which it belongs,
;;;     the key passed to make-value when x was created, and
;;;     the name of an operation of that ADT
;;; RETURNS: the value of that operation on the value x

(define (get val id key op)
  (if (and (list? val)
           (= 2 (length val))
           (eq? id (first val)))
      ((second val) key op)
      (error op "violation of abstraction barrier " val)))

;;; UTC? : Any -> Boolean
;;; GIVEN: any value
;;; RETURNS: true iff the value looks like it might be a value of the UTC ADT

(define (UTC? x)
  (and (list? x)
       (= 2 (length x))
       (eq? 'UTC (first x))))

;;; Note: Black-box tests cannot achieve 100% test coverage of
;;; the functions above because those functions test for error
;;; situations that cannot arise if their contracts and purpose
;;; statements are always satisfied.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Black-box tests.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-test

  (check-equal? (UTC-hour (make-UTC 0 59))
                0
                "UTC-hour doesn't work on (make-UTC 0 59)")

  (check-equal? (UTC-minute (make-UTC 0 59))
                59
                "UTC-minute doesn't work on (make-UTC 0 59)")

  (check-equal? (UTC-hour (make-UTC 23 0))
                23
                "UTC-hour doesn't work on (make-UTC 23 0)")

  (check-equal? (UTC-minute (make-UTC 23 0))
                0
                "UTC-minute doesn't work on (make-UTC 23 0)")

  (check-equal? (UTC=? (make-UTC 12 13) (make-UTC 12 13))
                true
                "UTC=? doesn't return true when it should")

  (check-equal? (UTC=? (make-UTC 14 15) (make-UTC 14 16))
                false
                "UTC=? doesn't return false when the minute is different")
  
  (check-equal? (UTC=? (make-UTC 14 15) (make-UTC 13 15))
                false
                "UTC=? doesn't return false when the hour is different"))

(begin-for-test

  (check-equal? (let* ((u0 (make-UTC 0 0))
                       (u1 (make-UTC 23 59))
                       (f (make-flight "Braniff 278" "SF" "LA" u0 u1)))
                  (list (flight-name f)
                        (departs f)
                        (arrives f)
                        (UTC-hour (departs-at f))
                        (UTC-minute (departs-at f))
                        (UTC-hour (arrives-at f))
                        (UTC-minute (arrives-at f))))
                (list "Braniff 278" "SF" "LA" 0 0 23 59)
                "wrong answer for Braniff 278")

  (check-equal? (let* ((u1 (make-UTC 0 59))
                       (u2 (make-UTC 23 01))
                       (u3 (make-UTC 0 59))
                       (u4 (make-UTC 23 1))
                       (f1 (make-flight "United 3865" "MSP" "PDX" u1 u2))
                       (f2 (make-flight "United 3865" "MSP" "PDX" u3 u4)))
                  (flight=? f1 f2))
                true
                "flight=? should have returned true")

  (check-equal? (let* ((u1 (make-UTC 0 58))
                       (u2 (make-UTC 23 01))
                       (u3 (make-UTC 0 59))
                       (u4 (make-UTC 23 1))
                       (f1 (make-flight "United 3865" "MSP" "PDX" u1 u2))
                       (f2 (make-flight "United 3865" "MSP" "PDX" u3 u4)))
                  (flight=? f1 f2))
                false
                "but 58 is different from 59")
  
  (check-equal? (let* ((u1 (make-UTC 1 59))
                       (u2 (make-UTC 23 01))
                       (u3 (make-UTC 0 59))
                       (u4 (make-UTC 23 1))
                       (f1 (make-flight "United 3865" "MSP" "PDX" u1 u2))
                       (f2 (make-flight "United 3865" "MSP" "PDX" u3 u4)))
                  (flight=? f1 f2))
                false
                "but 1 is different from 0")

  (check-equal? (let* ((u1 (make-UTC 0 59))
                       (u2 (make-UTC 23 01))
                       (u3 (make-UTC 0 59))
                       (u4 (make-UTC 22 1))
                       (f1 (make-flight "United 3865" "MSP" "PDX" u1 u2))
                       (f2 (make-flight "United 3865" "MSP" "PDX" u3 u4)))
                  (flight=? f1 f2))
                false
                "but 22 is not 23")

  (check-equal? (let* ((u1 (make-UTC 0 59))
                       (u2 (make-UTC 23 01))
                       (u3 (make-UTC 0 59))
                       (u4 (make-UTC 23 7))
                       (f1 (make-flight "United 3865" "MSP" "PDX" u1 u2))
                       (f2 (make-flight "United 3865" "MSP" "PDX" u3 u4)))
                  (flight=? f1 f2))
                false
                "but 1 is not 7")

  (check-equal? (let* ((u1 (make-UTC 0 59))
                       (u2 (make-UTC 23 01))
                       (u3 (make-UTC 0 59))
                       (u4 (make-UTC 23 1))
                       (f1 (make-flight "United 3865" "MSP" "PdX" u1 u2))
                       (f2 (make-flight "United 3865" "MSP" "PDX" u3 u4)))
                  (flight=? f1 f2))
                false
                "but PdX is not PDX")

  (check-equal? (let* ((u1 (make-UTC 0 59))
                       (u2 (make-UTC 23 01))
                       (u3 (make-UTC 0 59))
                       (u4 (make-UTC 23 1))
                       (f1 (make-flight "United 3865" "MSP" "PDX" u1 u2))
                       (f2 (make-flight "United 3865" "LAX" "PDX" u3 u4)))
                  (flight=? f1 f2))
                false
                "but LAX is not MSP")

  (check-equal? (let* ((u1 (make-UTC 0 59))
                       (u2 (make-UTC 23 01))
                       (u3 (make-UTC 0 59))
                       (u4 (make-UTC 23 1))
                       (f1 (make-flight "United 3864" "MSP" "PDX" u1 u2))
                       (f2 (make-flight "United 3865" "MSP" "PDX" u3 u4)))
                  (flight=? f1 f2))
                false
                "but 3864 is not 3865"))
