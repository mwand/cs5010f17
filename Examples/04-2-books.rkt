;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 04-2-books) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

;;; The Book Store example

;;; Back in the days before Amazon, there used to be things called
;;; "book stores" where you could go to buy books.   A book

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An Author is represented as a String (any string will do)
;; later on, we might refine this definition

;; A Title is represented as a String (any string will do)

;; An International Standard Book Number (ISBN) is represented as a
;; positive integer (PosInt).

;; Actually, an ISBN is a sequence of exactly 13 digits, divided into
;; four fields (see
;; https://en.wikipedia.org/wiki/International_Standard_Book_Number).
;; We don't need to represent all this information, so we will simply
;; represent it as a PosInt.

;; A DollarAmount is represented as an integer,
;; INTERP:  the amount in USD*100.
;; eg: the integer 3679 represents the dollar amount $36.79
;; A DollarAmount may be negative.

;; A BookStatus is represented as
;; (book-status isbn author title cost price on-hand)

;; INTERP:
;; isbn   : ISBN          -- the ISBN of the book
;; author : Author        -- the book's author
;; title  : Title         -- the book's title
;; cost   : DollarAmount  -- the wholesale cost of the book (how much
;;                           the bookstore paid for each copy of the
;;                           book
;; price  : DollarAmount  -- the price of the book (how much the
;;                           bookstore charges a customer for the
;;                           book)
;; on-hand: NonNegInt     -- the number of copies of the book that are
;;                           on hand in the bookstore)

;; IMPLEMENTATION:

(define-struct book-status (isbn author title cost price on-hand))

;; CONSTRUCTOR TEMPLATE:
;; (make-book-status ISBN Author Title DollarAmount DollarAmount
;; NonNegInt)

;; OBSERVER TEMPLATE:
;; book-status-fn : BookStatus -> ??
(define (book-status-fn b)
  (...
   (book-status-isbn b)
   (book-status-author b)
   (book-status-title b)
   (book-status-cost b)
   (book-status-price b)
   (book-status-on-hand b)))



;; An Inventory is represented as a list of BookStatus, in increasing
;; ISBN order, with at most one entry per ISBN.

;; CONSTRUCTOR TEMPLATES:
;; empty
;; (cons bs inv)
;; -- WHERE
;;    bs  is a BookStatus
;;    inv is an Inventory
;;    and
;;     (bookstatus-isbn bs) is less than the ISBN of any book in inv.

;; OBSERVER TEMPLATE:

;; inv-fn : Inventory -> ??
(define (inv-fn inv)
  (cond
    [(empty? inv) ...]
    [else (...
            (first inv)
	    (inv-fn (rest inv)))]))

;; since (first inv) is a BookStatus, it would also be OK to write:

#;
(define (inv-fn inv)
  (cond
    [(empty? inv) ...]
    [else (...
            (book-status-fn (first inv))
	    (inv-fn (rest inv)))]))


;; remember, 'list' is an abbreviation for a series of 'cons's.

(define inv1
  (list
    (make-book-status 1001001 "Felleisen" "HtDP/1"   1200 4500  20)
    (make-book-status 1002751 "Wand"      "EOPL"      500 1000  50)
    (make-book-status 237648  "Shakespeare" "Hamlet"   25  399 100)
    (make-book-status 82465   "Shakespeare" "Macbeth"  50  499  10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The inventory doesn't say what bookstore it belongs to, or what
;; date and time it represents.

;; A Date is represented as a ....

;; A BookstoreState is represented as a (bookstore-state date stock)
;; INTERP:
;; date   : Date         -- the date we are modelling
;; stock  : Inventory    -- the inventory of the bookstore as of 9am ET on
;;                        the given date.

;;  IMPLEMENTATION:

(define-struct bookstore-state (date stock))

;; CONSTRUCTOR TEMPLATE
;; (make-bookstore-state Date Inventory)

;; OBSERVER TEMPLATE
;; state-fn : BookstoreState -> ??
(define (state-fn bss)
  (... (bookstore-state-date bss)
       (bookstore-state-stock bss)))

;; An InventoryHistory is a list of BookstoreStates, in decreasing 
;; order of date (ie, most recent is first)

;; CONSTRUCTOR TEMPLATES:
;; empty             -- the empty history
;; (cons inv hist)
;;   WHERE:
;;   inv  is an Inventory
;;   hist is an InventoryHistory
;;   AND
;;   the date of inv is more recent than any of the dates in hist.

;; OBSERVER TEMPLATE:
;; ih-fn : InventoryHistory -> ??
(define (ih-fn hist)
  (cond
    [(empty? hist) ...]
    [else (... (first hist)
               (ih-fn (rest hist)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTION DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; book-value : BookStatus -> DollarAmount
;; GIVEN: the inventory record for a book
;; RETURNS: the sales value of the copies on hand of the given book
;; EXAMPLE:
;; (book-inventory-value (make-book-status
;;                          1001001 "Felleisen"
;;                         "HtDP/1"   1200 4500  20))
;; = 4500 * 20 = 90000
;; STRATEGY: Use template for Book on b

(define (book-value b)
  (* (book-status-on-hand b) (book-status-price b)))

;; TEST
(begin-for-test
  (check-equal?
   (book-value (make-book-status 1001001 "Felleisen"
                                           "HtDP/1"   1200 4500  20))
   90000
   "value of 20 Felleisens at $45 should have been $900"))

;; inventory-authors : Inventory -> AuthorList
;; GIVEN: An Inventory
;; RETURNS: A list of the all the authors of the books in the
;; inventory.  Repetitions are allowed.  Books with no copies in stock
;; are included. The authors may appear in any order.
;; EXAMPLE: (inventory-authors inv1) = (list "Felleisen" "Wand"
;;                                       "Shakespeare" "Shakespeare")
;; STRATEGY: Use observer template for Inventory

(define (inventory-authors inv)
  (cond
    [(empty? inv) empty]
    [else (cons
           (book-status-author (first inv))
           (inventory-authors  (rest inv)))]))

(begin-for-test
  (check-equal?
   (inventory-authors inv1)
   (list "Felleisen" "Wand" "Shakespeare" "Shakespeare")
   "inventory-authors: wrong answer for inv1"))

;;; SEE GUIDED PRACTICE 4.3 FOR MORE EXAMPLES
