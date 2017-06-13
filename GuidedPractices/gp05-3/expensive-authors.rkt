;; expensive-authors.rkt

(define-struct book (author title on-hand price))

;; A Book is a 
;;  (make-book String String NonNegInt NonNegInt)
;; Interpretation:
;; author is the author’s name
;; title is the title
;; on-hand is the number of copies on hand
;; price is the price in USD

;; book-fn : Book -> ??
;; (define (book-fn b)
;;   (... (book-author b) (book-title b) (book-on-hand b) (book-price
;;   b)))

;; authors-of-expensive-books : Number ListOfBook -> ListOfString
;; Given a price and a list of books, returns the list of all
;; the authors of books whose prices are greater than the given
;; price. 
;; Strategy: Use HOF filter on books

(define (authors-of-expensive-books p books)
  (map book-author
    (filter
      (lambda (b) (> (book-price b) p))
      books)))

;; Challenge problem: what if we wanted to list the authors WITHOUT
;; DUPLICATION? 




