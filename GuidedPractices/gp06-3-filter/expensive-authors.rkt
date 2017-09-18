;; expensive-authors.rkt

;; authors-of-expensive-books : Real BookStatusList -> AuthorList
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




