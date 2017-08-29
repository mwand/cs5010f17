;; Here's a proposal for talking about interfaces.

;; A class has two interfaces:
;; -- the methods it provides (perhaps with the help of its
;; superclasses)
;; -- the methods it requires (from its subclasses).

;; Unfortunately, the Racket object system only allows us to document
;; the former; the latter is implicit in the list of (abstract ..)
;; declarations in the superclass.

;; The concept is that the superclass provides services to its
;; subclasses; only the instantiable subclass provides the full interface.

;; Under this plan, the superclass wouldn't claim to implement
;; SBall<%>; only the instantiable subclasses do so.

;; Need to change L11.2 slide 10 to remove (abstract add-to-scene).
;; L11.2 slide 17 needs to change to reflect the new interface.

;; note that add-to-scene is in SBall<%> but not in DWprovides<%>

(define DWprovides<%>
  (interface ()
    update-wall-pos
    after-tick
    after-button-down
    after-button-up
    after-drag))

(define DWHooks<%>
  (interface ()
    next-x-pos
    next-speed
    in-this?))

(define DraggableWidget%
  (class* object% (DWprovides%)

    (abstract 
      next-x-pos
      next-speed
      in-this?)

    (define/public (update-wall-pos n) ...)

    (define/public (after-tick) 
      (if selected?
        this
        (let ((x1     (send this next-x-pos))
              (speed1 (send this next-speed)))
          ...)))

    ;; to be supplied by each subclass
    (abstract next-x-pos)
    (abstract next-speed)

    ; NOT an abstract method of this class
    ; (abstract add-to-scene)

    ; after-button-down : Integer Integer -> Void
    (define/public (after-button-down mx my)
      (if (send this in-this? mx my) ...))

    ;; to be supplied by the subclass
    (abstract in-this?)

    ; after-button-up : Integer Integer -> Void
    (define/public (after-button-up mx my)
      (if (send this in-this? mx my) ...))

    ; after-drag : Integer Integer -> Void
    (define/public (after-drag mx my) ...)   
    
    ))

(define Ball%
  (class*

    ;; inherit method implementations from DraggableWidget%
    DraggableWidget%
    
    ;; all the methods of SBall<%> are either inherited from
    ;; DraggableWidget% or defined locally.
    ;; Since we inherit from DraggableWidget%, we need to provide its
    ;; hooks
    (SBall<%> DWHooks<%>)

    (define/public (add-to-scene ...) ...)

    (define/override (next-x-pos) ...)
    (define/override (next-speed) ...)
    (define/override (in-this? other-x other-y) ...)
    
    ))

