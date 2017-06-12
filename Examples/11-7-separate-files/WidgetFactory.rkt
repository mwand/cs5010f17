#lang racket

(require 2htdp/universe)
(require "interfaces.rkt")

;; the factory requires each of the tools for making widgets :}
(require "SBall.rkt")
(require "FlashingBall.rkt")
(require "Square.rkt")

;; provide the function, hide the class
(provide make-widget-factory)

;; The WidgetFactory% class

;; accepts key events and adds SWidgetListeners to the world.
;; We have only one such class and one object of this class.  This is
;; called the "singleton pattern".

;; Wall World -> SWidget
;; RETURNS: a widget factory which will tell each widget it creates
;; about the given wall, and which will add each new widget to the
;; given world.  The widget factory is itself an swidget.
(define (make-widget-factory wall world)
  (new WidgetFactory% [wall wall][world world]))


(define WidgetFactory%
  (class* object% (SWidget<%>)

    (init-field world)  ; the world to which the factory adds balls
    (init-field wall)   ; the wall that the new balls should bounce
                        ; off of.

    (super-new)

    ; KeyEvent -> Void
    (define/public (after-key-event kev)
      (cond
        [(key=? kev "b")
         (send world add-stateful-widget (make-ball wall))]
        [(key=? kev "f")
         (send world add-stateful-widget (make-flashing-ball wall))]
        [(key=? kev "s")
         (send world add-stateful-widget (make-square wall))]
        ))

    ;; the Factory has no other behavior

    (define/public (after-tick) this)
    (define/public (after-button-down mx my) this)
    (define/public (after-button-up mx my) this)
    (define/public (after-drag mx my) this)
    (define/public (add-to-scene s) s)

    ))

