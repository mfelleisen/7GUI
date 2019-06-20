#! /usr/bin/env gracket
#lang racket/gui

;; a flight booker that allows a choice between one-way and return bookings
;; and, depending on the choice, a start date or a start date and an end date. 

;; ---------------------------------------------------------------------------------------------------
(require 7GUI/Macros/7guis 7GUI/Macros/7state)
(require gregor)

;; gregor should not raise an exception when parsing fails, but return #f
(define (to-date d) (with-handlers ([exn? (λ (_) #f)]) (parse-date d "d.M.y")))

;; ---------------------------------------------------------------------------------------------------
(define DATE0   "27.03.2014")
(define ONE     "one-way flight")
(define RETURN  "return flight")
(define CHOICES `(,ONE ,RETURN))
(define RED     (make-object color% "red"))
(define WHITE   (make-object color% "white"))

(define (enable-book . _)
  (send book enable #f)
  (when (and *start:date (date<=? (today) *start:date)
             (or (and (string=? ONE *kind))
                 (and *return:date (date<=? *start:date *return:date))))
    (send book enable #t)))

(define-state *kind ONE (λ (kf) (send return-date enable (string=? RETURN kf)) (enable-book)))
(define-state *start:date  (to-date DATE0) enable-book)
(define-state *return:date (to-date DATE0) enable-book)

(define date-field% (class text-field% (init e) (super-new [label ""][init-value DATE0][enabled e])))

(define check-date
  (with date #:post to-date 
        (cond
          [date (send self set-field-background WHITE) date]
          [else (send self set-field-background RED)   (send book enable #f) none])))

(define set-kind (with x #:post (curry list-ref CHOICES) #:method get-selection x))

(gui "Flight Booker" 
     (choice% #:change *kind set-kind [label ""][choices CHOICES])
     (date-field% #:change *start:date check-date (e #t))
     (#:id return-date date-field% #:change *return:date check-date (e #f))
     (#:id book button% [label "Book"][enabled #f][callback (λ _ (displayln "confirmed"))]))
