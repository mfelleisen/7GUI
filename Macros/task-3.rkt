#! /usr/bin/env gracket
#lang racket/gui

;; a flight booker that allows a choice between one-way and return bookings
;; and, depending on the choice, a start date or a start date and an end date. 

;; ---------------------------------------------------------------------------------------------------
(require gregor)
(require 7GUI/Macros/7guis)

;; gregor should not raise an exception when parsing fails, but return #f
(define (to-date d) (with-handlers ([exn? (λ (_) #f)]) (parse-date d "d.M.y")))

;; ---------------------------------------------------------------------------------------------------
(define DATE0   "27.03.2014")
(define ONE     "one-way flight")
(define RETURN  "return flight")
(define CHOICES `(,ONE ,RETURN))
(define RED     (make-object color% "red"))
(define WHITE   (make-object color% "white"))

(define (enable-book (start-date *start:date))
  (send book enable #f)
  (when (and start-date (date<=? (today) start-date)
             (or (and (string=? ONE *kind-flight))
                 (and *return:date (date<=? start-date *return:date))))
    (send book enable #t)))

(define date-field%
  (class text-field%
    (init setter! enabled)
    (define (field-cb self _evt)
      (define date (to-date (send self get-value)))
      (cond
        [date (send self set-field-background WHITE) (setter! date)]
        [else (send self set-field-background RED)   (enable-book #f)]))
    (super-new [label ""][init-value DATE0][enabled enabled] [callback field-cb])))

(gui "Flight Booker"
     {(*kind-flight ONE (λ (kf) (send return-date enable (string=? RETURN kf)) (enable-book)))
      (*start:date  (to-date DATE0)  (λ _ (enable-book)))
      (*return:date (to-date DATE0)  (λ _ (enable-book)))}  
     (choice% [label ""][choices CHOICES]
              [callback (λ (it _) (set! *kind-flight (list-ref CHOICES (send it get-selection))))])
     (#:id start-date  date-field% (setter! (λ (nu) (set! *start:date nu)))  (enabled #t))
     (#:id return-date date-field% (setter! (λ (nu) (set! *return:date nu))) (enabled #f))
     (#:id book button% [label "Book"][enabled #f][callback (λ _ (displayln "confirmed"))]))