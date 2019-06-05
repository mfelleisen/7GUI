#! /usr/bin/env gracket
#lang racket/gui

;; a flight booker that allows a choice between one-way and return bookings
;; and, depending on the choice, a start date or a start date and an end date. 


;; TODO:
;; -- how to propagate changes to *start-date and *return-date
;; -- how to get vertical panes 

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

(define (enable-book (start-date *start-date) (return-date *return-date))
  (send book enable #f)
  (when (and start-date (date<=? (today) start-date)
             (or (and (string=? ONE *kind-flight))
                 (and return-date (date<=? start-date return-date))))
    (send book enable #t)))

(define (enable-return-book . self+evt)
  (set! *kind-flight (list-ref CHOICES (send (first self+evt) get-selection))))

(define ((field-cb date-setter! enabled) self _evt)
  (define date (to-date (send self get-value)))
  (cond
    [date (send self set-field-background WHITE) (date-setter! date) (enable-book)]
    [else (send self set-field-background RED)   (enable-book #f #f)]))

(define date-field%
  (class text-field%
    (init setter! enabled)
    (super-new [label ""][init-value DATE0][enabled enabled] [callback (field-cb setter! enabled)])))

(gui "Flight Booker"
     {(*kind-flight (list-ref CHOICES 0)
                    (λ (*kind-flight)
                      (send return-d enable (string=? RETURN *kind-flight))
                      (enable-book))) ;; one of the CHOICES
      (*start-date  (to-date DATE0)  values)     ;; date
      (*return-date (to-date DATE0)  values)}    ;; date
     ((choice% [label ""][choices CHOICES][callback enable-return-book])
      (date-field% (setter! (λ (nu) (set! *start-date nu))) (enabled #t))
      (#:id return-d date-field% (setter! (λ (nu) (set! *return-date nu))) (enabled #f))
      (#:id book button% [label "Book"][callback (λ _ (displayln "confirmed"))])))