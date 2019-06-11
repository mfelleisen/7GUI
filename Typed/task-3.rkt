#! /usr/bin/env gracket
#lang typed/racket/gui

;; a flight booker that allows a choice between one-way and return bookings
;; and, depending on the choice, a start date or a start date and an end date. 

;; ---------------------------------------------------------------------------------------------------
(require 7GUI/Typed/gregor)

;; gregor should not raise an exception when parsing fails, but return #f
(define (to-date {d : String}) (with-handlers ([exn:fail? (λ (_) #f)]) (parse-date d "d.M.y")))

;; ---------------------------------------------------------------------------------------------------
(define DATE0   "27.03.2014")
(define ONE     "one-way flight")
(define RETURN  "return flight")
(define CHOICES `(,ONE ,RETURN))
(define RED     (make-object color% "red"))
(define WHITE   (make-object color% "white"))

(define *kind (list-ref CHOICES 0))    ;; one of the CHOICES
(define *start-date  (to-date DATE0))  ;; date
(define *return-date (to-date DATE0))  ;; date 

(: enable-book (->* () ((U Date False) (U Date False)) Void))
(define (enable-book (start-date *start-date) (return-date *return-date))
  (send book enable #f)
  (when (and start-date (date<=? (today) start-date)
             (or (and (string=? ONE *kind))
                 (and return-date (date<=? start-date return-date))))
    (send book enable #t)))

(: enable-return-book (->* [] [(Instance Choice%) Any] Void))
(define (enable-return-book (self #f) (_evt #f))
  (set! *kind (list-ref CHOICES (or (and self (send self get-selection)) 0)))
  (send return-d enable (string=? RETURN *kind))
  (enable-book))

(: field (-> (-> Date Void) Boolean (Instance Text-Field%)))
(define (field date-setter! enabled)
  (: field-cb (-> (Instance Text-Field%) Any Void))
  (define (field-cb self _evt)
    (define date (to-date (send self get-value)))
    (cond
      [date (send self set-field-background WHITE) (date-setter! date) (enable-book)]
      [else (send self set-field-background RED)   (enable-book #f #f)]))
  (new text-field% [parent frame][label ""][init-value DATE0][enabled enabled] [callback field-cb]))

(define frame    (new frame% [label "flight booker"]))
(define choice   (new choice% [label ""][parent frame][choices CHOICES][callback enable-return-book]))
(define start-d  (field (λ (nu) (set! *start-date nu))  #t))
(define return-d (field (λ (nu) (set! *return-date nu)) #f))
(define book     (new button% [label "Book"][parent frame][callback (λ _ (displayln "confirmed"))]))

(enable-return-book)
(send frame show #t)
