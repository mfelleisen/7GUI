#! /usr/bin/env gracket
#lang racket/gui

;; a flight booker that allows a choice between one-way and return bookings
;; and, depending on the choice, a start date or a start date and an end date. 

;; ---------------------------------------------------------------------------------------------------
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

(define *kind-flight (list-ref CHOICES 0)) ;; one of the CHOICES
(define *start-date  (to-date DATE0))      ;; date
(define *return-date (to-date DATE0))      ;; date 

(define (enable-book (start-date *start-date) (return-date *return-date))
  (send book enable #f)
  (when (and start-date (date<=? (today) start-date)
             (or (and (string=? ONE *kind-flight))
                 (and return-date (date<=? start-date return-date))))
    (send book enable #t)))

(define (enable-return-book . self+evt)
  (set! *kind-flight (list-ref CHOICES (if (null? self+evt) 0 (send (first self+evt) get-selection))))
  (send return-d enable (string=? RETURN *kind-flight))
  (enable-book))

(define (field date-setter! enabled)
  (define (field-cb self evt)
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