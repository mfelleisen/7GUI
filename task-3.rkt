#lang racket/gui

;; a flight booker that allows a choice between one-way and return bookings
;; and, depending on the choice, a start date or a start date and an end date. 

;; ---------------------------------------------------------------------------------------------------
(require gregor)

;; this should be fixed in gregor 
(define (to-date d) (with-handlers ([exn? (λ (_) #f)]) (parse-date d "d.m.y")))

;; ---------------------------------------------------------------------------------------------------
(define DATE0   "27.03.2014")
(define ONE     "one-way flight")
(define RETURN  "return flight")
(define CHOICES `(,ONE ,RETURN))
(define RED     (make-object color% "red"))
(define WHITE   (make-object color% "white"))

(define *flight      (list-ref CHOICES 0)) ;; one of the CHOICES
(define *start-date  (to-date DATE0))      ;; date
(define *return-date (to-date DATE0))      ;; date 

(define (enable-book (start-date *start-date) (return-date *return-date))
  (send book enable #f)
  (when (and start-date (date<=? (today) start-date)
             (or (string=? ONE *flight)
                 (and return-date (date<=? start-date return-date))))
    (send book enable #t)))

(define (enable-return-book . self+evt)
  (set! *flight (list-ref CHOICES (if (empty? self+evt) 0 (send  (first self+evt) get-selection))))
  (send return-d enable (string=? RETURN *flight))
  (enable-book))

(define-syntax-rule (field *date e)
  (let ((field-cb (lambda (self evt)
                    (define date (to-date (send self get-value)))
                    (cond
                      [date (set! *date date)
                            (send self set-field-background WHITE)
                            (enable-book)]
                      [else (send self set-field-background RED)
                            (enable-book #f #f)]))))
    (new text-field% [parent frame][label ""][init-value DATE0][enabled e] [callback field-cb])))

(define frame    (new frame% [label "flight booker"]))
(define choice   (new choice% [label ""][parent frame][choices CHOICES][callback enable-return-book]))
(define start-d  (field *start-date #t))
(define return-d (field *return-date #f))
(define book     (new button% [label "Book"][parent frame][callback (compose displayln list)]))

(enable-return-book)
(send frame show #t)