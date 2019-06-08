#! /usr/bin/env gracket
#lang at-exp racket/gui

;; a create-read-update-deleted MVC implementation 

;; ---------------------------------------------------------------------------------------------------
(require 7GUI/Macros/7guis)
(define-syntax-rule (when=> (name exp) body ...) (let ((name exp)) (begin name body ...))) ;; -> base 

;; ---------------------------------------------------------------------------------------------------
(define (selector! nu) (set! *prefix nu))
(define (select s) (string-prefix? s *prefix))
(define (data->selected! _)  (set! *selected (if (string=? "" *prefix) *data (filter select *data))))

(define-state *data '("Emil, Hans" "Mustermann, Max" "Tisch, Roman") data->selected!)
(define-state *prefix "" data->selected!)
(define-state *selected *data (λ (s) (send lbox set s))) ;; selected = (filter select data)

(define (Create-cb *data) (append *data (list (get-name))))
(define (Update-cb i) (if i (operate-on i (curry cons (get-name))) none))
(define (Delete-cb i) (if i (operate-on i values) none))

#; {N [[Listof X] -> [Listof X]] -> [Listof X]}
;; traverse list to the i-th position of selected in data, then apply operator to rest (efficiency)
;; ASSUME selected = (filter selector data)
;; ASSUME i <= (length selected)
(define (operate-on i operator)
  (let sync ((i i) (data *data) (selected *selected))
    (if (select (first data))
        (if (zero? i)
            (operator (rest data))
            (cons (first data) (sync (sub1 i) (rest data) (rest selected))))
        (cons (first data) (sync i (rest data) selected)))))

(define (get-name) (string-append (send surname get-value) ", " (send name get-value)))

;; ---------------------------------------------------------------------------------------------------
(define-syntax-rule (from field get-value f) (λ (_) (f (send field get-value))))

(define-gui frame "CRUD"
  (#:horizontal
   (#:vertical
    (#:id prefix text-field% #:change *prefix (from prefix get-value values)
     [label "Filter prefix: "][init-value ""])
    (#:id lbox list-box% [label #f][choices '()][min-width 100][min-height 100]))
   (#:vertical
    (#:id name    text-field% [label "Name:      "][init-value ""][min-width 200])
    (#:id surname text-field% [label "Surname: "][init-value ""][min-width 200])))
  (#:horizontal 
   (button% #:change *data Create-cb [label "Create"])
   (button% #:change *data (from lbox get-selection Update-cb) [label "Update"])
   (button% #:change *data (from lbox get-selection Delete-cb) [label "Delete"])))

(selector! "")
(send frame show #t)