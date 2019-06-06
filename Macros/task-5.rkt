#! /usr/bin/env gracket
#lang at-exp racket/gui

;; a create-read-update-deleted MVC implementation 

;; ---------------------------------------------------------------------------------------------------
(require 7GUI/Macros/7guis)

(define *data '("Emil, Hans" "Mustermann, Max" "Tisch, Roman"))
(define *selector "")
(define *selected *data) ;; selected = (filter select data)

(define (selector! nu) (set! *selector nu) (data->selected!))
(define (select s) (string-prefix? s *selector))
(define (data->selected!)  (set! *selected (if (string=? "" *selector) *data (filter select *data))))

(define-syntax-rule (def-! (name x ...) exp) (define (name x ...) (set! *data exp) (data->selected!)))
(def-! (create-entry new-entry) (append *data (list new-entry)))
(def-! (update-entry new-entry i) (operate-on i (curry cons new-entry) *data select *selected))
(def-! (delete-from i) (operate-on i  values)) *data select *selected

#; {N [[Listof X] -> [Listof X]] [Listof X] [X -> Boolean] [Listof X] -> [Listof X]}
;; traverse list to the i-th position of selected in data, then apply operator to rest (efficiency)
;; ASSUME selected = (filter selector data)
;; ASSUME i <= (length selected)
(define (operate-on i operator (data *data) (select select) (selected *selected))
  (let sync ((i i) (data data) (selected selected))
    (if (select (first data))
        (if (zero? i)
            (operator (rest data))
            (cons (first data) (sync (sub1 i) (rest data) (rest selected))))
        (cons (first data) (sync i (rest data) selected)))))

;; ---------------------------------------------------------------------------------------------------
(define-syntax-rule (def-cb (name x) exp ...) (define (name x _y) exp ... (send lbox set *selected)))
(def-cb (prefix-cb field) (selector! (if (string? field) field (send field get-value))))
(def-cb (Create-cb _b) (create-entry (retrieve-name)))
(def-cb (Update-cb _b) (common-cb (curry update-entry (retrieve-name))))
(def-cb (Delete-cb _b) (common-cb delete-from))

(define (common-cb f) (define i (send lbox get-selection)) (when i (f i)))
(define (retrieve-name) (string-append (send surname get-value) ", " (send name get-value)))

;; ---------------------------------------------------------------------------------------------------
(define frame   (new frame% [label "CRUD"]))
(define hpane1  (new horizontal-pane% [parent frame][border 10][alignment '(left bottom)]))
(define vpane1  (new vertical-pane% [parent hpane1]))
(new text-field% [parent vpane1][label "Filter prefix: "][init-value ""][callback prefix-cb])
(define lbox    (new list-box% [parent vpane1][label #f][choices '()][min-width 100][min-height 100]))
(define vpane2  (new vertical-pane% [parent hpane1][alignment '(right center)]))
(define name    (new text-field% [parent vpane2][label "Name:      "][init-value ""][min-width 200]))
(define surname (new text-field% [parent vpane2][label "Surname: "][init-value ""][min-width 200]))
(define hpane2  (new horizontal-pane% [parent frame]))
(new button% [label "Create"][parent hpane2][callback Create-cb])
(new button% [label "Update"][parent hpane2][callback Update-cb])
(new button% [label "Delete"][parent hpane2][callback Delete-cb])

(prefix-cb "" '***)
(send frame show #t)