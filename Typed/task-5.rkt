#! /usr/bin/env gracket
#lang typed/racket/gui

;; a create-read-update-deleted MVC implementation 

;; ---------------------------------------------------------------------------------------------------
(define-type Data [Listof String])

(define *data '("Emil, Hans" "Mustermann, Max" "Tisch, Roman"))
(define *selector "")
(define *selected *data) ;; selected = (filter select data)

(define (selector! {nu : String}) (set! *selector nu) (data->selected!))
(define (select {s : String}) (string-prefix? s *selector))
(define (data->selected!) : Void
  (set! *selected (if (string=? "" *selector) *data (filter select *data))))

(define-syntax-rule (def-! (name x ...) exp) (define (name x ...) (set! *data exp) (data->selected!)))
(def-! (create-entry {new-entry : String}) (append *data (list new-entry)))
(def-! (update-entry {new-entry : String} {i : Natural})
  (operate-on i (λ {{x : Data}} (cons new-entry x)) *data select *selected))
(def-! (delete-from {i : Natural}) (operate-on i values))

(: operate-on (->* (Natural [Data -> Data]) (Data [String -> Boolean] Data) Data))
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
(define-syntax-rule (def-cb (name {x : T}) exp ...)
  (define (name {x : (U String T)} {_y : Any}) : Void exp ... (send lbox set *selected)))
(def-cb (prefix-cb {f : (Instance Text-Field%)}) (selector! (if (string? f) f (send f get-value))))
(def-cb (Create-cb {_b : Any}) (create-entry (retrieve-name)))
(def-cb (Update-cb {_b : Any}) (common-cb (λ ({x : Natural}) (update-entry (retrieve-name) x))))
(def-cb (Delete-cb {_b : Any}) (common-cb delete-from))

(: common-cb (-> (-> Natural Void) Void))
(define (common-cb f) (define i (send lbox get-selection)) (when i (f i)))
(: retrieve-name (-> String))
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
