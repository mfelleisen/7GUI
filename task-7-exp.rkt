#lang racket

(provide
 LETTERS

 #; {String -> {U Integer False}}
 valid-content

 #; {String -> Exp* u False}
 string->exp*

 #; {Exp* u False -> String}
 exp*->string

 #; {Exp* -> (Listof Ref*)}
 depends-on

 #;{ Exp* [Hashof Ref* Integer]  -> Integer}
 evaluate)

(define LETTERS  "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

;; EXPRESSIONS: EXTERNAL, STRING-BASED REPRESENTATION 
#; {Index      : N in [0,99]}
#; {Reference  is a Letter followed by an Index}
#; {Expression =  Reference || Integer || (+ Expression Expression)}

;; EXPRESSIONS: INTERNAL 
#; {Ref*       =  (List Letter Index)}
#; {Exp*       =  Ref*      || Integer || (list '+ Exp* Exp*)}

(define (valid-content x)
  (define n (string->number x))
  (and n (integer? n) n))
  
(define (string->exp* x)
  (define ip (open-input-string x))
  (define y (read ip))
  (and (eof-object? (read ip))
       (let loop ([y y])
         (match y
           [(? valid-cell) (valid-cell y)]
           [(? integer?) y]
           [(list '+ y1 y2) (list '+ (loop y1) (loop y2))]
           [else #f]))))
  
(define (exp*->string exp*)
  (if (boolean? exp*)
      ""
      (let render-exp* ((exp* exp*))
        (match exp*
          [(? number?) (~a exp*)]
          [(list letter index) (~a letter index)]
          [(list '+ left right) (format "(+ ~a ~a)" (render-exp* left) (render-exp* right))]))))
  
(define (depends-on exp*)
  (let loop ([exp* exp*][accumulator (set)])
    (match exp*
      [(? number?) accumulator]
      [(list L I) (set-add accumulator exp*)]
      [(list '+ left right) (loop left (loop right accumulator))])))
  
(define (evaluate exp* global-env)
  (let loop ([exp* exp*])
    (match exp*
      [(? number?) exp*]
      [(list L I) (hash-ref global-env exp* 0)]
      [(list '+ left right) (+ (loop left) (loop right))])))

#; {Symbol -> (List Letter Index) u False}
(define (valid-cell x:sym)
  (and (symbol? x:sym)
       (let* ([x:str (symbol->string x:sym)]
              [x (regexp-match #px"([A-Z])(\\d\\d)" x:str)])
         (or (and x (split x))
             (let ([x (regexp-match #px"([A-Z])(\\d)" x:str)])
               (and x (split x)))))))

(define (split x)
  (match x [(list _ letter index) (list (string-ref letter 0) (string->number index))]))