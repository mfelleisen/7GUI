#lang at-exp racket

@;{The task is to build a frame containing two textfields TC and TF representing the
 temperature in Celsius and Fahrenheit, respectively.}

(require htdp/convert)

(define (celsius->fahrenheit c) (+ (* c 9/5) 32))
(define (fahrenheit->celsius f) (* (- f 32) 5/9))

(convert-gui fahrenheit->celsius)