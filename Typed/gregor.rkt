#lang typed/racket

(provide Date parse-date date<=? today)

(require/typed gregor
               [#:opaque Date date?]
               [parse-date (-> String String Date)]
               [date<=?    (-> Date Date Boolean)]
               [today      (-> Date)])