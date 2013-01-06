#lang racket

(require srfi/1)


(define (generate-nonce)
  (string-join
   (map number->string (map (lambda (x) (random 10)) (range 10)))
   ""))

(define (get-timestamp)
  (number->string (current-seconds)))

(provide (all-defined-out))
