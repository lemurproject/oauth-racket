#lang racket

(require net/url)

(require "oauth.rkt")
(require "common.rkt")


(define (test-generate-nonce)
  (not (equal? (generate-nonce) (generate-nonce))))

(define (test-normalize-params)
  (let ((params '(("greeting" "hello")
		 ("q1" "how")
		 ("q1" "are")
		 ("a" "b"))))
    (equal? (normalize-params params)
	    '(("a" "b")
	      ("greeting" "hello")
	      ("q1" "are")
	      ("q1" "how")))))

(define (test-oauth)
  (let* ((oauth-obj (oauth 
		     *request-endpoint* 
		     '1.0 
		     *consumer-key* 
		     *consumer-secret* 
		     'hmac-sha1))
	 (params (append 
    (unless (assert-oauth oauth-obj) (error "FUCK UP"))

    

    #t))
