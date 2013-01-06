#lang racket

; Racket oauth 1.0 implementation

(require net/base64)
(require net/uri-codec)
(require net/url)
(require web-server/private/util)
(require web-server/stuffers/hmac-sha1
	 openssl/sha1)

(require "common.rkt")


(define *oauth-version-key "oauth_version")
(define *oauth-nonce-key "oauth_nonce")
(define *oauth-timestamp-key "oauth_timestamp")
(define *oauth-consumer-key-key "oauth_consumer_key")
(define *oauth-signature-method-key "oauth_signature_method")
(define *oauth-signature-key "oauth_signature")

(struct oauth (url version consumer-key consumer-secret signature-method nonce timestamp))

(define (normalize-params params)
  (define (param-sort-method k1v1 k2v2)
    (let ((key1 (car k1v1))
	  (val1 (cadr k1v1))
	  (key2 (car k2v2))
	  (val2 (cadr k2v2)))
      (if (not (equal? key1 key2))
	  (string<? key1 key2)
	  (string<? val1 val2))))
  
  (sort params param-sort-method))

(define (fetch-params oauth-obj)
  (url-query (string->url (oauth-url oauth-obj))))

(define (build-oauth-params oauth-obj)
  (let* ((url-params (map (lambda (x) (list (symbol->string (car x)) (cdr x))) 
			  (fetch-params oauth-obj)))
	 (oauth-easy-params (list
			     `("oauth_consumer_key" ,(oauth-consumer-key oauth-obj))
			     `("oauth_signature_method" ,(oauth-signature-method oauth-obj))
			     `("oauth_timestamp" ,(oauth-timestamp oauth-obj))
			     `("oauth_nonce" ,(oauth-nonce oauth-obj))
			     `("oauth_version" "1.0"))))
    (normalize-params (append url-params oauth-easy-params))))

(define (get-sgn-base-string oauth-obj)
  
  (define (url-parametrize-param a-list)
    (cons (string->symbol (first a-list)) (second a-list)))

  (let* 
      ((params (build-oauth-params oauth-obj))
       (supplied-url (string->url (oauth-url oauth-obj))))
    
    (string-join
     (list 
      "GET"
      (uri-encode (string-join
		   (list (url-scheme supplied-url)
			 "://"
			 (url-host supplied-url)
			 (url-path->string (url-path supplied-url)))
		   ""))
      (uri-encode (string-join
		   (map (lambda (a-kv-pair) (string-join (list (first a-kv-pair) (second a-kv-pair)) "=")) params)
		   "&")))
     "&")))

(define (sign s key method)
  (define (sign-hmac-sha1 s key)
    (string-trim 
     (format "~a"
	     (base64-encode (HMAC-SHA1 (string->bytes/locale key) (string->bytes/locale s))))))

  (define (sign-plaintext s)
    (error "NOT IMPLEMENTED YET!"))

  (cond ((equal? method "HMAC-SHA1") (sign-hmac-sha1 s key))
	((eq? method "PLAINTEXT") (sign-plaintext s))
	((eq? method "RSA-SHA1") (error "NOT IMPLEMENTED YET!"))
	(else (error "FUCK UP"))))


(provide (all-defined-out))
