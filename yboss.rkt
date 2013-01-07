#lang racket

(require net/url)
(require racket/list)

(require (planet dherman/json:4:0))

(require "common.rkt")
(require "oauth.rkt")


(define *yboss-base-url-string* "http://yboss.yahooapis.com/ysearch/")

; buckets: web / image / blogs etc. For now I just support queries from 1 bucket
; query: query string
; consumer-key: yahoo! BOSS consumer-key
; consumer-secret: see above
; count: # of results per query
; format: "xml" or "json"
; num-results: 0 - 1000 (YBOSS Supports max 1000 results only)
(struct yboss (bucket query consumer-key consumer-secret count format num-results))


(define (build-urls a-yboss)

  (let* ((base-url-string
	  (string-join
	   (list *yboss-base-url-string*  ; base url
		 (yboss-bucket a-yboss))  ; bucket
	   ""))
	 (starts (range 0 (yboss-num-results a-yboss) 50))
	 (counts (append (map (lambda (x) "50") (rest starts)) 
			 (list (number->string (remainder (yboss-num-results a-yboss) 50))))))
    
    (map 
     (lambda (a-sv) (struct-copy 
		url 
		(string->url base-url-string) 
		(query (list
			(cons 'q (yboss-query a-yboss))
			(cons 'format (yboss-format a-yboss))
			(cons 'start (number->string (first a-sv)))
			(cons 'count (second a-sv))))))
     (map list starts counts))))

(define (get-results a-yboss)
  (results->map
   (yboss-format a-yboss)
   (flatten
    (map (lambda (x) (port->string (get-pure-port x)))
	 (map (lambda (u) (build-final-url (oauth 
				       (url->string u) 
				       "1.0" 
				       (yboss-consumer-key a-yboss)
				       (yboss-consumer-secret a-yboss)
				       "HMAC-SHA1"
				       (generate-nonce)
				       (get-timestamp)
				       "GET")))
	      (build-urls a-yboss))))))

(define (results->map format results)
  (if (equal? format "json")
      (map json->jsexpr results)
      (error "NOT IMPLEMENTED!")))

(define (fetch-field-from-results field results-maps)
  (flatten
   (map (lambda (results-map) 
	  (map (lambda (x) (hash-ref x field))
	      (hash-ref (hash-ref (hash-ref results-map 'bossresponse) 'web) 'results)))
	results-maps)))
