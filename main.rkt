#lang racket

(require math/base
         net/base64)

(require date
         sha
         uuid)

(define hashcash-version 1)
(define partial-pre-image-bits 20)

(define (get-date-string)
  (let ([the-date (current-date 0)])
    (format "~a~a~a~a~a"
            (date-year the-date)
            (date-month the-date)
            (date-day the-date)
            (date-hour the-date)
            (date-minute the-date))))

(define (base64-encode-string s)
  (bytes->string/utf-8 (base64-encode (string->bytes/utf-8 s) "")))

(define (verify-hashcash header-string)
  (let* ([hashed (bytes->hex-string (sha1 (string->bytes/utf-8 header-string)))]
         [partial-pre-image-bytes (/ partial-pre-image-bits 4)]
         [zero-string (make-string partial-pre-image-bytes #\0)]
         [check-string (substring hashed 0 partial-pre-image-bytes)]
         [match? (string=? zero-string check-string)])
    match?))

(define (generate-hashcash resource-string)
  (let ([random-data (base64-encode-string (uuid-string))])
        (define (try counter)
          (let* ([c (base64-encode-string (number->string counter))]
                 [the-header (format "~a:~a:~a:~a::~a:~a"
                                     hashcash-version
                                     partial-pre-image-bits
                                     (get-date-string)
                                     resource-string
                                     random-data
                                     c)])
            (if (verify-hashcash the-header) the-header (try (add1 counter)))))
  (try (random-integer 0 (expt 2 160)))))

(provide generate-hashcash verify-hashcash)

