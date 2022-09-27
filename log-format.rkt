#lang racket/base

(provide
  common-log-formatter)

(require
  racket/match
  racket/format
  racket/string
  racket/date
  racket/function
  json
  "common.rkt")

(define no-log-value "-")

(define (common-log-formatter access-log)
  (define (get-fragment key fragments (formatter ~a))
    (let* ((kvp (assoc key fragments))
           (fragment (cdr kvp)))
      (if fragment
        (formatter fragment)
        no-log-value)))

  (define (timestamp->string timestamp)
    (match-let (((list seconds timezone year month day hour minute second) timestamp))
      (apply format `("[~a/~a/~a:~a:~a:~a ~a]"
                      ,year
                      ,@(map (lambda (n) (~r n #:min-width 2 #:pad-string "0"))
                             `(,month ,day ,hour ,minute ,second))
                      ,timezone))))

  (match-let (((list timestamp access-log) access-log))
    (let ((errors (get-fragment 'errors access-log identity)))
      (string-join
        `(,(timestamp->string timestamp)
           ,@(map (lambda (k)
                    (get-fragment k access-log))
                  '(client-ip method path code bytes-transferred time-to-serve uuid))
           ,(if (jsexpr-object-empty? errors) no-log-value (jsexpr->string errors)))))))
