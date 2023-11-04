;; -*- mode: scheme -*-

(library (Data.String.Unsafe foreign)
  (export char charAt)
  (import
    (only (rnrs base) = define if lambda string-length string-ref)
    (only (rnrs exceptions) raise-continuable with-exception-handler)
    (only (rnrs conditions) make-message-condition))

  (define charAt
    (lambda (i)
      (lambda (s)
        (string-ref s i))))

    (define char
      (lambda (s)
        (string-ref s 0)))
)
