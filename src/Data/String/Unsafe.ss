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
        (with-exception-handler
          (lambda (e)
            (raise-continuable
              (make-message-condition
                "Data.String.Unsafe.charAt: Invalid index.")))
          (lambda ()
            (string-ref s i))))))

    (define char
      (lambda (s)
        (if (= (string-length s) 1)
          (string-ref s 0)
          (raise-continuable
            (make-message-condition
              "Data.String.Unsafe.char: Expected string of length 1.")))))
)
