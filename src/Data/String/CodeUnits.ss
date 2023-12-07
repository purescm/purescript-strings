;; -*- mode: scheme -*-
(library (Data.String.CodeUnits foreign)
  (export
    fromCharArray
    toCharArray
    singleton
    _charAt
    _toChar
    _uncons
    length
    countPrefix
    _indexOf
    _indexOfStartingAt
    _lastIndexOf
    _lastIndexOfStartingAt
    take
    drop
    slice
    splitAt)
  (import
    (except (chezscheme) length)
    (only (purs runtime bytestring) bytestring-empty?
                                    bytestring-singleton
                                    bytestring-slice
                                    bytestring-length-code-units
                                    string->bytestring
                                    bytestring-ref
                                    bytestring-uncons-code-unit
                                    bytestring-take
                                    bytestring-drop
                                    bytestring-index-of
                                    bytestring-last-index-of
                                    char-flexvector->bytestring
                                    bytestring->char-flexvector)
    (prefix (purs runtime lib) rt:))

  (define fromCharArray char-flexvector->bytestring)

  (define toCharArray bytestring->char-flexvector)

  (define singleton bytestring-singleton)

  (define _charAt
    (lambda (just)
      (lambda (nothing)
        (lambda (i)
          (lambda (s)
            (if (fx<? i (bytestring-length-code-units s))
              (just (bytestring-ref s i))
              nothing))))))

  (define _toChar
    (lambda (just)
      (lambda (nothing)
        (lambda (s)
          (if (fx=? (bytestring-length-code-units s) 1)
            (just (bytestring-ref s 0))
            nothing)))))

  (define _uncons
    (lambda (just)
      (lambda (nothing)
        (lambda (s)
          (if (bytestring-empty? s)
            nothing
            (let-values ([(head tail) (bytestring-uncons-code-unit s)])
              (just (rt:make-object (cons (string->bytestring "head") head)
                                    (cons (string->bytestring "tail") tail)))))))))

  (define length bytestring-length-code-units)

  (define countPrefix
    (lambda (p)
      (lambda (s)
        (let loop ([i 0] [rest s])
          (if (bytestring-empty? rest)
            i
            (let-values ([(head tail) (bytestring-uncons-code-unit rest)])
              (if (p head)
                (loop (fx1+ i) tail)
                i)))))))

  (define take
    (lambda (n)
      (lambda (s)
        (bytestring-take s n))))

  (define drop
    (lambda (n)
      (lambda (s)
        (bytestring-drop s n))))

  (define _indexOf
    (lambda (just)
      (lambda (nothing)
        (lambda (pattern)
          (lambda (s)
            (let ([res (bytestring-index-of s pattern)])
              (if (not res) nothing (just res))))))))

  (define _indexOfStartingAt
    (lambda (just)
      (lambda (nothing)
        (lambda (pattern)
          (lambda (startAt)
            (lambda (s)
              (if (or (fx<? startAt 0) (fx>? startAt (bytestring-length-code-units s)))
                nothing
                (let ([res (bytestring-index-of (bytestring-slice s startAt) pattern)])
                  (if (not res) nothing (just (fx+ res startAt)))))))))))

  (define _lastIndexOf
    (lambda (just)
      (lambda (nothing)
        (lambda (pattern)
          (lambda (s)
            (let ([res (bytestring-last-index-of s pattern)])
              (if (not res) nothing (just res))))))))

  (define _lastIndexOfStartingAt
    (lambda (just)
      (lambda (nothing)
        (lambda (pattern)
          (lambda (startAt)
            (lambda (s)
              (let* ([i (fx+ (fxmax 0 startAt) (bytestring-length-code-units pattern))]
                     [res (bytestring-last-index-of (bytestring-slice s 0 i) pattern)])
                (if (not res) nothing (just res)))))))))

  (define slice
    (lambda (b)
      (lambda (e)
        (lambda (s)
          (let* ([len (bytestring-length-code-units s)]
                 [bn (if (fx<? b 0) (fx+ len b) b)]
                 [en (if (fx<? e 0) (fx+ len e) e)])
            (bytestring-slice s bn en))))))

  (define splitAt
    (lambda (i)
      (lambda (s)
        (rt:make-object
          (cons (string->bytestring "before") (bytestring-slice s 0 i))
          (cons (string->bytestring "after") (bytestring-slice s i))))))

  )
