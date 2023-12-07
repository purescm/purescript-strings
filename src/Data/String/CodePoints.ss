(library (Data.String.CodePoints foreign)
  (export length
          unsafeCodePointAt0
          _codePointAt
          countPrefix
          fromCodePointArray
          _toCodePointArray
          singleton
          _take
          _uncons)
  (import
    (prefix (purs runtime lib) rt:)
    (prefix (purs runtime srfi :214) srfi:214:)
    (only (purs runtime bytestring) string->bytestring
                                    bytestring-length-code-points
                                    bytestring-ref-code-point
                                    bytestring-empty?
                                    bytestring-uncons-code-point
                                    bytestring-take-code-points
                                    code-points->bytestring)
    (only (rnrs base) apply define lambda let let-values cons error if not)
    (only (chezscheme) fx1+ fx=?))

  (define length bytestring-length-code-points)

  (define unsafeCodePointAt0
    (lambda (s)
      (bytestring-ref-code-point s 0)))

  (define _codePointAt
    (lambda (Just)
      (lambda (Nothing)
        (lambda (index)
          (lambda (s)
            (let loop ([i 0] [cur s])
              (if (bytestring-empty? cur)
                Nothing
                (let-values ([(head tail) (bytestring-uncons-code-point cur)])
                  (if (fx=? i index)
                    (Just head)
                    (loop (fx1+ i) tail))))))))))

  (define countPrefix
    (lambda (pred)
      (lambda (s)
        (let loop ([count 0] [rest s])
          (if (bytestring-empty? rest)
            count
            (let-values ([(head tail) (bytestring-uncons-code-point rest)])
              (if (pred head)
                (loop (fx1+ count) tail)
                count)))))))

  (define fromCodePointArray
    (lambda (cps)
      (apply code-points->bytestring (srfi:214:flexvector->list cps))))

  (define singleton code-points->bytestring)

  (define _take
    (lambda (n)
      (lambda (s)
        (bytestring-take-code-points s n))))

  (define _uncons
    (lambda (Just)
      (lambda (Nothing)
        (lambda (s)
          (if (bytestring-empty? s)
            Nothing
            (let-values ([(c tail) (bytestring-uncons-code-point s)])
              (Just (rt:make-object
                      (cons (string->bytestring "head") c)
                      (cons (string->bytestring "tail") tail)))))))))

  (define _toCodePointArray
    (lambda (fallback)
      (lambda (unsafeCodePointAt0)
        (lambda (s)
          (error #f "Data.String.CodePoints._toCodePointArray not implemented")))))

  )
