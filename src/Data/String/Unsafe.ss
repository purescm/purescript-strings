(library (Data.String.Unsafe foreign)
  (export charAt char)
  (import (only (chezscheme) define lambda let-values)
          (only (purs runtime bytestring) bytestring-ref bytestring-singleton bytestring-uncons-code-unit))

  (define charAt
    (lambda (n)
      (lambda (s)
        (bytestring-ref s n))))

  (define char
    (lambda (s)
      (let-values ([(c _) (bytestring-uncons-code-unit s)])
        c)))

  )

