(library (Test.Data.String.CodePoints foreign)
  (export str)
  (import
    (only (rnrs base) define lambda)
    (only (purs runtime bytestring) code-points->bytestring))

  (define str (code-points->bytestring #x61 #xDC00 #xD800 #xD800 #x16805 #x16A06 #x7A))
  )
