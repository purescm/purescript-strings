#!chezscheme
(library (Bench.Main foreign)
  (export toChezString readTextFile lengthNative stringAppend)
  (import (chezscheme)
          (only (purs runtime bytestring) bytestring->string string->bytestring)
          (prefix (purs runtime lib) rt:))

  (define toChezString bytestring->string)

  (define readTextFile
    (lambda (path)
      (lambda ()
        (with-input-from-file (bytestring->string path)
          (lambda ()
            (let loop ((chars '())
                       (next-char (read-char)))
               (if (eof-object? next-char)
                   (string->bytestring (list->string (reverse chars)))
                   (loop (cons next-char chars)
                         (read-char)))))))))

  (define lengthNative string-length)

  (define stringAppend string-append)

  )
