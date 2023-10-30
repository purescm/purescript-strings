;; -*- mode: scheme -*-

(library (Data.String.CodeUnits foreign)
  (export
    fromCharArray
    toCharArray
    singleton
    _charAt
    _toChar
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
    (only (rnrs base) + < = > >= and cons define if lambda let max min or string string-length string-ref)
    (prefix (purs runtime lib) rt:)
    (prefix (purs runtime srfi :152) srfi:152:)
    (prefix (purs runtime srfi :214) srfi:214:))

  (define fromCharArray srfi:214:flexvector->string)

  (define toCharArray srfi:214:string->flexvector)

  (define singleton string)

  (define _charAt
    (lambda (just)
      (lambda (nothing)
        (lambda (i)
          (lambda (s)
            (if (and (>= i 0) (< i (string-length s)))
              (just (string-ref s i))
              nothing))))))

  (define _toChar
    (lambda (just)
      (lambda (nothing)
        (lambda (s)
          (if (= 1 (string-length s))
            (just (string-ref s 0))
            nothing)))))

  (define length string-length)

  (define countPrefix
    (lambda (p)
      (lambda (s)
        (or
          (srfi:152:string-skip s p)
          (string-length s)))))

  (define _indexOf
    (lambda (just)
      (lambda (nothing)
        (lambda (x)
          (lambda (s)
            (let ((i (srfi:152:string-contains s x)))
              (if i (just i) nothing)))))))

  (define _indexOfStartingAt
    (lambda (just)
      (lambda (nothing)
        (lambda (x)
          (lambda (startAt)
            (lambda (s)
              (if (or (> startAt (string-length s)) (< startAt 0))
                nothing
                (let ((i (srfi:152:string-contains s x startAt)))
                  (if i (just i) nothing)))))))))

  (define _lastIndexOf
    (lambda (just)
      (lambda (nothing)
        (lambda (x)
          (lambda (s)
            (let ((i (srfi:152:string-contains-right s x)))
              (if i (just i) nothing)))))))

  (define _lastIndexOfStartingAt
    (lambda (just)
      (lambda (nothing)
        (lambda (x)
          (lambda (startAt)
            (lambda (s)
              (let ((clamped (clamp-to-length s (+ startAt (string-length x)))))
                (let ((i (srfi:152:string-contains-right s x 0 clamped)))
                  (if i (just i) nothing)))))))))

  (define take
    (lambda (n)
      (lambda (s)
          (srfi:152:string-take s (clamp-to-length s n)))))

  (define drop
    (lambda (n)
      (lambda (s)
          (srfi:152:string-drop s (clamp-to-length s n)))))

  (define slice
    (lambda (b)
      (lambda (e)
        (lambda (s)
          (let ((len (string-length s)))
            (let
              ((bn (if (>= b 0) b (+ len b)))
               (en (if (>= e 0) e (+ len e))))
              (if (> bn en)
                ""
                (srfi:152:substring s (max 0 bn) (min len en)))))))))

  (define splitAt
    (lambda (i)
      (lambda (s)
        (let ((clamped (clamp-to-length s i)))
          (rt:make-object
            (cons "before" (srfi:152:string-take s clamped))
            (cons "after" (srfi:152:string-drop s clamped)))))))

  (define clamp-to-length
    (lambda (s n)
      (max 0 (min n (string-length s)))))
)
