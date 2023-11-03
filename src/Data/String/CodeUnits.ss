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
    (only (rnrs base) + - * / < = > >= and begin cons define if lambda let let* max min not or string string-length string-ref)
    (prefix (rnrs bytevectors) bvs:)
    (only (rnrs io ports) bytevector->string make-transcoder utf-16-codec)
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
            (let*
              ([cus (bvs:string->utf16 s)]
               [v (bvs:make-bytevector 2)]
               [ix (* i 2)]
               [tx (make-transcoder (utf-16-codec))]
               [max-ix (- (bvs:bytevector-length cus) 2)])
              (if (> ix max-ix)
                nothing
                (begin
                  (bvs:bytevector-u16-set!
                    v
                    0
                    (bvs:bytevector-u16-ref cus ix (bvs:native-endianness))
                    (bvs:native-endianness))
                  (just (string-ref (bytevector->string v tx) 0))))))))))

  (define _toChar
    (lambda (just)
      (lambda (nothing)
        (lambda (s)
          (if (= 1 (string-length s))
            ((((_charAt just) nothing) 0) s)
            nothing)))))

  (define length
    (lambda (s)
      (/
        (bvs:bytevector-length (bvs:string->utf16 s))
        2)))

  (define countPrefix
    (lambda (p)
      (lambda (s)
        (let*
          ([cus (bvs:string->utf16 s)]
           [v (bvs:make-bytevector 2)]
           [tx (make-transcoder (utf-16-codec))]
           [max-ix (- (bvs:bytevector-length cus) 2)])
          (let loop ([n 0])
            (if (> n max-ix)
              (/ n 2)
              (begin
                (bvs:bytevector-u16-set!
                  v
                  0
                  (bvs:bytevector-u16-ref cus n (bvs:native-endianness))
                  (bvs:native-endianness))
                (if (p (string-ref (bytevector->string v tx) 0))
                  (loop (+ n 2))
                  (/ n 2)))))))))

  (define _indexOf
    (lambda (just)
      (lambda (nothing)
        (lambda (pattern)
          (lambda (s)
            (let ([i (srfi:152:string-contains s pattern)])
              (if (not i)
                nothing
                (just (length (srfi:152:string-take s i))))))))))

  (define _indexOfStartingAt
    (lambda (just)
      (lambda (nothing)
        (lambda (pattern)
          (lambda (startAt)
            (lambda (s)
              (if (or (> startAt (string-length s)) (< startAt 0))
                nothing
                (let ([i (srfi:152:string-contains s pattern startAt)])
                  (if i (just i) nothing)))))))))

  (define _lastIndexOf
    (lambda (just)
      (lambda (nothing)
        (lambda (pattern)
          (lambda (s)
            (let ([i (srfi:152:string-contains-right s pattern)])
              (if i (just i) nothing)))))))

  (define _lastIndexOfStartingAt
    (lambda (just)
      (lambda (nothing)
        (lambda (pattern)
          (lambda (startAt)
            (lambda (s)
              (let
                ([pattern-ix
                  (clamp-index-to-length s (+ startAt (string-length pattern)))])
                (let ([i (srfi:152:string-contains-right s pattern 0 pattern-ix)])
                  (if i (just i) nothing)))))))))

  (define take
    (lambda (n)
      (lambda (s)
          (srfi:152:string-take s (clamp-index-to-length s n)))))

  (define drop
    (lambda (n)
      (lambda (s)
          (srfi:152:string-drop s (clamp-index-to-length s n)))))

  (define slice
    (lambda (b)
      (lambda (e)
        (lambda (s)
          (let ([len (string-length s)])
            (let
              ([bn (if (>= b 0) b (+ len b))]
               [en (if (>= e 0) e (+ len e))])
              (if (> bn en)
                ""
                (srfi:152:substring s (max 0 bn) (min len en)))))))))

  (define splitAt
    (lambda (i)
      (lambda (s)
        (let ([ix (clamp-index-to-length s i)])
          (rt:make-object
            (cons "before" (srfi:152:string-take s ix))
            (cons "after" (srfi:152:string-drop s ix)))))))

  (define clamp-index-to-length
    (lambda (s n)
      (max 0 (min n (string-length s)))))
)
