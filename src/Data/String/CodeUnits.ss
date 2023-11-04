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
    (only (rnrs base) + - * / < = > >= begin cons define if lambda let let* max min not or string string-length string-ref)
    (prefix (rnrs bytevectors) bvs:)
    (only (rnrs io ports) bytevector->string make-transcoder utf-16-codec)
    (prefix (purs runtime lib) rt:)
    (prefix (purs runtime srfi :1) srfi:1:)
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

  (define take
    (lambda (n)
      (lambda (s)
        (let*
          ([cus (bvs:bytevector->uint-list (bvs:string->utf16 s) (bvs:native-endianness) 2)]
           [tx (make-transcoder (utf-16-codec))]
           [to-take (max 0 (min n (length s)))])
          (bytevector->string
            (bvs:uint-list->bytevector
              (srfi:1:take cus to-take)
              (bvs:native-endianness)
              2)
            tx)))))

  (define drop
    (lambda (n)
      (lambda (s)
        (let*
          ([cus (bvs:bytevector->uint-list (bvs:string->utf16 s) (bvs:native-endianness) 2)]
           [tx (make-transcoder (utf-16-codec))]
           [to-drop (max 0 (min n (length s)))])
          (bytevector->string
            (bvs:uint-list->bytevector
              (srfi:1:drop cus to-drop)
              (bvs:native-endianness)
              2)
            tx)))))

  (define _indexOf
    (lambda (just)
      (lambda (nothing)
        (lambda (pattern)
          (lambda (s)
            (let ([i (srfi:152:string-contains s pattern)])
              (if (not i)
                nothing
                (just (length ((take i) s))))))))))

  (define _indexOfStartingAt
    (lambda (just)
      (lambda (nothing)
        (lambda (pattern)
          (lambda (startAt)
            (lambda (s)
              (if (or (< startAt 0) (> startAt (length s)))
                nothing
                (let*
                  ([s-after-start ((drop startAt) s)]
                   [i (srfi:152:string-contains s-after-start pattern)])
                  (if (not i)
                    nothing
                    (just
                      (+
                        (length ((take startAt) s))
                        (length ((take i) s-after-start)))))))))))))

  (define _lastIndexOf
    (lambda (just)
      (lambda (nothing)
        (lambda (pattern)
          (lambda (s)
            (let ([i (srfi:152:string-contains-right s pattern)])
              (if (not i)
                nothing
                (just (length ((take i) s))))))))))

  (define _lastIndexOfStartingAt
    (lambda (just)
      (lambda (nothing)
        (lambda (pattern)
          (lambda (startAt)
            (lambda (s)
              (let*
                ([pattern-ix
                  (max 0 (min (string-length s) (+ startAt (string-length pattern))))]
                 [i (srfi:152:string-contains-right s pattern 0 pattern-ix)])
                (if (not i)
                  nothing
                  (just (length ((take i) s)))))))))))

  (define slice
    (lambda (b)
      (lambda (e)
        (lambda (s)
          (let*
            ([len (length s)]
             [bn (if (>= b 0) b (+ len b))]
             [en (if (>= e 0) e (+ len e))])
            (if (> bn en)
              ""
              ((take (- (min len en) (max 0 bn))) ((drop (max 0 bn)) s))))))))

  (define splitAt
    (lambda (i)
      (lambda (s)
        (let ([ix (max 0 (min (length s) i))])
          (rt:make-object
            (cons "before" ((take ix) s))
            (cons "after" ((drop ix) s)))))))

)
