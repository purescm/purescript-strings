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
    (only (rnrs base) + - * / < = > >= and begin car cdr cond cons define else equal? if lambda let let* list->string max min not or pair? string string-length string->list string-ref)
    (only (chezscheme) reverse!)
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
            (((((_indexOfStartingAt just) nothing) pattern) 0) s))))))

  (define _indexOfStartingAt
    (lambda (just)
      (lambda (nothing)
        (lambda (pattern)
          (lambda (startAt)
            (lambda (s)
              (if (or (< startAt 0) (> startAt (length s)))
                nothing
                (let
                  ([s-after-start (string->u16-uint-list ((drop startAt) s))]
                   [pattern-bytes (string->u16-uint-list pattern)]
                   [pattern-length (length pattern)])
                  (let go ([ix startAt]
                           [found-first #f]
                           [pat pattern-bytes]
                           [s-tail s-after-start])
                    (cond
                      ((and found-first (pair? pat) (pair? s-tail))
                        (if (equal? (car pat) (car s-tail))
                          (go (+ ix 1) #t (cdr pat) (cdr s-tail))
                          (go (+ ix 1) #f pattern-bytes (cdr s-tail))))
                      ((and found-first (pair? pat))
                        nothing)
                      (found-first
                        (just (- ix pattern-length)))
                      ((and (pair? pat) (pair? s-tail))
                        (if (equal? (car pat) (car s-tail))
                          (go (+ ix 1) #t (cdr pat) (cdr s-tail))
                          (go (+ ix 1) #f pattern-bytes (cdr s-tail))))
                      ((pair? pat)
                        nothing)
                      (else
                        (just (- ix pattern-length)))))))))))))

  (define _lastIndexOf
    (lambda (just)
      (lambda (nothing)
        (lambda (pattern)
          (lambda (s)
            (((((_lastIndexOfStartingAt just) nothing) pattern) (length s)) s))))))

  (define _lastIndexOfStartingAt
    (lambda (just)
      (lambda (nothing)
        (lambda (pattern)
          (lambda (startAt)
            (lambda (s)
              (if (< startAt 0)
                (let ([pat-at-ix-0 ((take (length pattern)) s)])
                  (if (equal? pattern pat-at-ix-0) (just 0) nothing))
                (let*
                  ([s-len (length s)]
                   [start-at (min s-len (+ startAt (length pattern)))]
                   [s-bytes (reverse! (string->u16-uint-list ((take start-at) s)))]
                   [pattern-bytes (reverse! (string->u16-uint-list pattern))])
                  (let go ([ix start-at]
                           [found-first #f]
                           [pat pattern-bytes]
                           [s-tail s-bytes])
                    (cond
                      ((and found-first (pair? pat) (pair? s-tail))
                        (if (equal? (car pat) (car s-tail))
                          (go (- ix 1) #t (cdr pat) (cdr s-tail))
                          (go (- ix 1) #f pattern-bytes (cdr s-tail))))
                      ((and found-first (pair? pat))
                        nothing)
                      (found-first
                        (just ix))
                      ((and (pair? pat) (pair? s-tail))
                        (if (equal? (car pat) (car s-tail))
                          (go (- ix 1) #t (cdr pat) (cdr s-tail))
                          (go (- ix 1) #f pattern-bytes (cdr s-tail))))
                      ((pair? pat)
                        nothing)
                      (else
                        (just ix))))))))))))

  (define string->u16-uint-list
    (lambda (s)
      (bvs:bytevector->uint-list
        (bvs:string->utf16 s)
        (bvs:native-endianness)
        2)))

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
