(library (Data.String.Regex foreign)
  (export showRegexImpl
          regexImpl
          source
          flagsImpl
          test
          _match
          replace
          _replaceBy
          _search
          split
          )
  (import
    (except (chezscheme) length)
    (only (purs runtime bytestring) bytestring=?
                                    bytestring-singleton
                                    bytestring-slice
                                    bytestring-length-code-units
                                    string->bytestring
                                    bytestring->string
                                    bytestring-ref)
    (only (purs runtime irregex) string->irregex
                                 irregex-match-data?
                                 irregex-match
                                 irregex-match-substring
                                 irregex-match-num-submatches
                                 irregex-match-start-index
                                 irregex-match-valid-index?
                                 irregex-search
                                 irregex-replace
                                 irregex-replace/all
                                 irregex-split)
    (prefix (purs runtime srfi :125) srfi:125:)
    (prefix (purs runtime srfi :214) srfi:214:)
    (prefix (purs runtime lib) rt:))

  (define-structure (regex irregex source flags))

  ;; NOTE: this doesn't include the flags as they are not part of the regex string
  ;; in irregex PCRE strings.
  (define showRegexImpl regex-source)

  (define (flags->options flags)
    (define (flag->option flag)
      (cond
        [(bytestring=? flag (string->bytestring "ignoreCase")) 'i]
        [(bytestring=? flag (string->bytestring "multiline")) 'm]
        [(bytestring=? flag (string->bytestring "dotAll")) 's]
        [else #f]))

    (srfi:125:hash-table-fold
      (lambda (k v acc)
        (let ([o (flag->option k)])
          (if (and v o) (cons o acc) acc)))
      '()
      flags))

  (define regexImpl
    (lambda (left)
      (lambda (right)
        (lambda (s)
          (lambda (flags)
            (let ([options (flags->options flags)])
              (call/cc
                (lambda (k)
                  (with-exception-handler
                    (lambda (e) (k (left (condition-message e))))
                    (lambda () (right (make-regex (apply string->irregex (cons s options)) s flags))))))))))))

  (define source regex-source)

  (define flagsImpl regex-flags)

  (define test
    (lambda (r)
      (lambda (s)
        (let ([match (irregex-search (regex-irregex r) s)])
          (irregex-match-data? match)))))

  (define _match
    (lambda (just)
      (lambda (nothing)
        (lambda (r)
          (lambda (s)
            (let ([match (irregex-search (regex-irregex r) s)])
              (if (irregex-match-data? match)
                (let* ([num-matches (fx1+ (irregex-match-num-submatches match))]
                       [arr (srfi:214:make-flexvector num-matches)])
                  (let loop ([i 0])
                    (if (fx=? i num-matches)
                      (just arr)
                      (begin
                        (srfi:214:flexvector-set! arr i (just (irregex-match-substring match i)))
                        (loop (fx1+ i))))))
                nothing)))))))

  (define (submatches->flexvector just nothing match)
    (let* ([num-matches (irregex-match-num-submatches match)]
           [arr (srfi:214:make-flexvector num-matches)])
      (if (fx=? num-matches 0)
        arr
        (let loop ([n 0] [i 1])
          (if (fx=? n num-matches)
            arr
            (if (irregex-match-substring match i)
              (begin
                (srfi:214:flexvector-set! arr n (just (irregex-match-substring match i)))
                (loop (fx1+ n) (fx1+ i)))
              (begin
                (srfi:214:flexvector-set! arr n nothing)
                (loop (fx1+ n) (fx1+ i)))))))))

  (define replace
    (lambda (r)
      (lambda (replacement)
        (lambda (s)
          (if (srfi:125:hash-table-ref/default (regex-flags r) (string->bytestring "global") #f)
            (irregex-replace/all (regex-irregex r) s replacement)
            (irregex-replace (regex-irregex r) s replacement))))))

  (define _replaceBy
    (lambda (just)
      (lambda (nothing)
        (lambda (r)
          (lambda (f)
            (lambda (s)
              (irregex-replace
                (regex-irregex r)
                s
                (lambda (m)
                  ((f (irregex-match-substring m 0)) (submatches->flexvector just nothing m))))))))))

  (define _search
    (lambda (just)
      (lambda (nothing)
        (lambda (r)
          (lambda (s)
            (let ([match (irregex-search (regex-irregex r) s)])
              (if (irregex-match-data? match)
                (just (irregex-match-start-index match 0))
                nothing)))))))

  (define split
    (lambda (r)
      (lambda (s)
        (srfi:214:list->flexvector (irregex-split (regex-irregex r) s)))))


  )
