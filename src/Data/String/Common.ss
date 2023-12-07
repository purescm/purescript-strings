;; -*- mode: scheme -*-
(library (Data.String.Common foreign)
  (export _localeCompare replace replaceAll split toLower toUpper trim joinWith)
  (import
    (only (rnrs base) define lambda if)
    (only (purs runtime bytestring) bytestring<? bytestring=?
                                    bytestring-replace bytestring-replace-all
                                    bytestring-trim
                                    bytestring-split
                                    bytestring-downcase
                                    bytestring-upcase
                                    bytestring-join-with))

  ;; TODO This is the same as `Ord`
  (define _localeCompare
    (lambda (lt)
      (lambda (eq)
        (lambda (gt)
          (lambda (s1)
            (lambda (s2)
              (if (bytestring<? s1 s2)
                  lt
                  (if (bytestring=? s1 s2) eq gt))))))))

  (define replace
    (lambda (pattern)
      (lambda (replacement)
        (lambda (target)
          (bytestring-replace target pattern replacement)))))

  (define replaceAll
    (lambda (pattern)
      (lambda (replacement)
        (lambda (target)
          (bytestring-replace-all target pattern replacement)))))

  (define split
    (lambda (sep)
      (lambda (s)
        (bytestring-split s sep))))

  (define toLower bytestring-downcase)

  (define toUpper bytestring-upcase)

  (define trim bytestring-trim)

  (define joinWith
    (lambda (sep)
      (lambda (xs)
        (bytestring-join-with xs sep))))
)
