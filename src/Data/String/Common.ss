;; -*- mode: scheme -*-

(library (Data.String.Common foreign)
  (export _localeCompare replace replaceAll split toLower toUpper trim joinWith)
  (import
    (only (rnrs base) define lambda)
    (only (rnrs conditions) make-message-condition)
    (only (rnrs exceptions) raise)
    (only (rnrs unicode) string-downcase string-upcase)
    (prefix (purs runtime srfi :152) srfi:152:)
    (prefix (purs runtime srfi :214) srfi:214:)
    (prefix (purs runtime irregex irregex) irregex:)
    (prefix (purs runtime irregex irregex-utils) irregex-utils:))

  (define _localeCompare
    (lambda (lt)
      (lambda (eq)
        (lambda (gt)
          (lambda (s1)
            (lambda (s2)
              (raise (make-message-condition "Data.String.Common._localeCompare: not implemented"))))))))

  (define replace
    (lambda (pattern)
      (lambda (replacement)
        (lambda (target)
          (irregex:irregex-replace
            (irregex-utils:irregex-quote pattern)
            target
            replacement)))))

  (define replaceAll
    (lambda (pattern)
      (lambda (replacement)
        (lambda (target)
          (irregex:irregex-replace/all
            (irregex-utils:irregex-quote pattern)
            target
            replacement)))))

  (define split
    (lambda (sep)
      (lambda (s)
        (srfi:214:list->flexvector
          (srfi:152:string-split s sep)))))

  (define toLower string-downcase)

  (define toUpper string-upcase)

  (define trim srfi:152:string-trim-both)

  (define joinWith
    (lambda (sep)
      (lambda (xs)
        (srfi:152:string-join
          (srfi:214:flexvector->list xs)
          sep))))
)

