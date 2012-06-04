#lang typed/racket
(require "structures.rkt")
#|
(provide/contract
 [eliminate-whitespace (() ((listof symbol?) (boolean? . -> . boolean?)) . ->* . (element? . -> . element?))])
|#


;; eliminate-whitespace : (listof Symbol) (Bool -> Bool) -> Element -> Element
(: eliminate-whitespace
   (case-lambda [-> (Element -> Element)]
                [(Listof Symbol) -> Element -> Element]
                [(Listof Symbol) (Boolean -> Boolean) -> Element -> Element]))
(define eliminate-whitespace
  (opt-lambda: ([special : (Listof Symbol) empty]
                [eliminate-special? : (Boolean -> Boolean)
                                    (lambda: ([x : Boolean]) x)])
  (letrec: ([blank-it : (Element -> Element)
            (lambda: ([el : Element])
              (let: ([name : Symbol (element-name el)]
                     [content : (Listof Content)
                             (map (lambda: ([x : Content])
                                    (if (element? x) (blank-it x) x))
                                  (element-content el))])
                (make-element
                 (source-start el)
                 (source-stop el)
                 name
                 (element-attributes el)
                 (cond
                   [(eliminate-special? (and (memq (element-name el) special) #t))
                    (filter (lambda: ([s : Content])
                              (not (and (pcdata? s)
                                        (or (all-blank (pcdata-string s))
                                            (error 'eliminate-blanks "Element <~a> is not allowed to contain text ~e" name (pcdata-string s))))))
                            content)]
                   [else content]))))])
    blank-it)))

;; all-blank : String -> Bool
(: all-blank (String -> Boolean))
(define (all-blank s) (andmap char-whitespace? (string->list s)))


(provide eliminate-whitespace)
