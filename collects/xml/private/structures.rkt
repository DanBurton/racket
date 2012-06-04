#lang typed/racket/base

; Location = (make-location Nat Nat Nat) | Symbol ;;; | #f
(define-struct: location
  ([line : Natural]
   [char : Natural]
   [offset : Natural])
  #:transparent)
(define-type Location (U location Symbol #f))
(define-predicate location/c Location)

(: loc-fun-opt (All (A) ((location -> A) -> (Location -> (Option A)))))
(define (loc-fun-opt f)
  (lambda (x)
    (cond [(location? x) (f x)]
          [else #f])))

(: location-line-opt (Location -> (Option Natural)))
(define location-line-opt (loc-fun-opt location-line))

(: location-char-opt (Location -> (Option Natural)))
(define location-char-opt (loc-fun-opt location-char))

(: location-offset-opt (Location -> (Option Natural)))
(define location-offset-opt (loc-fun-opt location-offset))


; Source = (make-source Location Location)
(define-struct: source
  ([start : Location]
   [stop : Location])
  #:transparent)
(define-type Source source)

; Document = (make-document Prolog Element (listof Misc))
(define-struct: document
  ([prolog : Prolog]
   [element : Element]
   [misc : (Listof Misc)])
  #:transparent)
(define-type Document document)

; Prolog = (make-prolog (listof Misc) Document-type (listof Misc))
(define-struct: prolog
  ([misc : (Listof Misc)]
   [dtd : Document-Type]
   [misc2 : (Listof Misc)])
  #:transparent)
(define-type Prolog prolog)

; Document-type = (make-document-type sym External-dtd #f)
;               | #f
(define-struct: document-type
  ([name : Symbol]
   [external : External-Dtd]
   [inlined : #f]) ;;; Boolean or #f?
  #:transparent)
(define-type Document-Type (Option document-type))

; External-dtd = (make-external-dtd/public str str)
;              | (make-external-dtd/system str)
;              | #f
(define-struct: external-dtd
  ([system : String])
  #:transparent)
(define-struct: (external-dtd/public external-dtd)
  ([public : String])
  #:transparent)
(define-struct:
  (external-dtd/system external-dtd)
  ()
  #:transparent)
(define-type External-Dtd external-dtd)

; Element = (make-element Location Location Symbol (listof Attribute) (listof Content))
(define-struct: (element source)
  ([name : Symbol]
   [attributes : (Listof Attribute)]
   [content : (Listof Content)])
  #:transparent)
(define-type Element element)

; Attribute = (make-attribute Location Location Symbol String)
(define-struct: (attribute source)
  ([name : Symbol]
   [value : String])
  #:transparent)
(define-type Attribute attribute)

; Pcdata = (make-pcdata Location Location String)
(define-struct: (pcdata source)
  ([string : String])
  #:transparent)
(define-type Pcdata pcdata)

; Cdata = (make-cdata Location Location String)
(define-struct: (cdata source)
  ([string : String])
  #:transparent)
(define-type Cdata cdata)

; Content = Pcdata  
;         |  Element
;         |  Entity
;         |  Misc
;         |  Cdata
(define-type Content (U Pcdata Element Entity Misc Cdata))
(define-predicate content/c Content)

; Misc = Comment
;      |  Processing-instruction
(define-type Misc (U Comment Processing-Instruction))
(define-predicate misc/c Misc)

; Section 2.2 of XML 1.1
; (XML 1.0 is slightly different and looks less restrictive)
(: valid-char? (Any -> Boolean)) ;;; TODO: add witness that True means Char.
(define (valid-char? i)
  (and (exact-nonnegative-integer? i)
       (or (= i #x9)
           (= i #xA)
           (= i #xD)
           (<= #x20 i #xD7FF)
           (<= #xE000 i #xFFFD)
           (<= #x10000 i #x10FFFF))))
(define-type Valid-Char Char)

; Entity = (make-entity Location Location (U Nat Symbol))
(define-struct: (entity source)
  ([text : (U Symbol Number)]) ;;; Not just any Nat
  #:transparent)
(define-type Entity entity)

; Processing-instruction = (make-p-i Location Location String String)
; also represents XMLDecl
(define-struct: (p-i source)
  ([target-name : Symbol] ;;; Comment above is wrong?
   [instruction : String])
  #:transparent)
(define-type Processing-Instruction p-i)

; Comment = (make-comment String)
(define-struct: (comment source)
  ([text : String])
  #:transparent)
(define-type Comment comment)


(provide
 (struct-out location) Location location/c
 location-line-opt
 location-char-opt
 location-offset-opt
 (struct-out source) Source
 (struct-out external-dtd) External-Dtd
 (struct-out external-dtd/public)
 (struct-out external-dtd/system)
 (struct-out document-type) Document-Type
 (struct-out comment) Comment
 (struct-out p-i) Processing-Instruction
 Misc misc/c
 (struct-out prolog) Prolog
 (struct-out document) Document
 (struct-out element) Element
 (struct-out attribute) Attribute
 Content content/c
 (struct-out pcdata) Pcdata
 (struct-out cdata) Cdata
 valid-char? Valid-Char
 (struct-out entity) Entity)
