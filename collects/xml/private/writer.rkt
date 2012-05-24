#lang typed/racket
(require "structures.rkt")

;; TODO: provide type-restricted versions of these functions
;;       that demand an output port?

#|
(provide/contract
 [write-xml ((document?) (output-port?) . ->* . void?)]
 [display-xml ((document?) (output-port?) . ->* . void?)]
 [write-xml/content ((content/c) (output-port?) . ->* . void?)]
 [display-xml/content ((content/c) (output-port?) . ->* . void?)]
 [empty-tag-shorthand (parameter/c (or/c (symbols 'always 'never) (listof symbol?)))]
 [html-empty-tags (listof symbol?)])
|#

;; This pattern shows up a lot, so let's name it something sensible
(define-type (Xml-Writer A)
  (A Natural (Natural Output-Port -> Void) Output-Port
     -> Void))


(: html-empty-tags (Listof Symbol))
(define html-empty-tags
  '(param meta link isindex input img hr frame col br basefont base area))

;; (empty-tag-shorthand) : (U 'always 'never (listof Symbol))
(: empty-tag-shorthand (Parameterof Any (U 'always 'never (Listof Symbol))))
(define empty-tag-shorthand
  (make-parameter 
   html-empty-tags
   (lambda (x)
     (if (or (eq? x 'always) (eq? x 'never) (and (list? x) (andmap symbol? x)))
         x
         (error 'empty-tag-shorthand "expected 'always, 'never, or a list of symbols: received ~e" x)))))

;; gen-write/display-xml/content : (Nat Output-port -> Void) -> Content [Output-Port]-> Void
(: gen-write/display-xml/content
   ((Natural Output-Port -> Void)
    -> (case-lambda (Content Output-Port -> Void)
                    (Content -> Void))))
(define (gen-write/display-xml/content dent)
  (opt-lambda: ([c : Content] [out : Output-Port (current-output-port)])
    (write-xml-content c 0 dent out)))

;; indent : Nat Output-port -> Void
(: indent (Natural Output-Port -> Void))
(define (indent n out)
  (newline out)
  (let loop ([n n])
    (unless (zero? n)
      (display #\space out)
      (loop (sub1 n)))))

;; write-xml/content : Content [Output-port] -> Void
(: write-xml/content
   (case-lambda (Content -> Void)
                (Content Output-Port -> Void)))
(define write-xml/content (gen-write/display-xml/content void))

;; display-xml/content : Content [Output-port] -> Void
(: display-xml/content (case-lambda (Content -> Void)
                                    (Content Output-Port -> Void)))
(define display-xml/content (gen-write/display-xml/content indent))

;; gen-write/display-xml : (Content [Output-port] -> Void) -> Document [Output-port] -> Void
(: gen-write/display-xml
   ((Content Output-Port -> Void)
    -> (case-lambda
         (Document -> Void)
         (Document Output-Port -> Void))))
(define (gen-write/display-xml output-content)
  (opt-lambda: ([doc : Document] [out : Output-Port (current-output-port)])
    (let ([prolog (document-prolog doc)])
      (display-outside-misc (prolog-misc prolog) out)
      (display-dtd (prolog-dtd prolog) out)
      (display-outside-misc (prolog-misc2 prolog) out))
    (output-content (document-element doc) out)
    (display-outside-misc (document-misc doc) out)))

; display-dtd : document-type oport -> void
(: display-dtd (Document-Type Output-Port -> Void))
(define (display-dtd dtd out)
  (when dtd
    (fprintf out "<!DOCTYPE ~a" (document-type-name dtd))
    (let ([external (document-type-external dtd)])
      (cond
        [(external-dtd/public? external)
         (fprintf out " PUBLIC \"~a\" \"~a\""
                  (external-dtd/public-public external)
                  (external-dtd-system external))]
        [(external-dtd/system? external)
         (fprintf out " SYSTEM \"~a\"" (external-dtd-system external))]
        [(not external) (void)]))
    (display ">" out)
    (newline out)))

;; write-xml : Document [Output-port] -> Void
(: write-xml (case-lambda (Document -> Void) (Document Output-Port -> Void)))
(define write-xml (gen-write/display-xml write-xml/content))

;; display-xml : Document [Output-port] -> Void
(: display-xml (case-lambda (Document -> Void) (Document Output-Port -> Void)))
(define display-xml (gen-write/display-xml display-xml/content))

;; display-outside-misc : (listof Misc) Output-port -> Void
(: display-outside-misc ((Listof Misc) Output-Port -> Void))
(define (display-outside-misc misc out)
  (for-each (lambda (x)
              (define: (A) (use-writer [f : (Xml-Writer A)] [a : A]) : Void
                (f a 0 void out))
              (cond
                 [(comment? x) (use-writer write-xml-comment x)]
                 [(p-i? x) (use-writer write-xml-p-i x)])
              (newline out))
            misc))


;; write-xml-content : Content Nat (Nat Output-Stream -> Void) Output-Stream -> Void
(: write-xml-content (Xml-Writer Content))
(define (write-xml-content el over dent out)
  (define: (A) (use-writer [f : (Xml-Writer A)] [a : A]) : Void
                (f a over dent out))
  (cond
     [(element? el) (use-writer write-xml-element el)]
     [(pcdata? el) (use-writer write-xml-pcdata el)]
     [(cdata? el) (use-writer write-xml-cdata el)]
     [(entity? el) (use-writer write-xml-entity el)]
     [(comment? el) (use-writer write-xml-comment el)]
     [(p-i? el) (use-writer write-xml-p-i el)]
     [else (error 'write-xml-content "received ~e" el)]))

;; write-xml-element : Element Nat (Nat Output-Stream -> Void) Output-Stream -> Void
(: write-xml-element (Xml-Writer Element))
(define (write-xml-element el over dent out)
  (let* ([name (element-name el)]
         [start (lambda: ([str : String])
                  (write-xml-base str over dent out)
                  (display name out))]
         [content (element-content el)])
    (start "<")
    (for ([att (in-list (element-attributes el))])
      (fprintf out " ~a=\"~a\"" (attribute-name att)
               (escape (attribute-value att) escape-attribute-table)))
    (if (and (null? content)
             (let ([short (empty-tag-shorthand)])
               (cond
                 [(symbol? short)
                  (case short
                    [(always) #t]
                    [(never) #f])]
                 [else (memq (lowercase-symbol name) short)])))
        (display " />" out)
        (begin
          (display ">" out)
          (for ([c (in-list content)])
            (write-xml-content c (incr over) dent out))
          (start "</")
          (display ">" out)))))

; : sym -> sym
(: lowercases (HashTable Symbol Symbol))
(define lowercases (make-weak-hash))
(: lowercase-symbol (Symbol -> Symbol))
(define (lowercase-symbol x)
  (or (hash-ref lowercases x (lambda () #f))
      (let ([s (symbol->string x)])
        (let ([s (string->symbol (string-downcase s))])
          (hash-set! lowercases x s)
          s))))

;; write-xml-base : (U String Char Symbol) Nat (Nat Output-Stream -> Void) Output-Stream -> Void
(: write-xml-base (Xml-Writer (U String Char Symbol)))
(define (write-xml-base el over dent out)
  (dent over out)
  (display el out))

;; write-xml-pcdata : Pcdata Nat (Nat Output-Stream -> Void) Output-Stream -> Void
(: write-xml-pcdata (Xml-Writer Pcdata))
(define (write-xml-pcdata str over dent out)
  (write-xml-base (escape (pcdata-string str) escape-table) over dent out))

;; write-xml-cdata : Cdata Nat (Nat Output-Stream -> Void) Output-Stream -> Void
(: write-xml-cdata (Xml-Writer Cdata))
(define (write-xml-cdata cdata over dent out)
  ;; XXX: Different kind of quote is needed, for assume the user includes the <![CDATA[...]]> with proper quoting
  (write-xml-base (format "~a" (cdata-string cdata)) over dent out))

;; write-xml-p-i : Processing-instruction Nat (Nat Output-Stream -> Void) Output-Stream -> Void
(: write-xml-p-i (Xml-Writer Processing-Instruction))
(define (write-xml-p-i p-i over dent out)
  (write-xml-base (format "<?~a ~a?>" (p-i-target-name p-i) (p-i-instruction p-i)) over dent out))

;; write-xml-comment : Comment Nat (Nat Output-Stream -> Void) Output-Stream -> Void
(: write-xml-comment (Xml-Writer Comment))
(define (write-xml-comment comment over dent out)
  (write-xml-base (format "<!--~a-->" (comment-text comment)) over dent out))

;; write-xml-entity : Entity Nat (Nat Output-stream -> Void) Output-stream -> Void
(: write-xml-entity (Xml-Writer Entity))
(define (write-xml-entity entity over dent out)
  (let ([n (entity-text entity)])
    (fprintf out (if (number? n) "&#~a;" "&~a;") n)))

(: escape-table Regexp)
(define escape-table #rx"[<>&]")
(: escape-table Regexp)
(define escape-attribute-table #rx"[<>&\"]")

(: replace-escaped (String String * -> String))
(define: (replace-escaped [s : String] s-rest : String *) : String
  (case (string-ref s 0)
    [(#\<) "&lt;"]
    [(#\>) "&gt;"]
    [(#\&) "&amp;"]
    [(#\") "&quot;"]
    [else (error "unexpected string escape replacement demanded")]))

;; escape : String -> String
(: escape (String Regexp -> String))
(define (escape x table)
  (regexp-replace* table x replace-escaped))


(provide escape
         escape-table
         escape-attribute-table
         lowercase-symbol
         write-xml-cdata
         write-xml-comment
         write-xml-p-i
         write-xml-element
         write-xml
         display-xml
         write-xml/content
         display-xml/content
         empty-tag-shorthand
         html-empty-tags)


;; incr : Nat -> Nat
(: incr (Natural -> Natural))
(define (incr n) (+ n 2))
