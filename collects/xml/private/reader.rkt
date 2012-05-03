#lang typed/racket
(require "structures.rkt")
#|
(provide/contract
 [read-xml (() (input-port?) . ->* . document?)]
 [read-xml/document (() (input-port?) . ->* . document?)]
 [read-xml/element (() (input-port?) . ->* . element?)]
 [read-comments (parameter/c boolean?)]
 [collapse-whitespace (parameter/c boolean?)]
 [exn:xml? (any/c . -> . boolean?)])
|#

(provide
 read-xml
 read-xml/document
 read-xml/element
 read-comments
 collapse-whitespace
 exn:xml?)

;; Start-tag ::= (make-start-tag Location Location Symbol (listof Attribute))
(define-struct: (start-tag source)
  ([name : Symbol]
   [attrs : (Listof Attribute)]))
(define-type Start-Tag start-tag)

;; End-tag ::= (make-end-tag Location Location Symbol)
(define-struct: (end-tag source)
  ([name : Symbol]))
(define-type End-Tag end-tag)

;; Token ::= Contents | Start-tag | End-tag | Eof
(define-type Token (U Content Start-Tag End-Tag Eof))

(: read-comments (Parameter Boolean))
(define read-comments (make-parameter #f))
(: collapse-whitespace (Parameter Boolean))
(define collapse-whitespace (make-parameter #f))

(define-type (function-with-default a b)
  (case-> (-> b) (a -> b)))

(define: (subtract-opt [x : (Option Integer)] [y : (Option Integer)]) : (Option Integer)
  (cond [(and x y) (- x y)]
        [else #f]))

;; read-xml : [Input-port] -> Document
(: read-xml (function-with-default Input-Port Document))
(define read-xml
  (opt-lambda: ([in : Input-Port (current-input-port)])
    (let*-values ([(in pos) (positionify in)]
                  [(misc0 start) (read-misc in pos)])
      (make-document (make-prolog misc0 #f empty)
                     (read-xml-element-helper pos in start)
                     (let-values ([(misc1 end-of-file) (read-misc in pos)])
                       (unless (Eof? end-of-file)
                         (parse-error (list
                                       (make-srcloc
                                        (object-name in)
                                        #f
                                        #f
                                        (location-offset-opt (source-start end-of-file))
                                        (subtract-opt
                                         (location-offset-opt (source-stop end-of-file))
                                         (location-offset-opt (source-start end-of-file)))))
                                      "extra stuff at end of document ~e"
                                      end-of-file))
                       misc1)))))

;; read-xml : [Input-port] -> Document
(: read-xml/document (function-with-default Input-Port Document))
(define read-xml/document
  (opt-lambda: ([in : Input-Port (current-input-port)])
    (let*-values: ([([in : Input-Port] [pos : (-> Location)]) (positionify in)]
                   [([misc0 : (Listof Misc)] [start : Token]) (read-misc in pos)])
      (make-document (make-prolog misc0 #f empty)
                     (read-xml-element-helper pos in start)
                     empty))))

;; read-xml/element : [Input-port] -> Element
(: read-xml/element (function-with-default Input-Port Element))
(define read-xml/element
  (opt-lambda: ([in : Input-Port (current-input-port)])
    (let-values ([(in pos) (positionify in)])
      (skip-space in)
      (read-xml-element-helper pos in (lex in pos)))))

;; read-xml-element-helper : Nat Iport Token -> Element
(: read-xml-element-helper ((-> Location) Input-Port Token -> Element))
(define (read-xml-element-helper pos in start)
  (cond
    [(start-tag? start) (read-element start in pos)]
    [(element? start) start]
    [else 
     (parse-error
      (list
       (make-srcloc
        (object-name in)
        #f
        #f
        ; XXX Some data structures should really be changed to be sources
        (if (source? start)
            (location-offset-opt (source-start start))
            #f)
        (if (source? start)
            (subtract-opt
             (location-offset-opt (source-stop start))
             (location-offset-opt (source-start start)))
            #f)))
      "expected root element - received ~e"
      (cond
        [(pcdata? start) (pcdata-string start)]
        [(Eof? start) eof]
        [else start]))]))

;; read-misc : Input-port (-> Location) -> (listof Misc) Token
(: read-misc (Input-Port (-> Location) -> (Values (Listof Misc) Token)))
(define (read-misc in pos)
  (let read-more ()
    (let ([x (lex in pos)])
      (cond
        [(p-i? x)
         (let-values ([(lst next) (read-more)])
           (values (cons x lst) next))]
        [(comment? x)
         (let-values ([(lst next) (read-more)])
           (if (read-comments)
               (values (cons x lst) next)
               (values lst next)))]
        [(and (pcdata? x) (andmap char-whitespace? (string->list (pcdata-string x))))
         (read-more)]
        [else (values null x)]))))

;; read-element : Start-tag Input-port (-> Location) -> Element
(: read-element (Start-Tag Input-Port (-> Location) -> Element))
(define (read-element start in pos)
  (let: ([name : Symbol (start-tag-name start)]
         [a : Location (source-start start)]
         [b : Location (source-stop start)])
    (let: read-content : Element
      ([k : ((Listof Content) Location -> Element)
          (lambda (body end-loc)
            (make-element
             a end-loc name (start-tag-attrs start)
             body))])
      (let ([x (lex in pos)])
        (cond
          [(Eof? x)
           (parse-error (list
                         (make-srcloc
                          (object-name in)
                          #f
                          #f
                          (location-offset-opt (source-start start))
                          (subtract-opt
                           (location-offset-opt (source-stop start))
                           (location-offset-opt (source-start start)))))
                        "unclosed `~a' tag at [~a ~a]"
                        name
                        (format-source a)
                        (format-source b))]
          [(start-tag? x)
           (let ([next-el (read-element x in pos)])
             (read-content (lambda (body end-loc)
                             (k (cons next-el body)
                                end-loc))))]
          [(end-tag? x)
           (let ([end-loc (source-stop x)])
             (unless (eq? name (end-tag-name x))
               (parse-error
                (list
                 (make-srcloc (object-name in)
                              #f
                              #f
                              (location-offset-opt a)
                              (subtract-opt
                               (location-offset-opt b)
                               (location-offset-opt a)))
                 (make-srcloc (object-name in)
                              #f
                              #f
                              (location-offset-opt (source-start x))
                              (subtract-opt
                               (location-offset-opt end-loc)
                               (location-offset-opt (source-start x)))))
                "start tag `~a' at [~a ~a] doesn't match end tag `~a' at [~a ~a]"
                name
                (format-source a)
                (format-source b)
                (end-tag-name x)
                (format-source (source-start x))
                (format-source end-loc)))
             (k null end-loc))]
          [(entity? x) (read-content (lambda (body end-loc)
                                       (k (cons (expand-entity x) body)
                                          end-loc)))]
          [(comment? x) (if (read-comments)
                            (read-content (lambda (body end-loc) (k (cons x body) end-loc)))
                            (read-content k))]
          [else (read-content (lambda (body end-loc) (k (cons x body) end-loc)))])))))


;; expand-entity : Entity -> (U Entity Pcdata)
;; more here - allow expansion of user defined entities
(: expand-entity (Entity -> (U Entity Pcdata)))
(define (expand-entity x)
  (let ([expanded (default-entity-table (entity-text x))])
    (if expanded
        (make-pcdata (source-start x) (source-stop x) expanded)
        x)))


;; default-entity-table : Symbol -> (U #f String)
(: default-entity-table (Any -> (Option String)))
(define (default-entity-table name)
  (case name
    [(amp) "&"]
    [(lt) "<"]
    [(gt) ">"]
    [(quot) "\""]
    [(apos) "'"]
    [else #f]))

(define-struct: (Eof source) ())


;; lex : Input-port (-> Location) -> (U Token special)
(: lex (Input-Port (-> Location) -> Token))
(define (lex in pos)
  (let ([c (peek-char in)])
    (cond
      [(eof-object? c) (read-char in) (Eof (pos) (pos))]
      [(eq? c #\&) (lex-entity in pos)]
      [(eq? c #\<) (lex-tag-cdata-pi-comment in pos)]
      ;[(not (char? c)) (read-char in)]
      [else (lex-pcdata in pos)])))

; lex-entity : Input-port (-> Location) -> Entity
; pre: the first char is a #\&
(: lex-entity (Input-Port (-> Location) -> Entity))
(define (lex-entity in pos)
  (let ([start (pos)])
    (read-char in)
    (let ([data (case (peek-char in)
                  [(#\#)
                   (read-char in)
                   (let: ([n : (Option Number) (case (peek-char in)
                              [(#\x) (read-char in)
                                     (string->number (read-until #\; in pos) 16)]
                              [else (string->number (read-until #\; in pos))])])
                     (cond
                       [(not (number? n)) 
                        (lex-error in pos "malformed numeric entity")]
                       [(not (valid-char? n))
                        (lex-error in pos "not a well-formed numeric entity (does not match the production for Char, see XML 4.1)")]
                       [else n]))]
                  [else
                   (begin0
                     (lex-name in pos)
                     (unless (eq? (read-char in) #\;)
                       (lex-error in pos "expected ; at the end of an entity")))])])
      (make-entity start (pos) data))))

; lex-tag-cdata-pi-comment : Input-port (-> Location) -> Start-tag | Element | End-tag | Cdata | p-i | Comment
; pre: the first char is a #\<
(: lex-tag-cdata-pi-comment (Input-Port (-> Location) -> Token))
(define (lex-tag-cdata-pi-comment in pos)
  (let ([start (pos)])
    (read-char in)
    (case (non-eof peek-char in pos)
      [(#\!)
       (read-char in)
       (case (non-eof peek-char in pos)
         [(#\-) (read-char in)
                (unless (eq? (read-char-or-special in) #\-)
                  (lex-error in pos "expected second - after <!-"))
                (let ([data (lex-comment-contents in pos)])
                  (unless (eq? (read-char in) #\>)
                    (lex-error in pos "expected > to end comment (\"--\" can't appear in comments)"))
                  (make-comment start (pos) data))]
                  ;(make-comment data))]
         [(#\[) (read-char in)
                (define: s : (U String EOF) (read-string 6 in))
                (unless (and (string? s) (string=? s "CDATA["))
                  (lex-error in pos "expected CDATA following <["))
                (let ([data (lex-cdata-contents in pos)])
                  (make-cdata start (pos) (format "<![CDATA[~a]]>" data)))]
         [else (skip-dtd in pos)
               (skip-space in)
               (unless (eq? (peek-char-or-special in) #\<)
                 (lex-error in pos "expected p-i, comment, or element after doctype"))
               (lex-tag-cdata-pi-comment in pos)])]
      [(#\?) (read-char in)
             (let ([name (lex-name in pos)])
               (skip-space in)
               (let ([data (lex-pi-data in pos)])
                 (make-p-i start (pos) name data)))]
      [(#\/) (read-char in)
             (let ([name (lex-name in pos)])
               (skip-space in)
               (unless (eq? (read-char-or-special in) #\>)
                 (lex-error in pos "expected > to close ~a's end tag" name))
               (make-end-tag start (pos) name))]
      [else ; includes 'special, but lex-name will fail in that case
       (let ([name (lex-name in pos)]
             [attrs (lex-attributes in pos)])
         (skip-space in)
         (case (read-char-or-special in)
           [(#\/)
            (unless (eq? (read-char in) #\>)
              (lex-error in pos "expected > to close empty element ~a" name))
            (make-element start (pos) name attrs null)]
           [(#\>) (make-start-tag start (pos) name attrs)]
           [else (lex-error in pos "expected / or > to close tag `~a'" name)]))])))

;; lex-attributes : Input-port (-> Location) -> (listof Attribute)
(: lex-attributes (Input-Port (-> Location) -> (Listof Attribute)))
(define (lex-attributes in pos)
  ((inst sort Attribute Nothing) 
   (let: loop : (Listof Attribute) ()
     (skip-space in)
     (define: c : (U Char EOF) (peek-char in))
     (cond [(and (char? c) (name-start? c))
            (cons (lex-attribute in pos) (loop))]
           [else null]))
   (lambda: ([a : Attribute] [b : Attribute])
     (let ([na (attribute-name a)]
           [nb (attribute-name b)])
       (cond
         [(eq? na nb) (lex-error in pos "duplicated attribute name ~a" na)]
         [else (string<? (symbol->string na) (symbol->string nb))])))))

;; lex-attribute : Input-port (-> Location) -> Attribute
(: lex-attribute (Input-Port (-> Location) -> Attribute))
(define (lex-attribute in pos)
  (let ([start (pos)]
        [name (lex-name in pos)])
    (skip-space in)
    (unless (eq? (read-char in) #\=)
      (lex-error in pos "expected = in attribute ~a" name))
    (skip-space in)
    ;; more here - handle entites and disallow "<"
    (let* ([delimiter (read-char in)]
           [value (case delimiter
                    [(#\' #\")
                     (list->string
                      (let read-more ()
                        (let ([c (non-eof peek-char in pos)])
                          (cond
                            [(eq? c 'special)
                             (lex-error in pos "attribute values cannot contain non-text values")]
                            [(eq? c delimiter) (read-char in) null]
                            [(eq? c #\&)
                             (let ([entity (expand-entity (lex-entity in pos))])
                               (if (pcdata? entity)
                                   (append (string->list (pcdata-string entity)) (read-more))
                                   ;; more here - do something with user defined entites
                                   (read-more)))]
                            [else (read-char in) (cons c (read-more))]))))]
                    [else (cond
                            [(char? delimiter)
                             (lex-error in pos "attribute values must be in ''s or in \"\"s")]
                            [(eof-object? delimiter)
                             (lex-error in pos "unexpected EOF")]
                            [else delimiter])])])
      (make-attribute start (pos) name value))))


;; skip-space : Input-port -> Void
;; deviation - should sometimes insist on at least one space
(: skip-space (Input-Port -> Void))
(define (skip-space in)
  (let loop ()
    (let ([c (peek-char-or-special in)])
      (when (and (char? c)
                 (char-whitespace? c))
        (read-char in)
        (loop)))))

;; lex-pcdata : Input-port (-> Location) -> Pcdata
;; deviation - disallow ]]> "for compatability" with SGML, sec 2.4 XML spec 
(: lex-pcdata (Input-Port (-> Location) -> Pcdata))
(define (lex-pcdata in pos)
  (let ([start (pos)]
        [data (let: loop : (Listof Char) ()
                (let ([next (peek-char in)])
                  (cond
                    [(or (eof-object? next)
                         (not (char? next))
                         (eq? next #\&)
                         (eq? next #\<))
                     null]
                    [(and (char-whitespace? next) (collapse-whitespace))
                     (skip-space in)
                     (cons #\space (loop))]
                    [else (read-char in) (cons next (loop))])))])
    (make-pcdata start
                 (pos)
                 (list->string data))))

;; lex-name : Input-port (-> Location) -> Symbol
(: lex-name (Input-Port (-> Location) -> Symbol))
(define (lex-name in pos)
  (let ([c (non-eof read-char in pos)])
    (unless (name-start? c)
      (lex-error in pos "expected name, received ~e" c))
    (string->symbol
     (list->string
      (cons c (let: lex-rest : (Listof Char) ()
                (let ([c (non-eof peek-char in pos)])
                  (cond
                    [(eq? c 'special)
                     (lex-error in pos "names cannot contain non-text values")]
                    [(name-char? c)
                     (read-char in)
                     (cons c (lex-rest))]
                    [else null]))))))))

;; skip-dtd : Input-port (-> Location) -> Void
(: skip-dtd (Input-Port (-> Location) -> Void))
(define (skip-dtd in pos)
  (let skip ()
    (case (non-eof read-char in pos)
      [(#\') (read-until #\' in pos) (skip)]
      [(#\") (read-until #\" in pos) (skip)]
      [(#\<)
       (case (non-eof read-char in pos)
         [(#\!) (case (non-eof read-char in pos)
                  [(#\-) (read-char in) (lex-comment-contents in pos) (read-char in) (skip)]
                  [else (skip) (skip)])]
         [(#\?) (lex-pi-data in pos) (skip)]
         [else (skip) (skip)])]
      [(#\>) (void)]
      [else (skip)])))

;; name-start? : Char -> Bool
(: name-start? (Char -> Boolean))
(define (name-start? ch)
  (and (char? ch)
       (or (char-alphabetic? ch)
           (eq? ch #\_)
           (eq? ch #\:))))

;; name-char? : Char -> Bool
(: name-char? (Char -> Boolean))
(define (name-char? ch)
  (and (char? ch)
       (or (name-start? ch)
           (char-numeric? ch)
           (eq? ch #\.)
           (eq? ch #\-))))

;; read-until : Char Input-port (-> Location) -> String
;; discards the stop character, too
(: read-until (Char Input-Port (-> Location) -> String))
(define (read-until char in pos)
  (list->string

   (let read-more ()
     (let ([c (non-eof read-char in pos)])
       (cond
         [(eq? c char) null]
         [else (cons c (read-more))])))))

;; non-eof : (Input-port -> (U Char Eof)) Input-port (-> Location) -> Char
(: non-eof ((Input-Port -> (U Char EOF)) Input-Port (-> Location) -> Char))
(define (non-eof f in pos)
  (let: ([c : (U Char EOF) (f in)])
    (if (eof-object? c)
        (lex-error in pos "unexpected eof")
        c)))

;; gen-read-until-string : String -> Input-port (-> Location) -> String
;; uses Knuth-Morris-Pratt from
;; Introduction to Algorithms, Cormen, Leiserson, and Rivest, pages 869-876
;; discards stop from input
;; ---
;; Modified by Jay to look more like the version on Wikipedia after discovering a bug when parsing CDATA
;; The use of the hasheq table and the purely numeric code trades hash efficiency for stack/ec capture efficiency
(define-struct: hash-string
  ([port : Input-Port]
   [pos : (-> Location)]
   [ht : (HashTable Integer Char)]))
(: hash-string-ref (hash-string Integer -> Char))
(define (hash-string-ref hs k)
  (match-define (hash-string port pos ht) hs)
  (hash-ref! ht k (lambda () (non-eof read-char port pos))))

(: gen-read-until-string (String -> (Input-Port (-> Location) -> String)))
(define (gen-read-until-string W)
  (define: Wlen : Integer (string-length W))
  (define: T : (Vectorof Integer) (make-vector Wlen 0))
  (vector-set! T 0 -1)
  ;(vector-set! T 1 0)
  (let kmp-table ([pos 2] [cnd 0])
    (when (pos . < . Wlen)
      (cond
        [(char=? (string-ref W (sub1 pos)) (string-ref W cnd))
         (vector-set! T pos (add1 cnd))
         (kmp-table (add1 pos) (add1 cnd))]
        [(cnd . > . 0)
         (kmp-table pos (vector-ref T cnd))]
        [(zero? cnd)
         (vector-set! T pos 0)
         (kmp-table (add1 pos) 0)])))
  (lambda (S-as-port S-pos)
    (define S (hash-string S-as-port S-pos (make-hasheq)))
    (define W-starts-at
      (let: kmp-search : Integer
        ([m : Integer 0] [i : Integer 0])
        (if (char=? (string-ref W i) (hash-string-ref S (+ m i)))
            (let ([i (add1 i)])
              (if (= i Wlen)
                  m
                  (kmp-search m i)))
            (let* ([Ti (vector-ref T i)]
                   [m (+ m i (* -1 Ti))])
              (if (Ti . > . -1)
                  (let ([i Ti])
                    (kmp-search m i))
                  (let ([i 0])
                    (kmp-search m i)))))))
    (list->string
     (for/list ([i (in-range 0 W-starts-at)])
       (hash-string-ref S i)))))

;; "-->" makes more sense, but "--" follows the spec.
(: lex-comment-contents (Input-Port (-> Location) -> String))
(define lex-comment-contents (gen-read-until-string "--"))

(: lex-pi-data (Input-Port (-> Location) -> String))
(define lex-pi-data (gen-read-until-string "?>"))

(: lex-cdata-contents (Input-Port (-> Location) -> String))
(define lex-cdata-contents (gen-read-until-string "]]>"))


;; positionify : Input-port -> Input-port (-> Location)
; This function predates port-count-lines! and port-next-location.
; Otherwise I would have used those directly at the call sites.
(: positionify (Input-Port -> (Values Input-Port (-> Location))))
(define (positionify in)
  (port-count-lines! in)
  (values
   in
   (lambda ()
     (let-values ([(line column offset) (port-next-location in)])
       (and line column offset (make-location line column offset))))))

;; locs : (listof (list number number))
(define-struct: (exn:xml exn:fail:read) ())

;; lex-error : Input-port String (-> Location) TST* -> alpha
;; raises a lexer error, using exn:xml
(: lex-error : (All (A) (Input-Port (-> Location) String Any * -> A))) ;;; comment is wrong
(define (lex-error in pos str . rest)
  (let* ([the-pos (pos)]
         [offset (and (location? pos) (location-offset the-pos))])
    (raise
     (make-exn:xml
      (format "read-xml: lex-error: at position ~a: ~a"
              (format-source the-pos)
              (apply format str rest))
      (current-continuation-marks)
      (list
       (make-srcloc (object-name in) #f #f offset 1))))))

;; parse-error : (listof srcloc) (listof TST) *-> alpha
;; raises a parsing error, using exn:xml
(: parse-error (All (A) ((Listof srcloc) String Any * -> A)))
(define (parse-error src fmt . args)
  (raise (make-exn:xml (string-append "read-xml: parse-error: "
                                      (apply format fmt args))
                       (current-continuation-marks)
                       src)))

;; format-source : Location -> string
;; to format the source location for an error message
(: format-source (Location -> String))
(define (format-source loc)
  (if (location? loc)
      (format "~a.~a/~a" (location-line loc) (location-char loc) (location-offset loc))
      (format "~a" loc)))
