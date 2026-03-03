#lang racket/base

;; Haskell code generation from Redex grammar data structures.
;; This module is AI-generated.
;;
;; Converts grammar data (as produced by define-grammar/pred's
;; name-grammar-data binding) into Haskell algebraic data type
;; declarations, either as raw snippets or complete compilable modules.

(require racket/list
         racket/string
         racket/format)

(provide grammar->haskell
         grammar->haskell/file
         grammar->haskell/string
         grammar->haskell-module
         grammar->haskell-module/file
         grammar->haskell-module/string)

;; Convert symbol to Haskell type name (CamelCase)
;; Converts hyphens to word boundaries, capitalizes each word
;; e.g., unsafe-procedure-call -> UnsafeProcedureCall
(define (make-haskell-type-name sym)
  (let* ([str (symbol->string sym)]
         [parts (string-split str "-")])
    (apply string-append
           (map (lambda (part)
                  (if (string=? part "")
                      ""
                      (string-append (string-upcase (substring part 0 1))
                                     (substring part 1))))
                parts))))

;; Get the base nonterminal name, stripping numeric suffixes
;; e.g., reg_1 -> reg, expr_2 -> expr
(define (nonterminal-base name)
  (let ([str (symbol->string name)])
    (if (regexp-match? #rx"_[0-9]+$" str)
        (string->symbol (regexp-replace #rx"_[0-9]+$" str ""))
        name)))


;; Map predicate symbols to Haskell types (returns symbol)
;; Optional: predicate-map is an association list of (predicate . haskell-type) pairs
;; If provided, checks the map before using default mappings
(define (predicate->haskell-type pred [predicate-map '()])
  (let ([mapped (assq pred predicate-map)])
    (if mapped
        (cdr mapped)
        (case pred
          [(int64?) 'Int64]
          [(int32?) 'Int32]
          [(int16?) 'Int16]
          [(int8?) 'Int8]
          [(uint64?) 'Word64]
          [(uint32?) 'Word32]
          [(uint16?) 'Word16]
          [(uint8?) 'Word8]
          [(integer?) 'Integer]
          [(number?) 'Number]
          [(string?) 'String]
          [(symbol?) 'Symbol]
          [(boolean?) 'Bool]
          [else (string->symbol (sanitize-haskell-name (make-haskell-type-name pred)))]))))

;; Parse a single clause definition term
;; literals: list of predicate symbols from #:literals
;; datum-literals: list of syntactic symbols from #:datum-literals
;; nonterminals: list of defined nonterminal names
;; clauses: full clause definitions (needed to resolve pure embeddings)
(define (parse-clause-term term literals datum-literals nonterminals
                           [predicate-map '()] [clauses '()])
  (cond
    [(eq? term '...) '(...)]
    [(boolean? term) (if term 'True 'False)]
    [(symbol? term)
     (cond
       ;; It's a declared predicate (in #:literals)
       [(memq term literals)
        (predicate->haskell-type term predicate-map)]
       ;; It's a datum literal (in #:datum-literals) — capitalize for Haskell
       [(memq term datum-literals)
        (string->symbol (symbol->haskell-ctor term))]
       ;; It's a direct reference to a nonterminal
       [(memq term nonterminals)
        ;; Check if it's a pure embedding — resolve to the underlying type
        (let ([pred (get-embedding-predicate term clauses literals)])
          (if pred
              (predicate->haskell-type pred predicate-map)
              (make-haskell-type-name term)))]
       ;; Check if it's a nonterminal with a numeric suffix (e.g., reg_1)
       [(let ([base (nonterminal-base term)])
          (and (not (eq? base term)) (memq base nonterminals)))
        (let* ([base (nonterminal-base term)]
               [pred (get-embedding-predicate base clauses literals)])
          (if pred
              (predicate->haskell-type pred predicate-map)
              (make-haskell-type-name base)))]
       ;; Unknown symbol — capitalize for Haskell type compatibility
       [else (string->symbol (symbol->haskell-ctor term))])]
    [(list? term)
     ;; Could be a syntactic form (keyword args...) or a structural pattern
     (let ([head (car term)])
       (cond
         [(not (symbol? head))
          (flatten-clause-args term literals datum-literals nonterminals predicate-map clauses)]
          ;; Datum-literal sub-form: return inferred type reference
          [(memq head datum-literals)
           (symbol->haskell-ctor head)]
         ;; Otherwise, structural pattern
         [else
          (flatten-clause-args term literals datum-literals nonterminals predicate-map clauses)]))]
    [else term]))

;; Parse a list of clause arguments, handling `...` (repeat) markers
(define (flatten-clause-args args literals datum-literals nonterminals
                             [predicate-map '()] [clauses '()])
  (let loop ([args args] [result '()])
    (cond
      [(null? args) (reverse result)]
      [(eq? (car args) '...)
       ;; Repeat operator applies to previous element: wrap it in a list marker
       (if (null? result)
           (loop (cdr args) result)
           (let ([prev (car result)])
             (loop (cdr args) (cons (list 'list-of prev) (cdr result)))))]
      [else
       (let ([parsed (parse-clause-term (car args) literals datum-literals nonterminals predicate-map clauses)])
         (loop (cdr args) (cons parsed result)))])))

;; Format a type for Haskell output
(define (format-haskell-type ty)
  (cond
    [(symbol? ty) (symbol->string ty)]
    [(and (list? ty) (not (null? ty)) (eq? (car ty) 'list-of))
     ;; [ty] for "list of ty"
     (string-append "[" (format-haskell-type (cadr ty)) "]")]
    [(list? ty)
     ;; Structural pattern → Haskell tuple (collapse single-element)
     (if (= 1 (length ty))
         (format-haskell-type (car ty))
         (string-append "(" (string-join (map format-haskell-type ty) ", ") ")"))]
    [else (format "~a" ty)]))

;; Sanitize a string for use as a Haskell constructor
;; Replaces special characters with meaningful names
(define (sanitize-haskell-name str)
  (cond
    ;; Exact single-character operators
    [(string=? str "*") "Times"]
    [(string=? str "+") "Plus"]
    [(string=? str "-") "Minus"]
    [(string=? str "/") "Div"]
    [(string=? str "<") "Lt"]
    [(string=? str ">") "Gt"]
    ;; Multi-character operators
    [(string=? str "<=") "Lte"]
    [(string=? str ">=") "Gte"]
    [(string=? str "eq?") "Eq"]
    [else
     ;; Replace special characters with names, remove others
     (let ([result
            (apply string-append
                   (map (lambda (ch)
                          (cond
                            [(char=? ch #\!) ""]
                            [(char=? ch #\?) ""]
                            [(char=? ch #\*) "Star"]
                            [(char=? ch #\+) "Plus"]
                            [(char=? ch #\<) "Lt"]
                            [(char=? ch #\>) "Gt"]
                            [(char=? ch #\=) "Eq"]
                            [else (string ch)]))
                        (string->list str)))])
       (if (string=? result "") "Unknown" result))]))

;; Convert a symbol to a valid Haskell constructor name
;; Sanitizes the raw symbol string first, then applies CamelCase
(define (symbol->haskell-ctor sym)
  (let* ([raw (symbol->string sym)]
         [sanitized (sanitize-haskell-name raw)])
    ;; Apply CamelCase to the sanitized result (split on hyphens)
    (let ([parts (string-split sanitized "-")])
      (apply string-append
             (map (lambda (part)
                    (if (string=? part "")
                        ""
                        (string-append (string-upcase (substring part 0 1))
                                       (substring part 1))))
                  parts)))))

;; Generate a Haskell constructor from a clause
;; type-prefix: string prefix for constructor names (typically the type name)
(define (generate-haskell-constructor clause literals datum-literals nonterminals
                                      [predicate-map '()] [type-prefix ""] [clauses '()])
  (cond
    [(null? clause) ""]
    [(boolean? clause)
     (string-append type-prefix (if clause "True" "False"))]
    [(symbol? clause)
     (cond
       ;; Predicate literal — constructor wraps a value of the predicate's type
       [(memq clause literals)
        (let ([htype (symbol->string (predicate->haskell-type clause predicate-map))])
          (string-append type-prefix htype " " htype))]
       ;; Datum-literal — nullary constructor (constant tag)
       [(memq clause datum-literals)
        (string-append type-prefix (symbol->haskell-ctor clause))]
       ;; Nonterminal reference — constructor wraps a value of that type
       [(memq clause nonterminals)
        (let ([embedding-pred (get-embedding-predicate clause clauses literals)])
          (if embedding-pred
              (let ([htype (symbol->string (predicate->haskell-type embedding-pred predicate-map))])
                (string-append type-prefix (symbol->haskell-ctor clause) " " htype))
              (let ([htype (make-haskell-type-name clause)])
                (string-append type-prefix (symbol->haskell-ctor clause) " " htype))))]
       ;; Suffixed nonterminal reference (e.g., reg_1)
       [(let ([base (nonterminal-base clause)])
          (and (not (eq? base clause)) (memq base nonterminals)))
        (let* ([base (nonterminal-base clause)]
               [embedding-pred (get-embedding-predicate base clauses literals)])
          (if embedding-pred
              (let ([htype (symbol->string (predicate->haskell-type embedding-pred predicate-map))])
                (string-append type-prefix (symbol->haskell-ctor clause) " " htype))
              (let ([htype (make-haskell-type-name base)])
                (string-append type-prefix (symbol->haskell-ctor clause) " " htype))))]
       ;; Unknown symbol — nullary constructor
       [else
        (string-append type-prefix (symbol->haskell-ctor clause))])]
    [(list? clause)
     (let ([head (car clause)])
       (cond
         [(symbol? head)
          (let* ([ctor-name (string-append type-prefix (symbol->haskell-ctor head))]
                 [args (cdr clause)]
                 [parsed-args (flatten-clause-args args literals datum-literals nonterminals predicate-map clauses)])
            (if (null? parsed-args)
                ctor-name
                (string-append ctor-name " " (string-join (map format-haskell-type parsed-args) " "))))]
         [else
          ;; Head is not a symbol — whole clause is a structural pattern
          ;; Use flatten-clause-args to handle ... markers properly
          ;; Generate a constructor with the type prefix as name
          (let* ([parsed (flatten-clause-args clause literals datum-literals nonterminals predicate-map clauses)]
                 [field-strs (map format-haskell-type parsed)])
            (if (null? field-strs)
                type-prefix
                (string-append type-prefix " " (string-join field-strs " "))))]))]
    [else (format "~a" clause)]))

;; Check if a nonterminal is a pure embedding (only contains a single predicate)
;; Returns the predicate if true, #f otherwise
(define (get-embedding-predicate nonterminal clauses literals)
  (let ([clause-defs (assoc nonterminal clauses)])
    (if (and clause-defs
             (= 1 (length (cdr clause-defs)))
             (let ([single-clause (car (cdr clause-defs))])
               (and (symbol? single-clause)
                    (memq single-clause literals))))
        (car (cdr clause-defs))
        #f)))

;; Extract the constructor name (first word) from a constructor string
(define (constructor-name str)
  (let ([parts (string-split str " ")])
    (if (null? parts) str (car parts))))

;; Uniquify constructor strings by appending numeric suffixes to duplicates
;; e.g., ("Set Reg Int64" "Set Reg Reg") -> ("Set1 Reg Int64" "Set2 Reg Reg")
(define (uniquify-constructors strs)
  (let* ([names (map constructor-name strs)]
         ;; Count occurrences of each name
         [counts (foldl (lambda (n acc)
                          (hash-set acc n (add1 (hash-ref acc n 0))))
                        (hash)
                        names)])
    ;; Track per-name counters as we walk through
    (let loop ([remaining strs]
               [remaining-names names]
               [counters (hash)]
               [result '()])
      (if (null? remaining)
          (reverse result)
          (let* ([str (car remaining)]
                 [name (car remaining-names)]
                 [count (hash-ref counts name)]
                 [idx (add1 (hash-ref counters name 0))]
                 [new-counters (hash-set counters name idx)]
                 [new-str (if (> count 1)
                              ;; Duplicate: replace first word with suffixed version
                              (let ([rest (substring str (string-length name))])
                                (string-append name (number->string idx) rest))
                              str)])
            (loop (cdr remaining)
                  (cdr remaining-names)
                  new-counters
                  (cons new-str result)))))))

;; Generate Haskell data declaration for a nonterminal
;; Returns "" if the nonterminal is a pure embedding (will be handled as a type alias)
(define (generate-haskell-datatype nonterminal clauses literals datum-literals [predicate-map '()])
  (let* ([embedding-pred (get-embedding-predicate nonterminal clauses literals)]
         [type-name (make-haskell-type-name nonterminal)]
         [clause-defs (assoc nonterminal clauses)]
         [nonterminals (map car clauses)]
         [constructors (if clause-defs (cdr clause-defs) '())])
    (cond
      ;; Pure embedding: skip generation (will be treated as a type alias in Haskell)
      [embedding-pred ""]
      ;; Empty nonterminal
      [(null? constructors) ""]
      ;; Normal data type
      [else
       (let* ([constructor-strs
               (map (lambda (ctor)
                      (generate-haskell-constructor ctor literals datum-literals nonterminals predicate-map type-name clauses))
                    constructors)]
              [unique-strs (uniquify-constructors constructor-strs)])
         (string-append "data " type-name " = "
                       (string-join unique-strs " | ")
                       "\n"))])))


;; Scan a term for datum-literal-headed sub-forms that need inferred types
;; Records them in result hash: datum-literal-symbol → list of children lists
(define (scan-subform-types term datum-literals result)
  (cond
    [(and (list? term) (not (null? term))
          (symbol? (car term)) (memq (car term) datum-literals))
     ;; Found a datum-literal sub-form — record it
     (let ([head (car term)]
           [children (cdr term)])
       (let ([existing (hash-ref result head '())])
         (unless (member children existing)
           (hash-set! result head (cons children existing))))
       ;; Recurse into children for deeper sub-forms
       (for-each (lambda (child) (scan-subform-types child datum-literals result))
                 children))]
    [(list? term)
     (for-each (lambda (child) (scan-subform-types child datum-literals result))
               term)]
    [else (void)]))

;; Collect all datum-literal sub-forms from the grammar that need inferred types
;; Only scans inside top-level clause arguments (not the top-level head itself)
(define (collect-datum-literal-subtypes clauses datum-literals)
  (let ([result (make-hash)])
    (for-each
     (lambda (nt-clause)
       (for-each
        (lambda (clause)
          (when (list? clause)
            (let ([head (car clause)])
              (if (and (symbol? head) (memq head datum-literals))
                  ;; Top-level datum-literal form — scan children only
                  (for-each (lambda (child) (scan-subform-types child datum-literals result))
                            (cdr clause))
                  ;; Structural pattern — scan all elements
                  (for-each (lambda (child) (scan-subform-types child datum-literals result))
                            clause)))))
        (cdr nt-clause)))
     clauses)
    result))

;; Generate inferred data types for collected datum-literal sub-forms
(define (generate-inferred-types subtype-map literals datum-literals nonterminals
                                 predicate-map clauses)
  (apply string-append
    (hash-map subtype-map
      (lambda (head children-variants)
        (let* ([type-name (symbol->haskell-ctor head)]
               [constructor-strs
                (map (lambda (children)
                       (let* ([parsed-args (flatten-clause-args children literals datum-literals
                                                               nonterminals predicate-map clauses)]
                              [field-strs (map format-haskell-type parsed-args)])
                         (if (null? field-strs)
                             type-name
                             (string-append type-name " " (string-join field-strs " ")))))
                     children-variants)]
               [unique-strs (uniquify-constructors constructor-strs)])
          (string-append "data " type-name " = "
                         (string-join unique-strs " | ")
                         "\n"))))))

;; Generate type aliases for predicates whose Haskell types aren't built-in
;; Covers both pure embedding nonterminals and directly-used predicates
(define (generate-type-aliases literals predicate-map)
  (let ([known-types '("Int8" "Int16" "Int32" "Int64"
                        "Word8" "Word16" "Word32" "Word64"
                        "Integer" "Number" "String" "Symbol" "Bool")])
    (let ([seen (make-hash)])
      (apply string-append
        (filter-map
         (lambda (pred)
           (let ([type-str (symbol->string (predicate->haskell-type pred predicate-map))])
             (and (not (member type-str known-types))
                  (not (hash-ref seen type-str #f))
                  (begin
                    (hash-set! seen type-str #t)
                    (string-append "type " type-str " = String\n")))))
         literals)))))


;; Main public function: convert grammar data to Haskell code
;;
;; grammar-data is a property list with keys:
;;   'name - the grammar name
;;   'literals - list of predicate symbols (e.g., int64?, int32?)
;;   'datum-literals - list of syntactic keyword symbols
;;   'clauses - list of (nonterminal . clauses) pairs
;;
;; Optional: predicate-map is an association list mapping predicates to Haskell types
;;   e.g., '((my-int? Int) (my-string? Text))
;;   If provided, these override the built-in mappings
;;
;; Generates Haskell algebraic data type declarations for each nonterminal.
;;
;; Conversion rules:
;; - Nonterminals become data types (capitalized)
;; - Clauses become constructors
;; - Predicate literals map to Haskell types (int64? -> Int64)
;; - Datum literal symbols become constructor keywords (capitalized)
;; - Repetition (x ...) becomes [X] in Haskell
;; - Special symbols (* + ! ?) are sanitized to valid Haskell names
(define (grammar->haskell grammar-data [port (current-output-port)] [predicate-map '()])
  (let* ([clauses (cadr (member 'clauses grammar-data))]
         [literals (cadr (member 'literals grammar-data))]
         [datum-literals (cadr (member 'datum-literals grammar-data))]
         [nonterminals (map car clauses)]
         ;; Collect and generate inferred types for datum-literal sub-forms
         [subtype-map (collect-datum-literal-subtypes clauses datum-literals)]
         [inferred-code (generate-inferred-types subtype-map literals datum-literals
                                                  nonterminals predicate-map clauses)]
         ;; Generate type aliases for unmapped predicates
         [alias-code (generate-type-aliases literals predicate-map)]
         ;; Generate main data types
         [main-code
          (apply string-append
                 (map (lambda (nt)
                        (generate-haskell-datatype nt clauses literals datum-literals predicate-map))
                      nonterminals))]
         [haskell-code (string-append alias-code inferred-code main-code)])
    (display haskell-code port)))

;; Helper: output to file
;; Writes Haskell code to the specified filename, overwriting if it exists
(define (grammar->haskell/file grammar-data filename [predicate-map '()])
  (call-with-output-file filename
    (lambda (port)
      (grammar->haskell grammar-data port predicate-map))
    #:exists 'replace))

;; Helper: output to string
;; Returns Haskell code as a string
(define (grammar->haskell/string grammar-data [predicate-map '()])
  (let ([port (open-output-string)])
    (grammar->haskell grammar-data port predicate-map)
    (get-output-string port)))

;; Generate a complete Haskell module with proper header and imports
;; This creates a standalone, importable Haskell module
(define (grammar->haskell-module grammar-data [port (current-output-port)] [predicate-map '()])
  (let* ([name (cadr (member 'name grammar-data))]
         ;; Convert name to valid Haskell identifier (hyphens to underscores)
         [module-base (string-replace (symbol->string name) "-" "_")]
         [module-name (string-append (make-haskell-type-name (string->symbol module-base)) "Types")]
         [types-code (let ([p (open-output-string)])
                       (grammar->haskell grammar-data p predicate-map)
                       (get-output-string p))])
    ;; Write language pragmas
    (displayln "{-# LANGUAGE ExistentialQuantification #-}" port)
    (displayln "" port)
    ;; Write module header
    (displayln (string-append "module " module-name " where") port)
    (displayln "" port)
    (displayln "import Data.Int (Int8, Int16, Int32, Int64)" port)
    (displayln "import Data.Word (Word8, Word16, Word32, Word64)" port)
    (displayln "" port)
    (displayln "-- | Haskell types generated from Redex grammar" port)
    (displayln (string-append "-- | Grammar name: " (symbol->string name)) port)
    (displayln "" port)
    ;; Polymorphic Any type for Redex 'any' patterns
    (displayln "data Any = forall a. (Show a, Eq a) => Any a" port)
    (displayln "instance Show Any where show (Any x) = show x" port)
    (displayln "instance Eq Any where _ == _ = False" port)
    (displayln "" port)
    ;; Write the data types with deriving clauses for common type classes
    (let ([lines (string-split types-code "\n")])
      (for-each (lambda (line)
                  (cond
                    [(string-prefix? line "data ")
                     ;; Add deriving clauses to data declarations
                     (displayln (string-append line " deriving (Show, Eq)") port)]
                    [(string=? line "")
                     ;; Skip empty lines
                     (void)]
                    [else
                     ;; Keep other lines as-is (type aliases, etc.)
                     (displayln line port)]))
                lines))))

;; Helper: output module to file
;; Writes a complete Haskell module to the specified filename
(define (grammar->haskell-module/file grammar-data filename [predicate-map '()])
  (call-with-output-file filename
    (lambda (port)
      (grammar->haskell-module grammar-data port predicate-map))
    #:exists 'replace))

;; Helper: output module to string
;; Returns a complete Haskell module as a string
(define (grammar->haskell-module/string grammar-data [predicate-map '()])
  (let ([port (open-output-string)])
    (grammar->haskell-module grammar-data port predicate-map)
    (get-output-string port)))
