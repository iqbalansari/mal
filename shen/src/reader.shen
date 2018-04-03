(package reader (append [read-str]
                        [tokenize]
                        \* Special tokens from tokenizer *\
                        [keyword]
                        [open-paren close-paren]
                        [open-bracket close-bracket]
                        [open-brace close-brace]
                        [tilde splice-unquote]
                        [single-quote backtick at caret]
                        \* Special symbols from reader *\
                        [nil]
                        [unquote splice-unquote]
                        [quote quasiquote deref with-meta]
                        \* nil type from types.shen *\
                        [nil]
                        [intern-keyword]
                        \* Helpers from utils *\
                        [list->vector assoc->dict])

(defcc <symbol>
  [symbol Symbol] := (intern Symbol))

(defcc <keyword>
  [keyword Symbol] := (intern-keyword Symbol))

(defcc <number>
  [number Number] := (head (read-from-string Number)))

(defcc <string>
  [string String] := String)

(defcc <atom>
  <symbol> ; <keyword> ; <number> ; <string> )

(defcc <elements>
  <form> <elements> := [ <form> | <elements> ] ;
  <form>            := [ <form> ])

(defcc <key> <form>)

(defcc <value> <form>)

(defcc <pair>
  <key> <value> := (@p <key> <value>) ;
  <form>        := (error "Odd number of elements provided for hash-map"))

(defcc <pairs>
  <pair> <pairs> := [ <pair> | <pairs> ] ;
  <pair>         := [ <pair> ])

(defcc <list>
  open-paren <elements> close-paren :=  <elements> ;
  open-paren            close-paren := [] ;
  open-paren                        := (error "EOF while reading list"))

(defcc <vector>
  open-bracket <elements> close-bracket := (list->vector <elements> ) ;
  open-bracket            close-bracket := <> ;
  open-bracket                          := (error "EOF while reading vector"))

(defcc <hash-map>
  open-brace <pairs> close-brace := (assoc->dict <pairs>) ;
  open-brace         close-brace := (assoc->dict []) ;
  open-brace                     := (error "EOF while reading hash-map"))

(defcc <quoted-form>
  single-quote   <form> := [quote <form>] ;
  backtick       <form> := [quasiquote <form>] ;
  tilde          <form> := [unquote <form>] ;
  splice-unquote <form> := [splice-unquote <form>] ;
  at             <form> := [deref <form>])

(defcc <meta> <form>)

(defcc <meta-form>
  caret <meta> <form> := [with-meta <form> <meta>]  )

(defcc <form>
  <atom> ;
  <list> ; <vector> ; <hash-map> ;
  <quoted-form> ;
  <meta-form>)

(define read-str
  ""  -> nil
  Str -> (compile (function <form>) (tokenize Str)))

)
