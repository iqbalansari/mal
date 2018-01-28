(package tokenizer (append [tokenize]
                           [keyword]
                           [open-paren close-paren]
                           [open-bracket close-bracket]
                           [open-brace close-brace]
                           [tilde splice-unquote]
                           [single-quote backtick at caret]
                           [all?])

(define mal-white-space?
  " "     -> true
  ","     -> true
  "c#10;" -> true
  _       -> false)

(define digit?
  Char -> (let char-code (string->n Char)
            (and (> char-code 47)
                 (< char-code 58))))

(define special-token?
  "["      -> true
  "]"      -> true
  "("      -> true
  ")"      -> true
  "{"      -> true
  "}"      -> true
  "\"      -> true
  "`"      -> true
  "~"      -> true
  "^"      -> true
  "@"      -> true
  "c#34;"  -> true
  ";"      -> true
  Char     -> (mal-white-space? Char))

(define accumulate-until-h
  Pred ""             Acc -> (@p Acc "")
  Pred (@s Char Rest) Acc -> (if (Pred Char)
                                 (@p Acc (@s Char Rest))
                                 (accumulate-until-h Pred Rest (@s Acc Char))))

(define accumulate-until
  Pred Chars -> (accumulate-until-h Pred Chars ""))

(define accumulate-till
  Pred Chars -> (accumulate-until-h (/. X (not (Pred X))) Chars ""))

(define skip-till
  Pred Chars -> (snd (accumulate-till Pred Chars)))

(define skip-until
  Pred Chars -> (snd (accumulate-until Pred Chars)))

(define accumulate-string-h
  ""                 Acc -> (error "EOF encountered while reading string")
  (@s "c#34;"  Rest) Acc -> (@p [string Acc] Rest)
  (@s "\\"     Rest) Acc -> (accumulate-string-h Rest (@s Acc "\"))
  (@s "\n"     Rest) Acc -> (accumulate-string-h Rest (@s Acc "c#10;"))
  (@s "\c#34;" Rest) Acc -> (accumulate-string-h Rest (@s Acc "c#34;"))
  (@s Any      Rest) Acc -> (accumulate-string-h Rest (@s Acc Any)))

(define accumulate-string
  Chars -> (accumulate-string-h Chars ""))

(define accumulate-symbol
  Chars -> (let (@p Symbol Rest) (accumulate-until (function special-token?) Chars)
             (@p (cases (= Symbol "")                   (error "EOF encountered, expected symbol")
                        (all? (function digit?) Symbol) [number Symbol]
                        (= (pos Symbol 0) ":")          [keyword Symbol]
                        true                            [symbol Symbol])
                 Rest)))

(define accumulate-atom
  (@s "-"      Rest) -> (let (@p Digits Rest) (accumulate-till (function digit?) Rest)
                             Number (@s "-" Digits)
                          (if (= Digits "")
                              (accumulate-symbol (@s "-" Rest))
                              (@p [number Number] Rest)))
  (@s "c#34;"  Rest) -> (accumulate-string Rest)
  Chars              -> (accumulate-symbol Chars))

(define tokenize-h
  ""                Acc -> (reverse Acc)

  \* Whitespace *\
  (@s " "     Rest) Acc -> (tokenize-h (skip-till (function mal-white-space?) Rest) Acc)
  (@s ","     Rest) Acc -> (tokenize-h (skip-till (function mal-white-space?) Rest) Acc)
  (@s "c#10;" Rest) Acc -> (tokenize-h (skip-till (function mal-white-space?) Rest) Acc)

  \* Comments *\
  (@s ";"     Rest) Acc -> (tokenize-h (skip-until (= "c#10;")     Rest) Acc)

  \* Special tokens *\
  (@s "("     Rest) Acc -> (tokenize-h Rest [ open-paren     | Acc ])
  (@s ")"     Rest) Acc -> (tokenize-h Rest [ close-paren    | Acc ])
  (@s "["     Rest) Acc -> (tokenize-h Rest [ open-bracket   | Acc ])
  (@s "]"     Rest) Acc -> (tokenize-h Rest [ close-bracket  | Acc ])
  (@s "{"     Rest) Acc -> (tokenize-h Rest [ open-brace     | Acc ])
  (@s "}"     Rest) Acc -> (tokenize-h Rest [ close-brace    | Acc ])
  (@s "'"     Rest) Acc -> (tokenize-h Rest [ single-quote   | Acc ])
  (@s "`"     Rest) Acc -> (tokenize-h Rest [ backtick       | Acc ])
  (@s "~" "@" Rest) Acc -> (tokenize-h Rest [ splice-unquote | Acc ])
  (@s "~"     Rest) Acc -> (tokenize-h Rest [ tilde          | Acc ])
  (@s "@"     Rest) Acc -> (tokenize-h Rest [ at             | Acc ])
  (@s "^"     Rest) Acc -> (tokenize-h Rest [ caret          | Acc ])

  \* Atoms *\
  Chars             Acc -> (let (@p Atom Rest) (accumulate-atom Chars)
                             (tokenize-h Rest [ Atom | Acc ])))

(define tokenize
  Str -> (tokenize-h Str []))

)
