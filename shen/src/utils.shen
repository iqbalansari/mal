(package utils (append [assoc->dict dict->assoc]
                       [list->vector vector->list]
                       [list->assoc]
                       [seq->list flatten zip last all?]
                       [string-join]
                       [last butlast]
                       [read-line read-line/or prompt/or println])

\* Data structure helpers *\
(define assoc->dict-h
  [ ]                        Dict -> Dict
  [ (@p Key Value) | Pairs ] Dict -> (do (dict-> Dict Key Value)
                                         (assoc->dict-h Pairs Dict)))

(define assoc->dict
  List -> (let Length (length List)
            (assoc->dict-h List (dict (if (= Length 0) 4 Length)))))

(define dict->assoc-h
  []             Dict Acc -> Acc
  [ Key | Keys ] Dict Acc -> (dict->assoc-h Keys
                                            Dict
                                            [ (@p Key (<-dict Dict Key)) | Acc ]))

(define dict->assoc
  Dict -> (dict->assoc-h (dict-keys Dict) Dict []))

(define list->vector-h
  [ ]             Acc -> Acc
  [ Head | Tail ] Acc -> (list->vector-h Tail (@v Head Acc)))

(define list->vector
  List -> (list->vector-h (reverse List) <>))

(define vector->list-h
  <>             Acc -> (reverse Acc)
  (@v Head Tail) Acc -> (vector->list-h Tail [Head | Acc]))

(define vector->list
  Vector -> (vector->list-h Vector []))

(define list->assoc-h
  [ ]                  Acc -> (reverse Acc)
  [ Key ]              Acc -> (error "list->assoc called with list with old number of elements")
  [ Key Value | Tail ] Acc -> (list->assoc-h Tail [ (@p Key Value) | Acc ]))

(define list->assoc
  List -> (list->assoc-h List []))

\* Sequence helpers *\
(define seq->list
  Vector -> (vector->list Vector) where (vector? Vector)
  nil    -> []
  Any    -> Any)

(define flatten-h
  [ ] Acc             -> Acc
  [ Head | Tail ] Acc -> (flatten-h Tail (append Acc Head)))

(define flatten
  Lists -> (flatten-h Lists []))

(define zip-h
  Any       [ ]         Acc -> (reverse Acc)
  [ ]       Any         Acc -> (reverse Acc)
  [ H | T ] [ H' | T' ] Acc -> (zip-h T T' [ (@p H H') | Acc ]))

(define zip
  List OtherList -> (zip-h List OtherList []))

(define last
  [ Head ]        ->  Head
  [ Head | Tail ] -> (last Tail))

(define butlast
  [ Head ]        -> []
  [ Head | Tail ] -> [ Head | (butlast Tail) ])

(define all-list?
  Pred []      -> true
  Pred [H | T] -> (if (Pred H) (all? Pred T) false))

(define all-string?
  Pred ""             -> true
  Pred (@s Char Rest) -> (if (Pred Char) (all? Pred Rest) false))

(define all?
  Pred Sequence -> (all-string? Pred Sequence) where (string? Sequence)
  Pred Sequence -> (all-list? Pred Sequence))

\* Number helpers *\
(define max
  N M -> N where (> N M)
  _ M -> M)

\* String helpers *\
(define strlen-h
  Str Index -> (trap-error (do
                            (pos Str Index)
                            (strlen-h Str (+ Index 1)))
                           (/. _ Index)))

(define strlen
  Str -> (strlen-h Str 0))

(define string-join-h
  [ ]              _   Acc -> Acc
  [ Elem ]         Sep Acc -> (@s Acc Elem)
  [ Elem | Elems ] Sep Acc -> (string-join-h Elems Sep (@s Acc Elem Sep)))

(define string-join
  Elems Sep -> (string-join-h Elems Sep ""))

\* IO helpers *\
(define read-line-h
  Acc -> (let Byte (read-char-code)
           (cases (= Byte -1) end-of-input
                  (= Byte 10) Acc
                  true        (read-line-h (@s Acc (n->string Byte))))))

(define read-line -> (read-line-h ""))

(define read-line/or
  EndOfInput -> (let Input (read-line)
                  (if (= Input end-of-input)
                      (thaw EndOfInput)
                      Input)))

(define prompt/or
  Prompt EndOfInput -> (do (pr Prompt)
                           (read-line/or EndOfInput)))

(define println String -> (do (pr String) (pr "c#10;")))

\* Useful reader macros *\
\* https://groups.google.com/d/msg/qilang/CcTBjfQ0Z7g/D5GphpAOjZwJ *\
(defmacro let-pair
  [let [@p X Y] Z | W] -> (let Var (gensym (protect V))
                            [let Var Z | (subst [snd Var] Y (subst [fst Var] X W))]))

)