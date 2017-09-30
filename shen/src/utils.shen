(package utils [strlen all? read-line read-line/or prompt/or println]

(define strlen-h
  Str Index -> (trap-error (do
                            (pos Str Index)
                            (strlen-h Str (+ Index 1)))
                           (/. _ Index)))

(define strlen
  Str -> (strlen-h Str 0))

(define all-list?
  Pred []      -> true
  Pred [H | T] -> (if (Pred H) (all? Pred T) false))

(define all-string?
  Pred ""             -> true
  Pred (@s Char Rest) -> (if (Pred Char) (all? Pred Rest) false))

(define all?
  Pred Sequence -> (cases (string? Sequence) (all-string? Pred Sequence)
                          true               (all-list? Pred Sequence)))

(define read-line-h
  Acc -> (let Byte (read-char-code)
           (cases (= Byte -1) end-of-input
                  (= Byte 10) Acc
                  true (read-line-h (@s Acc (n->string Byte))))))

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

)
