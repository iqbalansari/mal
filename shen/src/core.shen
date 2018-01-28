(define pr-and-join
  Elems Readably? Sep -> (let Reps (map (/. X (pr-str X Readably?)) Elems)
                           (make-string "~A" (string-join Reps Sep))))

(define mal.count
  nil    -> 0
  Vector -> (limit Vector) where (vector? Vector)
  List   -> (length List)  where (list? List)
  _      -> (error "'count' works only with vectors, list and nil"))

(define mal.empty?
  nil -> true
  <>  -> true
  []  -> true
  _   -> false)

(define mal.seq=
  Seq OtherSeq -> (and (= (mal.count Seq) (mal.count OtherSeq))
                       (all? (/. Pair (mal.= (fst Pair) (snd Pair)))
                             (zip (seq->list Seq) (seq->list OtherSeq)))))

(define mal.hashmap=
  Hashmap OtherHashmap -> (and (== (dict-count Hashmap) (dict-count OtherHashmap))
                               (dict-fold (/. Key Value Equals
                                              (and Equals
                                                   (mal.= (<-dict/or OtherHashmap Key (freeze nil)) Value)))
                                          Hashmap
                                          true)))

(define mal.=
  Hashmap OtherHashmap -> (mal.hashmap= Hashmap OtherHashmap) where (and (dict? Hashmap) (dict? OtherHashmap))
  Seq OtherSeq         -> (mal.seq= Seq OtherSeq)             where (and (sequence? Seq) (sequence? OtherSeq))
  Any Other            -> (= Any Other))

(define mal.cons
  Any Sequence -> (cons Any (seq->list Sequence)))

(define mal.concat-h
  [ ]                       Acc -> Acc
  [ List OtherList | Rest ] Acc -> (mal.concat [ (append List OtherList) | Rest])
  [ List ]                  Acc -> List)

(define mal.concat
  Lists -> (mal.concat-h (map seq->list Lists) []))

(define mal.nth
  <>               _     -> (error "'nth' index out of range")
  []               _     -> (error "'nth' index out of range")

  [ Head | Tail  ] 0     -> Head
  (@v Head Tail  ) 0     -> Head

  [ _ | Tail     ] Index -> (mal.nth Tail (- Index 1))
  (@v _ Tail     ) Index -> (mal.nth Tail (- Index 1)))

(define mal.first
  []           -> nil
  <>           -> nil
  nil          -> nil

  [ Head | _ ] -> Head
  (@v Head  _) -> Head)

(define mal.rest
  []           -> []
  <>           -> []
  nil          -> []

  [ _ | Tail ] -> Tail
  (@v _ Tail)  -> (vector->list Tail))

(define mal.apply
  Fn []     -> (mal-apply Fn [])
  Fn [Args] -> (mal-apply Fn (seq->list Args))
  Fn Args   -> (let ArgList (append (butlast Args) (seq->list (last Args)))
                 (mal-apply Fn ArgList)))

(define mal.map
  Fn Seq -> (map (/. Arg (mal-apply Fn [Arg])) (seq->list Seq)))

(define mal.sequential?
  Value -> (or (vector? Value) (cons? Value) (= [] Value)))

(define mal.hashmap-h
  [ ]                   Dict -> Dict
  [ Key ]               Dict -> (error "'mal.hashmap' passed a list with odd number of parameters")
  [ Key Value | Pairs ] Dict -> (do (dict-> Dict Key Value)
                                    (mal.hashmap-h Pairs Dict)))

(define mal.hashmap
  List -> (let Length (/ (length List) 2)
            (mal.hashmap-h List (dict (if (= Length 0) 4 Length)))))

(define mal.assoc
  Hashmap List -> (let Hashmap' (dict-fold (/. Key Value Acc
                                               (do (dict-> Acc Key Value) Acc))
                                           Hashmap
                                           (dict (+ (dict-count Hashmap) (/ (length List) 2))))
                    (mal.hashmap-h List Hashmap')))

(define mal.dissoc
  Hashmap List -> (dict-fold (/. Key Value Acc
                                 (if (not (element? Key List))
                                     (do (dict-> Acc Key Value)
                                         Acc)
                                     Acc))
                             Hashmap
                             (dict (dict-count Hashmap))))

(define mal.symbol?
  nil    -> false
  Symbol -> (symbol? Symbol))

(set ns [(@p +       (mal-builtin-fn [a b]    +))
         (@p -       (mal-builtin-fn [a b]    -))
         (@p *       (mal-builtin-fn [a b]    *))
         (@p /       (mal-builtin-fn [a b]    /))
         (@p list    (mal-builtin-fn [& rest] (/. Rest Rest)))
         (@p list?   (mal-builtin-fn [seq]    list?))
         (@p empty?  (mal-builtin-fn [seq]    mal.empty?))
         (@p count   (mal-builtin-fn [seq]    mal.count))
         (@p =       (mal-builtin-fn [a b]    mal.=))
         (@p <       (mal-builtin-fn [a b]    <))
         (@p <=      (mal-builtin-fn [a b]    <=))
         (@p >       (mal-builtin-fn [a b]    >))
         (@p >=      (mal-builtin-fn [a b]    >=))
         (@p pr-str  (mal-builtin-fn [& rest] (/. Rest
                                                  (pr-and-join Rest true " "))))
         (@p str     (mal-builtin-fn [& rest] (/. Rest
                                                  (pr-and-join Rest false ""))))
         (@p prn     (mal-builtin-fn [& rest] (/. Rest
                                                  (do (pr (pr-and-join Rest true " "))
                                                      (pr "c#10;")
                                                      nil))))
         (@p println (mal-builtin-fn [& rest] (/. Rest
                                                  (do (pr (pr-and-join Rest false " "))
                                                      (pr "c#10;")
                                                      nil))))
         (@p read-string (mal-builtin-fn [str] read-str))
         (@p slurp       (mal-builtin-fn [str] read-file-as-string))
         (@p atom        (mal-builtin-fn [val] atom))
         (@p atom?       (mal-builtin-fn [atom] atom?))
         (@p deref       (mal-builtin-fn [atom] deref))
         (@p reset!      (mal-builtin-fn [atom val] reset!))
         (@p swap!       (mal-builtin-fn [atom func & args] swap!))
         (@p cons        (mal-builtin-fn [elem list] mal.cons))
         (@p concat      (mal-builtin-fn [& lists] mal.concat))
         (@p nth         (mal-builtin-fn [seq index] mal.nth))
         (@p first       (mal-builtin-fn [seq] mal.first))
         (@p rest        (mal-builtin-fn [seq] mal.rest))
         (@p throw       (mal-builtin-fn [val] (/. Value
                                                   (do (set *error* Value)
                                                       (error "user-error")))))
         (@p apply       (mal-builtin-fn [fn & rest] mal.apply))
         (@p map         (mal-builtin-fn [fn list] mal.map))
         (@p nil?        (mal-builtin-fn [val] nil?))
         (@p true?       (mal-builtin-fn [val] (/. Val (= Val true))))
         (@p false?      (mal-builtin-fn [val] (/. Val (= Val false))))
         (@p symbol?     (mal-builtin-fn [val] mal.symbol?))
         (@p symbol      (mal-builtin-fn [string] string->symbol))
         (@p keyword     (mal-builtin-fn [string] (/. Val (if (keyword? Val) Val (intern-keyword Val)))))
         (@p keyword?    (mal-builtin-fn [val] keyword?))
         (@p vector      (mal-builtin-fn [& args] list->vector))
         (@p vector?     (mal-builtin-fn [string] vector?))
         (@p hash-map    (mal-builtin-fn [& args] mal.hashmap))
         (@p map?        (mal-builtin-fn [value] dict?))
         (@p get         (mal-builtin-fn [hashmap key] (/. Dict Key
                                                           (if (= Dict nil)
                                                               nil
                                                               (<-dict/or Dict Key (freeze nil))))))
         (@p contains?   (mal-builtin-fn [hashmap key] (/. Dict Key
                                                           (let Sentinel (gensym "nil")
                                                             (if (= Dict nil)
                                                                 false
                                                                 (not (= Sentinel (<-dict/or Dict Key (freeze Sentinel)))))))))
         (@p sequential? (mal-builtin-fn [value] mal.sequential?))
         (@p keys        (mal-builtin-fn [hashmap] dict-keys))
         (@p vals        (mal-builtin-fn [hashmap] dict-values))
         (@p assoc       (mal-builtin-fn [hashmap & list] mal.assoc))
         (@p dissoc      (mal-builtin-fn [hashmap & list] mal.dissoc))])
