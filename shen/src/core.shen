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

(define mal.=
  Any Other    -> (= Any Other) where (not (and (sequence? Any) (sequence? Other)))
  Seq OtherSeq -> (and (= (mal.count Seq) (mal.count OtherSeq))
                       (all? (/. Pair (mal.= (fst Pair) (snd Pair)))
                             (zip (seq->list Seq) (seq->list OtherSeq)))))

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
  []            -> []
  <>            -> []
  nil           -> []

  [ _ | Tail  ] -> Tail
  ( @v _ Tail ) -> (vector->list Tail))

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
         (@p rest        (mal-builtin-fn [seq] mal.rest))])
