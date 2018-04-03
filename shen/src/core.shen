(define to-list
  Vector -> (vector->list Vector) where (vector? Vector)
  Any    -> Any)

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
                             (zip (to-list Seq) (to-list OtherSeq)))))

(set ns [(@p +       (mal-builtin-fn [a b]    +))
         (@p -       (mal-builtin-fn [a b]    -))
         (@p *       (mal-builtin-fn [a b]    *))
         (@p /       (mal-builtin-fn [a b]    /))
         (@p list    (mal-builtin-fn [& rest] (/. Rest Rest)))
         (@p list?   (mal-builtin-fn [a]      list?))
         (@p empty?  (mal-builtin-fn [a]      mal.empty?))
         (@p count   (mal-builtin-fn [a]      mal.count))
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
         ])
