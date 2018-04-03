(package printer (append [pr-str]
                         \* Additional type checks *\
                         [atom? list? nil? keyword? mal-fn? mal-builtin-fn?]
                         [deref keyword->string]
                         \* Data structure helpers *\
                         [flatten vector->list dict->assoc]
                         [string-join])

(define readable-string-h
  ""                Acc -> (make-string "~S" Acc)
  (@s "\"     Rest) Acc -> (readable-string-h Rest (@s Acc "\\"))
  (@s "c#34;" Rest) Acc -> (readable-string-h Rest (@s Acc "\c#34;"))
  (@s "c#10;" Rest) Acc -> (readable-string-h Rest (@s Acc "\n"))
  (@s Any     Rest) Acc -> (readable-string-h Rest (@s Acc Any)))

(define readable-string
  Str -> (readable-string-h Str ""))

(define pr-string
  Str Readably? -> (if Readably?
                       (readable-string Str)
                       (make-string "~A" Str)))

(define pr-sequence
  Start Elements End Readably? ->
  (let Contents (string-join (map (/. Ast (pr-str Ast Readably?)) Elements)
                             " ")
    (@s Start Contents End)))

(define pr-list
  List Readably? -> (pr-sequence "(" List ")" Readably?))

(define pr-vector
  Vector Readably? -> (pr-sequence "[" (vector->list Vector) "]" Readably?))

(define pr-hash-map
  Dict Readably? ->  (let Folder (/. Key Value Acc [ Key Value | Acc ])
                          Elements (dict-fold Folder Dict [])
                       (pr-sequence "{" Elements "}" Readably?)))

(define pr-str
  Form Readably? -> (cases (symbol?         Form) (make-string "~A" Form)
                           (boolean?        Form) (make-string "~A" Form)
                           (nil?            Form) (make-string "~A" Form)
                           (number?         Form) (make-string "~A" Form)
                           (string?         Form) (pr-string Form Readably?)
                           (keyword?        Form) (keyword->string Form)
                           (list?           Form) (pr-list Form Readably?)
                           (vector?         Form) (pr-vector Form Readably?)
                           (dict?           Form) (pr-hash-map Form Readably?)
                           (atom?           Form) (make-string "(atom ~A)" (deref Form))
                           (mal-fn?         Form) "#<fn>"
                           (mal-builtin-fn? Form) "#<builtin-fn>"
                           true                   (@s "shen:" (make-string "~S" Form))))

)
