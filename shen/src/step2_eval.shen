(set *repl-env* (assoc->dict [(@p + (mal-builtin-fn [a b] +))
                              (@p - (mal-builtin-fn [a b] -))
                              (@p * (mal-builtin-fn [a b] *))
                              (@p / (mal-builtin-fn [a b] /))]))

(define eval-sequence
  List   Env -> (map (/. Elem (mal-eval Elem Env)) List) where (list? List)
  Vector Env -> (let List      (vector->list Vector)
                     Evaluated (eval-sequence List Env)
                  (list->vector Evaluated))              where (vector? Vector))

(define eval-hash-map
  HashMap Env -> (let Folder (/. Key Val Acc
                                 (do (dict-> Acc Key (mal-eval Val Env))
                                     Acc))
                      Acc    (dict (dict-count HashMap))
                   (dict-fold Folder HashMap Acc)))

(define eval-ast
  nil     Env -> nil
  Keyword Env -> Keyword                     where (keyword? Keyword)
  Symbol  Env -> (<-dict Env Symbol)         where (symbol? Symbol)
  List    Env -> (eval-sequence List Env)    where (list?   List)
  Vector  Env -> (eval-sequence Vector Env)  where (vector? Vector)
  HashMap Env -> (eval-hash-map HashMap Env) where (dict?   HashMap)
  Any     Env -> Any)

(define mal-read
  Str -> (read-str Str))

(define mal-eval
  []  Env -> []
  Ast Env -> (let Evaluated (eval-ast Ast Env)
               (if (list? Evaluated)
                   (mal-apply (head Evaluated) (tail Evaluated))
                   Evaluated)))

(define mal-print
  Ast -> (pr-str Ast true))

(define rep
  Str -> (mal-print (mal-eval (mal-read Str) (value *repl-env*))))

(define main ->
  (let input (prompt/or "user> " (freeze nil))
    (if (= input nil)
        nil
        (do (trap-error (println (rep input))
                        (/. E (println (error-to-string E))))
            (main)))))
