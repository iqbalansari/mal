(set *repl-env* (assoc->env [(@p + (mal-builtin-fn [a b] +))
                             (@p - (mal-builtin-fn [a b] -))
                             (@p * (mal-builtin-fn [a b] *))
                             (@p / (mal-builtin-fn [a b] /))]
                            nil))

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
  Symbol  Env -> (get-env Env Symbol)        where (symbol? Symbol)
  List    Env -> (eval-sequence List Env)    where (list?   List)
  Vector  Env -> (eval-sequence Vector Env)  where (vector? Vector)
  HashMap Env -> (eval-hash-map HashMap Env) where (dict?   HashMap)
  Any     Env -> Any)

(define mal-read
  Str -> (read-str Str))

(define eval-let*
  Bindings Form Env -> (eval-let* (vector->list Bindings) Form Env) where (vector? Bindings)
  Bindings Form Env ->
  (let Folder   (/. AccEnv Binding
                    (do (set-env AccEnv
                                 (fst Binding)
                                 (mal-eval (snd Binding) AccEnv))
                        AccEnv))
       Bindings (list->assoc Bindings)
       LetEnv   (fold-left Folder (make-env Env) Bindings)
    (mal-eval Form LetEnv)))

(define eval-list
  [ def! Symbol   Value ] Env -> (set-env Env Symbol (mal-eval Value Env))
  [ def! | Tail         ] Env -> (error "Encountered malformed def!")

  [ let* Bindings Form  ] Env -> (eval-let* Bindings Form Env)
  [ let* | Tail         ] Env -> (error "Encountered malformed let*")

  Ast                     Env -> (let Evaluated (eval-ast Ast Env)
                                   (mal-apply (head Evaluated) (tail Evaluated))))

(define mal-eval
  []  Env -> []
  Ast Env -> (eval-list Ast Env)                where (list? Ast)
  Ast Env -> (eval-ast Ast Env))

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