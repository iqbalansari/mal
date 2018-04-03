(set *repl-env* (assoc->env (value ns) nil))

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
  \* nil is a symbol in shen so handle it explicitly *\
  nil     Env -> nil
  Keyword Env -> Keyword                     where (keyword? Keyword)
  Symbol  Env -> (get-env Env Symbol)        where (symbol? Symbol)
  List    Env -> (eval-sequence List Env)    where (list?   List)
  Vector  Env -> (eval-sequence Vector Env)  where (vector? Vector)
  HashMap Env -> (eval-hash-map HashMap Env) where (dict?   HashMap)
  Any     Env -> Any)

(define mal-read
  Str -> (read-str Str))

(define mal-eval
  []  Env -> []
  Ast Env -> (eval-ast Ast Env) where (not (list? Ast))
  [ def! Symbol Value      ] Env -> (set-env Env Symbol (mal-eval Value Env))
  [ def! | Tail            ] Env -> (error "Encountered malformed def!")

  [ let* Bindings Form     ] Env -> (mal-eval [let* (vector->list Bindings) Form ] Env) where (vector? Bindings)
  [ let* Bindings Form     ] Env -> (let Folder   (/. AccEnv Binding
                                                      (do (set-env AccEnv
                                                                   (fst Binding)
                                                                   (mal-eval (snd Binding) AccEnv))
                                                          AccEnv))
                                         Bindings (list->assoc Bindings)
                                         LetEnv   (fold-left Folder (make-env Env) Bindings)
                                      (mal-eval Form LetEnv))
  [ let* | Tail            ] Env -> (error "Encountered malformed let*")

  [ if Pred IfTrue IfFalse ] Env -> (if (truthy? (mal-eval Pred Env))
                                        (mal-eval IfTrue Env)
                                        (mal-eval IfFalse Env))
  [ if Pred IfTrue         ] Env -> (mal-eval [if Pred IfTrue nil] Env)

  [ do                     ] Env -> nil
  [ do | Forms             ] Env -> (last (eval-ast Forms Env))

  [ fn* Binds Form         ] Env -> (mal-eval [ fn* (vector->list Binds) Form ] Env)    where (vector? Binds)
  [ fn* Binds Form         ] Env -> (mal-fn (/. Exprs
                                                (mal-eval Form
                                                          (bindings->env Binds
                                                                         Exprs
                                                                         Env)))
                                            (@p Form
                                                Binds
                                                Env))

  Ast                        Env -> (let Evaluated (eval-ast Ast Env)
                                         Func      (head Evaluated)
                                         Args      (tail Evaluated)
                                         IsBuiltin (mal-builtin-fn? Func)
                                      (if IsBuiltin
                                          (mal-apply Func Args)
                                          (let Meta  (mal-fn-meta Func)
                                               Form  (fst Meta)
                                               Binds (fst (snd Meta))
                                               Env   (snd (snd Meta))
                                            (mal-eval Form
                                                      (assoc->env (fold-args Binds Args)
                                                                  Env))))))

(define mal-print
  Ast -> (pr-str Ast true))

(define rep
  Str -> (mal-print (mal-eval (mal-read Str) (value *repl-env*))))

(rep "(def! not (fn* (a) (if a false true)))")

(define main ->
  (let input (prompt/or "user> " (freeze nil))
    (if (= input nil)
        nil
        (do (trap-error (println (rep input))
                        (/. E (println (error-to-string E))))
            (main)))))
