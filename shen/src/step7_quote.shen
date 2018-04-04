(define quasiquote-h
  [ unquote                   Body ] -> Body
  [ [ splice-unquote Head ] | Tail ] -> [concat Head (quasiquote Tail) ]
  [ Head                    | Tail ] -> [cons (quasiquote Head) (quasiquote Tail) ]
  Any                                -> [quote Any])

\* quasiquote simply massages vectors to list and calls back quasiquote-h which
   does actual quoting *\
(define quasiquote
  Ast -> (let ListMaybe (seq->list Ast)
           (quasiquote-h (if (list? ListMaybe)
                             (map seq->list ListMaybe)
                             ListMaybe))))

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
  Ast Env -> (eval-ast Ast Env)                                                         where (not (list? Ast))

  [ quote       Ast        ] Env -> Ast
  [ quasiquote  Ast        ] Env -> (mal-eval (quasiquote Ast) Env)

  [ def! Symbol Value      ] Env -> (set-env Env Symbol (mal-eval Value Env))
  [ def! | Tail            ] Env -> (error "Encountered malformed def!")

  [ let* Bindings Form     ] Env -> (mal-eval [let* (vector->list Bindings) Form ] Env) where (vector? Bindings)
  [ let* Bindings Form     ] Env -> (let Folder   (/. Acc Binding
                                                      (let Param (fst Binding)
                                                           Arg (snd Binding)
                                                        (do (set-env Acc
                                                                     Param
                                                                     (mal-eval Arg Acc))
                                                            Acc)))
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

(set *repl-env* (assoc->env (append (value ns)
                                    [(@p eval
                                         (mal-builtin-fn [ast]
                                                         (/. Ast
                                                             (mal-eval Ast (value *repl-env*)))))])
                            nil))

(rep "(def! not (fn* (a) (if a false true)))")
(rep "(def! load-file (fn* (f) (eval (read-string (str c#34;(do c#34; (slurp f) c#34;)c#34;)))))")

(define loop ->
  (let input (prompt/or "user> " (freeze nil))
    (if (= input nil)
        nil
        (do (trap-error (println (rep input))
                        (/. E (println (error-to-string E))))
            (loop)))))

(define toplevel ->
  (do (set-env (value *repl-env*) *ARGV* [])
      (loop)))

(define run-file
  File ARGV -> (do (set-env (value *repl-env*) *ARGV* ARGV)
                   (rep (make-string "(load-file c#34;~Ac#34;)" File))))

(define main
  []            -> (toplevel)
  [File | Args] -> (run-file File Args))