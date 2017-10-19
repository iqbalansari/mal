(package env (append [nil]
                     [make-env assoc->env bindings->env]
                     [set-env find-env get-env]
                     [fold-args]
                     [assoc->dict])

(define make-env
  Outer -> [env (dict 4) Outer])

(define assoc->env
  Assoc Outer -> [env (assoc->dict Assoc) Outer])

(define bindings->env
  Params Args Outer -> (assoc->env (fold-args Params Args) Outer))

(define set-env
  [ env Data _ ] Symbol Value -> (dict-> Data Symbol Value))

(define find-env
  [ env Data nil   ] Symbol -> (<-dict/or Data Symbol (freeze unbound))
  [ env Data Outer ] Symbol -> (<-dict/or Data Symbol (freeze (find-env Outer Symbol))))

(define get-env
  Env Symbol -> (let Val (find-env Env Symbol)
                  (if (= Val unbound)
                      (error "'~A' not found" Symbol)
                      Val)))

)
