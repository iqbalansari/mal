(define mal-read Str -> Str)

(define mal-eval Ast -> Ast)

(define mal-print Ast -> Ast)

(define rep Str -> (mal-print (mal-eval (mal-read Str))))

(define main ->
  (let input (prompt/or "user> " (freeze nil))
    (if (= input nil)
        nil
        (do (println (rep input)) (main)))))
