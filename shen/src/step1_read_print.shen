(define mal-read Str -> (read-str Str))

(define mal-eval Ast -> Ast)

(define mal-print Ast -> (pr-str Ast true))

(define rep Str -> (mal-print (mal-eval (mal-read Str))))

(define main ->
  (let input (prompt/or "user> " (freeze nil))
    (if (= input nil)
        nil
        (do (trap-error (println (rep input))
                        (/. E (println (error-to-string E))))
            (main)))))
