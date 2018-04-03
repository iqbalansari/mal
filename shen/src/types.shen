(package types (append [nil nil?]
                       [list?]
                       [intern-keyword keyword? keyword->string]
                       [mal-fn mal-fn? mal-builtin-fn mal-builtin-fn?]
                       [& fn builtin-fn]
                       [list? sequence? truthy?]
                       [fold-args mal-apply])

\* In shen it seems [1] is a cons but not [], this consistent with rest of the
   system since shen gives the following

   For empty function application

   (0-) ()
   []

   For empty list

   (1-) []
   []

   So [] might be any of the above. However we assume [] is always a list
*\
(define list?
  Element -> (or (cons? Element) (= Element [])))

(define sequence?
  Element -> (or (list? Element) (vector? Element)))

(define nil?
  Element -> (= Element nil))

\* Keyword type *\
(define intern-keyword
  (@s ":" String) -> (@p keyword (@s ":" String))
  String          -> (@p keyword (@s ":" String)) where (string? String)
  _               -> (error "'string->keyword' passed a non-string argument"))

(define keyword?
  (@p keyword _) -> true
  _              -> false)

(define keyword->string
  (@p keyword String) -> String
  _                   -> (error "'keyword->string' provided a non-keyword argument"))

(define truthy?
  false -> false
  nil   -> false
  _     -> true)

\* Function types *\

(define fold-args-h
  [ ]                [ ]            Acc -> (reverse Acc)
  [ & Rest ]         Args           Acc -> (reverse [ (@p Rest Args) | Acc ])
  [ Param | Params ] [ Arg | Args ] Acc -> (fold-args-h Params
                                                        Args
                                                        [ (@p Param Arg ) | Acc ])
  [ ]                _              Acc -> (error "Unexpected number of arguments provided")
  _                  [ ]            Acc -> (error "Unexpected number of arguments provided"))

(define fold-args
  Params Args -> (fold-args-h Params Args []))

(define mal-fn
  Closure -> (@p fn Closure))

(define extract-args
  []                  _    -> []
  [ cons A   []     ] Args -> [ [ hd Args ] ]
  [ cons A   Params ] Args -> [ [ hd Args ] | (extract-args Params [ tl Args ])]
  [ Param  | Params ] Args -> [ [ hd Args ] | (extract-args Params [ tl Args ])])

(define deconsify
  [ ]            -> [ ]
  [ cons A Tail] -> [ A | (deconsify Tail)])

(defmacro mal-builtin-fn-macro
  [mal-builtin-fn Signature Func] ->
  (let Arguments (filter (/. X (not (= X &))) (deconsify Signature))
       Params    (gensym (protect P))
       Folded    (gensym (protect F))
    [@p builtin-fn
        [lambda Params
          [let Folded [map snd [fold-args Signature Params]]
            [Func | (extract-args Arguments Folded)]]]]))

(define mal-builtin-fn?
  (@p builtin-fn _) -> true
  _                 -> false)

(define mal-fn
  Closure Meta -> (let Fn (gensym mal-fn)
                    (do (put Fn fn-closure Closure)
                        (put Fn fn-meta    Meta)
                        (put Fn macro      false)
                        (@p fn Fn))))

(define mal-fn-closure
  (@p fn Fn) -> (get Fn fn-closure)
  Any        -> (error "'~A' is not a mal-fn" Any))

(define mal-fn-meta
  (@p fn Fn) -> (get Fn fn-meta)
  Any        -> (error "'~A' is not a mal-fn" Any))

(define mal-fn?
  (@p fn _) -> true
  _         -> false)

(define mal-apply
  (@p fn         Fn)      Args -> ((get Fn fn-closure) Args)
  (@p builtin-fn Closure) Args -> (Closure Args)
  _                         _  -> (error "'mal-apply' passed a non-function"))

)
