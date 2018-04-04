(package types (append [nil nil?]
                       [list?]
                       [intern-keyword keyword? keyword->string]
                       \* Builtin function constructor *\
                       [mal-fn? mal-fn mal-fn-meta mal-fn-closure]
                       [mal-builtin-fn? mal-builtin-fn]
                       [mal-macro? mal-macro]
                       [& fn builtin-fn]
                       [string->keyword keyword? keyword->string]
                       [atom atom? deref reset! swap!]
                       [list? sequence? truthy?]
                       [fold-args mal-apply]
                       [copy]
                       [assoc->dict dict->assoc]
                       [list->vector vector->list])

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
  _               -> (error "'intern-keyword' passed a non-string argument"))

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

(define mal-macro
  (@p fn Fn) -> (do (put Fn macro true) (@p fn Fn))
  _          -> (error "Cannot convert non-function value to a macro"))

(define mal-macro?
  (@p fn Fn) -> (get Fn macro)
  _          -> false)

(define mal-apply
  (@p fn         Fn)      Args -> ((get Fn fn-closure) Args)
  (@p builtin-fn Closure) Args -> (Closure Args)
  _                         _  -> (error "'mal-apply' passed a non-function"))

\* Atom type *\
(define atom
  Val -> (@p atom (@v Val <>)))

(define atom?
  (@p atom _) -> true
  _           -> false)

(define deref
  (@p atom (@v Val _)) -> Val
  _                    -> (error "'deref' called on non-atom"))

(define reset!
  (@p atom Vector) Val -> (do (vector-> Vector 1 Val) Val)
  _                _   -> (error "'reset!' called on non-atom"))

(define swap!
  (@p atom Vector) Func Args -> (let Val    (<-vector Vector 1)
                                     NewVal (mal-apply Func [ Val | Args ])
                                  (do (vector-> Vector 1 NewVal) NewVal))
  _                _    _    -> (error "'swap!' called on non-atom"))

(define copy
  List               -> (map (/. X X) List)                  where (list? List)
  Vector             -> (list->vector (vector->list Vector)) where (vector? Vector)
  Dict               -> (assoc->dict (dict->assoc Dict))     where (dict? Dict)
  (@p fn Fn)         -> (let NewFn (gensym mal-fn)
                          (do (put NewFn fn-closure (get Fn fn-closure))
                              (put NewFn fn-meta    (get Fn fn-meta))
                              (put NewFn macro      (get Fn macro))
                              (@p fn NewFn)))
  (@p builtin-fn Fn) -> (@p builtin-fn Fn))
)
