(package types (append [nil nil?]
                       [list?]
                       [intern-keyword keyword? keyword->string])

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

)
