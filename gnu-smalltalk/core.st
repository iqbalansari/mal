Smalltalk at: #MalNs put: {
    #+         -> (MalBuiltinFunction from: [ :x :y | x + y ]) .
    #-         -> (MalBuiltinFunction from: [ :x :y | x - y ]) .
    #*         -> (MalBuiltinFunction from: [ :x :y | x * y ]) .
    #/         -> (MalBuiltinFunction from: [ :x :y | x / y ]) .
    #list      -> (MalBuiltinFunction from: [ :rest | rest ]) .
    #'list?'   -> (MalBuiltinFunction from: [ :el   | el class = List ]) .
    #'empty?'  -> (MalBuiltinFunction from: [ :list | list size = 0 ]) .
    #count     -> (MalBuiltinFunction from: [ :list | list size ]) .
    #=         -> (MalBuiltinFunction from: [ :x :y | x = y ]) .
    #<         -> (MalBuiltinFunction from: [ :x :y | x < y ]) .
    #<=        -> (MalBuiltinFunction from: [ :x :y | x <= y ]) .
    #>         -> (MalBuiltinFunction from: [ :x :y | x > y ]) .
    #>=        -> (MalBuiltinFunction from: [ :x :y | x >= y ]) .
    #'pr-str'  -> (MalBuiltinFunction from: [ :rest | (rest collect: [ :element | element malPrint: true ]) malJoinStr: ' ' ]) .
    #'str'     -> (MalBuiltinFunction from: [ :rest | (rest collect: [ :element | element malPrint: false ]) malJoinStr: '' ]) .
    #'prn'     -> (MalBuiltinFunction from: [ :rest | | repr |
                                              repr := (rest collect: [ :element | element malPrint: true ]) malJoinStr: ' '.
                                              repr displayNl.
                                              nil
                                            ]) .
    #'println' -> (MalBuiltinFunction from: [ :rest | | repr |
                                              repr := (rest collect: [ :element | element malPrint: false ]) malJoinStr: ' '.
                                              repr displayNl.
                                              nil
                                            ]) .
}
