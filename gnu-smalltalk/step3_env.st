FileStream fileIn: 'readline.st'.
FileStream fileIn: 'types.st'.
FileStream fileIn: 'reader.st'.
FileStream fileIn: 'env.st'.

Object subclass: MAL [
    | reader evaluators specialForms |

    initialize [
        reader := Reader new.

        evaluators := Dictionary from: {
            Symbol -> #evalSymbol:in: .
            List -> #evalList:in: .
            Vector -> #evalVector:in: .
            Dictionary -> #evalDictionary:in:
        }.

        specialForms := Dictionary from: {
            #'def!' -> #evalDef:in: .
            #'let*' -> #evalLet:in:
        }
    ]

    evalDef: ast in: environment [
        | variable value |

        ast size < 3 ifTrue: [
            self error: 'Extra parameters passed to ''def!'''
        ].

        variable := ast second.
        value := self eval: ast third in: environment.

        ^ environment set: variable to: value
    ]

    evalLet: ast in: environment [
        | scope bindings body |

        scope := MalEnvironment inheritingFrom: environment.
        bindings := ast second.
        body := ast third.

        (bindings size rem: 2) = 0 ifFalse: [
            self error: 'Uneven number of bindings for ''let*'''
        ].

        (Interval from: 1 to: (bindings size) by: 2) do: [ :index |
            | variable value |

            variable := bindings at: index.
            value := self eval: (bindings at: index + 1) in: scope.

            scope set: variable to: value
        ].

        ^ self eval: body in: scope
    ]

    evalAtom: atom in: environment [
        ^ atom
    ]

    evalSymbol: symbol in: environment [
        ^ environment get: symbol
    ]

    evalVector: vector in: environment [
        ^ vector collect: [ :element | self eval: element in: environment ]
    ]

    evalDictionary: dictionary in: environment [
        ^ dictionary collect: [ :value | self eval: value in: environment ]
    ]

    evalNonSpecialForm: list in: environment [
        | evaluatedList |

        evaluatedList := list collect: [ :element | self eval: element in: environment ].
        ^ evaluatedList first malApply: evaluatedList allButFirst asArray
    ]

    evalList: list in: environment [
        | evaluator |

        list isEmpty ifTrue: [ ^ list ].

        evaluator := specialForms at: (list first) ifAbsent: [ #evalNonSpecialForm:in: ].
        ^ self perform: evaluator with: list with: environment.
    ]

    read: string [
        ^ reader readForm: (TokenStream from: string)
    ]

    print: ast [
        ^ ast malPrint: true.
    ]


    eval: ast in: environment [
        | evaluator |

        evaluator := evaluators at: (ast class) ifAbsent: [ #evalAtom:in: ].
        ^ self perform: evaluator with: ast with: environment.
    ]
]

Object subclass: REPL [
    | mal readline basicEnvironment |

    initialize [
        mal := MAL new.
        readline := ReadLine new.
        basicEnvironment := MalEnvironment from: {
            #+ -> (MalBuiltinFunction from: [ :a :b | a + b ]) .
            #- -> (MalBuiltinFunction from: [ :a :b | a - b ]) .
            #* -> (MalBuiltinFunction from: [ :a :b | a * b ]) .
            #/ -> (MalBuiltinFunction from: [ :a :b | a / b ]) .
        }.
    ]

    printOutput: string [
        stdout nextPutAll: string ; flush.
    ]
    printOutputNl: string [
        self printOutput: string ; printOutput: (String with: Character nl).
    ]

    readInputRaw: prompt [
        self printOutput: prompt.
        ^ stdin nextLine
    ]

    readInput: prompt interactively: interactive [
        ^ interactive ifTrue: [ readline read: prompt ]
                      ifFalse: [ self readInputRaw: prompt]
    ]

    rep: input [
        ^ mal print: (mal eval: (mal read: input) in: basicEnvironment)
    ]

    loop: interactive [
        | input |

        input := self readInput: 'user> ' interactively: interactive.
        (input isNil) ifTrue: [ ^ self ].

        [
            self printOutputNl: (self rep: input)
        ] on: Error do: [ :error |
            Transcript show: (error messageText) ; cr.
        ].

        self loop: interactive
    ]

    start [
        | disableReadline dumbTerminal interactive |

        disableReadline := (Smalltalk getenv: 'PERL_RL') = 'false'.
        dumbTerminal := (Smalltalk getenv: 'TERM') = 'dumb'.
        interactive := (disableReadline | dumbTerminal) not.

        self loop: interactive.
    ]
]

(REPL new) start.

ObjectMemory quit
