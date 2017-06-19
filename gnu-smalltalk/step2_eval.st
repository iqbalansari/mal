FileStream fileIn: 'readline.st'.
FileStream fileIn: 'types.st'.
FileStream fileIn: 'reader.st'.

Object subclass: MAL [
    | reader evaluators |

    initialize [
        reader := Reader new.
        evaluators := Dictionary from: {
            Symbol -> #evalSymbol:in: .
            List -> #evalList:in: .
            Vector -> #evalVector:in: .
            Dictionary -> #evalDictionary:in:
        }
    ]

    evalSymbol: symbol in: environment [
        ^ environment at: symbol ifAbsent: [
            self error: 'Undefined symbol ''%1''' % { symbol asString }
        ]
    ]

    evalVector: vector in: environment [
        ^ vector collect: [ :element | self eval: element in: environment ]
    ]

    evalDictionary: dictionary in: environment [
        ^ dictionary collect: [ :value | self eval: value in: environment ]
    ]

    evalList: list in: environment [
        | evaluatedList |

        list isEmpty ifTrue: [ ^ list ].

        evaluatedList := list collect: [ :element | self eval: element in: environment ].
        ^ evaluatedList first malApply: evaluatedList allButFirst asArray
    ]

    evalAtom: atom in: environment [
        ^ atom
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
        ^ self perform: evaluator with: ast with: environment
    ]
]

Object subclass: REPL [
    | mal readline basicEnvironment |

    initialize [
        mal := MAL new.
        readline := ReadLine new.
        basicEnvironment := Dictionary from: {
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
