FileStream fileIn: 'readline.st'.
FileStream fileIn: 'types.st'.
FileStream fileIn: 'reader.st'.

Object subclass: MAL [
    | reader |

    initialize [
        reader := Reader new
    ]

    read: string [
        ^ reader readForm: (TokenStream from: string)
    ]

    print: ast [
        ^ ast malPrint: true.
    ]

    eval: string [
        ^ string
    ]
]

Object subclass: REPL [
    | mal readline |

    initialize [
        mal := MAL new.
        readline := ReadLine new
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
        ^ mal print: (mal eval: (mal read: input))
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
