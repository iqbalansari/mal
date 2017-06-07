Object subclass: MAL [
    read: string [
        ^ string
    ]
    print: string [
        ^ string
    ]
    eval: string [
        ^ string
    ]
]

Object subclass: REPL [
    | mal |

    initialize [
        mal := MAL new
    ]

    printOutput: string [
        stdout nextPutAll: string ; flush.
    ]
    printOutputNl: string [
        self printOutput: string ; printOutput: (String with: Character nl).
    ]

    readInput: prompt [
        self printOutput: prompt.
        ^ stdin nextLine
    ]

    rep: input [
        ^ mal print: (mal eval: (mal read: input))
    ]

    loop [
        | input |

        input := self readInput: 'user> '.
        (input isNil) ifTrue: [ ^ self ].

        self printOutputNl: (self rep: input).
        self loop
    ]
]

(REPL new) loop.

ObjectMemory quit
