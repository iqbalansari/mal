Smalltalk at: #Meta put: Dictionary new.

SequenceableCollection extend [
    malJoinStr: separator [
        "Like SequenceableCollection>>join returns an empty string instead of
         empty collection is called on an empty collection"
        ^ self isEmpty ifTrue: [ '' ] ifFalse: [
            self join: separator
        ]
    ]
]

Object extend [
    malPrintOn: stream printReadably: printReadably [
        self printOn: stream
    ]

    malPrintOn: stream [
        self malPrintOn: stream printReadably: true
    ]

    malPrint: readably [
        | stream |

        stream := ReadWriteStream on: String new.
        self malPrintOn: stream printReadably: readably.

        ^ stream contents
    ]

    "
     HACK: support for meta on objects using the global Meta dictionary,
     otherwise will need to wrap all Mal types.
    "

    meta: value [
        Meta at: (self identityHash) put: value
    ]

    meta [
        ^ Meta at: (self identityHash) ifAbsent: [ nil ]
    ]
]

Symbol extend [
    malPrintOn: stream printReadably: printReadably [
        stream nextPutAll: (self asString).
    ]
]

String extend [
    malPrintOn: stream printReadably: printReadably [
        | repr |

        repr := printReadably ifTrue: [
            | string |

            stream nextPut: $".

            string := self copy.
            string := string replacingAllRegex: '\\' with: '\\'.
            string := string replacingAllRegex: '"' with: '\"'.
            string := string replacingAllRegex: (Character nl asString) with: '\n'.
            stream nextPutAll: string.

            stream nextPut: $".

        ] ifFalse: [
            stream nextPutAll: self copy
        ].
    ]
]

OrderedCollection subclass: MalSequence [
    <shape: #inherit>

    openParen [
        ^ $(
    ]

    closeParen [
        ^ $)
    ]

    malPrintOn: stream printReadably: printReadably [
        | elementsRepr |

        elementsRepr := self collect: [ :element | element malPrint: printReadably ].

        stream nextPut: self openParen.
        stream nextPutAll: (elementsRepr malJoinStr: ' ').
        stream nextPut: self closeParen
    ]

    = aSequence [

        ^ (aSequence isKindOf: MalSequence) ifTrue: [

            self size = aSequence size and: [
                (self with: aSequence) inject: true into: [ :result :element |
                    result & (element first = element second)
                ]
            ]

        ] ifFalse: [
            false
        ]
    ]
]

MalSequence subclass: List [
    <shape: #inherit>

    openParen [
        ^ $(
    ]

    closeParen [
        ^ $)
    ]
]

MalSequence subclass: Vector [
    <shape: #inherit>

    openParen [
        ^ $[
    ]

    closeParen [
        ^ $]
    ]

]

Dictionary extend [
    malPrintOn: stream printReadably: printReadably [
        | elementsRepr |

        elementsRepr := self associations collect: [ :assoc |
            (assoc key malPrint: printReadably) , ' ', (assoc value malPrint: printReadably)
        ].

        stream nextPut: ${.
        stream nextPutAll: (elementsRepr malJoinStr: ' ').
        stream nextPut: $}
    ]
]

Object subclass: Keyword [
    | symbol |

    Keyword class >> intern: string [
        ^ (self new) symbol: string
    ]

    symbol: string [
        symbol := string
    ]

    symbol [
        ^ symbol
    ]

    malPrintOn: stream printReadably: printReadably [
        stream nextPutAll: symbol.
    ]

    = aKeyword [
        ^ (aKeyword class = Keyword and: [ aKeyword symbol = self symbol ])
    ]

    hash [
        ^ self symbol hash
    ]
]

Object subclass: MalBuiltinFunction [
    | block restParamPosition |

    MalBuiltinFunction class >> from: block [
        ^ (self new) block: block
    ]

    block: aBlock [
        | parameters |

        restParamPosition := aBlock block arguments indexOf: #rest.
        block := aBlock
    ]

    malApply: arguments [
        | arglist |

        restParamPosition = 0 ifTrue: [
            arglist := arguments

        ] ifFalse: [
            | normalArgs restArg |

            normalArgs := arguments copyFrom: 1 to: restParamPosition - 1.
            restArg := arguments copyFrom: restParamPosition to: arguments size.
            arglist := normalArgs , { List from: restArg }
        ].

        ^ block valueWithArguments: arglist
    ]

    malPrintOn: stream printReadably: printReadably [
        stream nextPutAll: '#<built-in function>'
    ]
]

Object subclass: MalFunction [
    | block |

    MalFunction class >> from: block [
        ^ (self new) block: block
    ]

    block: aBlock [
        block := aBlock
    ]

    malApply: arguments [
        ^ block value: arguments
    ]

    malPrintOn: stream printReadably: printReadably [
        stream nextPutAll: '#<function>'
    ]
]
