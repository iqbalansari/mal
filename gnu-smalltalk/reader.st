ReadStream subclass: TokenStream [
    | tokensRE |

    tokensRE := '[\s,]*(~@|[\[\]{}()''`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}(''"`,;)]*)'.

    TokenStream class >> from: string [
        | tokens |

        tokens := (string allOccurrencesOfRegex: tokensRE) collect: [ :rawToken |
            | token |

            token := rawToken replacingRegex: ',+$' with: ''. '$'.
            token := token replacingRegex: '^,+' with: ''.
            token trimSeparators
        ].

        tokens := tokens reject: [ :token |
            (token isEmpty) | (token startsWith: ';')
        ].

        ^ TokenStream on: tokens
    ]

    consume: token [
        self peek = token ifFalse: [
            (self next isNil) ifTrue: [
                self error: ('End of stream reached, expected ''%2''!' % { self next . token })
            ] ifFalse: [
                self error: ('Unexpected token ''%1'', expected ''%2''!' % { self next . token })
            ]
        ].

        ^ self consume
    ]

    consume [
        ^ self next
    ]
]

Object subclass: Reader [
    | atoms atomReaders sequenceReaders readerMacros |

    initialize [
        atomReaders := {
            [ :token | token startsWith: '"'] -> #readString: .
            [ :token | token matchRegex: '-?[0-9]+(.[0-9]+)?' ] -> #readNumber: .
            [ :token | token startsWith: ':'] -> #readKeyword: .
            [ :token | true ] -> #readSymbol: .
        }.

        readerMacros := Dictionary from: {
            '''' -> #quote .
            '`' -> #quasiquote .
            '~' -> #unquote .
            '~@' -> #'splice-unquote' .
            '@' -> #deref .
        }.

        sequenceReaders := Dictionary from: {
            '(' -> #readList: .
            '{' -> #readHashMap: .
            '[' -> #readVector: .
            '''' -> #readMacro: .
            '`' -> #readMacro: .
            '~' -> #readMacro: .
            '~@' -> #readMacro: .
            '@' -> #readMacro: .
            '^' -> #readMeta: .
        }.

        atoms := Dictionary from: {
            'true' -> true  .
            'false' -> false  .
            'nil' -> nil  .
        }
    ]

    readKeyword: tokens [
        ^ Keyword intern: tokens next.
    ]

    readSymbol: tokens [
        | token |

        token := tokens next.
        ^ atoms at: token ifAbsent: [ token asSymbol ].
    ]

    readNumber: tokens [
        ^ tokens next asNumber
    ]

    readString: tokens [
        | done string rawString |

        done := false.
        string := ReadStream on: (tokens next).
        rawString := OrderedCollection new.

        [ :character |
            character = $" ifFalse: [
                self error: 'Unexpected character ''%1'' encountered, expected ''"''' % { character }
            ]
        ] value: (string next).

        [ string atEnd | done ] whileFalse: [

            (string peek = $\ ) ifTrue: [

                string next.

                (string atEnd) ifTrue: [
                    self error: 'End of stream reached, expected ''"''!'
                ].

                rawString add: (string peek = $n ifTrue: [
                    string next.
                    Character nl

                ] ifFalse: [
                    string next
                ]).

            ] ifFalse: [
                | character |

                character := string next.

                (character = $") ifTrue: [
                    done := true.
                ] ifFalse: [
                    rawString add: character.
                ]

            ].
        ].

        done ifFalse: [
            self error: 'End of stream reached, expected ''"''!'
        ].

        ^ rawString asString
    ]

    readMacro: tokens [
        | quote |

        quote := tokens next.

        ^ List from: {
            readerMacros at: quote ifAbsent: [ self error: 'Unknown quote' ] .
            self readForm: tokens .
        }
    ]

    readMeta: tokens [
        | meta value |

        tokens consume: '^'.

        meta := self readForm: tokens.
        value := self readForm: tokens.

        ^ List from: { #'with-meta' . value . meta }
    ]

    readSequence: tokens startsWith: start endsWith: end [
        | stream |

        tokens consume: start.
        stream := WriteStream on: (Array new).

        [ tokens peek = end | tokens peek isNil ] whileFalse: [
            stream nextPut: (self readForm: tokens)
        ].

        tokens consume: end.

        ^ stream contents
    ]

    readList: tokens [
        ^ List from: (self readSequence: tokens startsWith: '(' endsWith: ')')
    ]

    readVector: tokens [
        ^ Vector from: (self readSequence: tokens startsWith: '[' endsWith: ']')
    ]

    readHashMap: tokens [
        | elements dictionary |

        elements := self readSequence: tokens startsWith: '{' endsWith: '}'.
        dictionary := Dictionary new.

        ( elements size \\ 2 = 0 ) ifFalse: [
            self error: 'Uneven number of elements in hashmap'
        ].

        (Interval from: 1 to: (elements size) by: 2) do: [ :index |
            | key value |

            key := elements at: index.
            value := elements at: (index + 1).

            dictionary at: key put: value
        ].

        ^ dictionary
    ]

    readAtom: tokens [
        | readerIndex reader |

        readerIndex := atomReaders findFirst: [
            :entry | (entry key) value: (tokens peek)
        ].

        reader := (atomReaders at: readerIndex) value.
        ^ self perform: reader with: tokens
    ]


    readForm: tokens [
        | reader |

        reader := sequenceReaders at: (tokens peek) ifAbsent: [ #readAtom: ].
        ^ self perform: reader with: tokens
    ]
]
