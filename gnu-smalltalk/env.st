Object subclass: MalEnvironment [
    | data parent |

    MalEnvironment class >> inheritingFrom: env [
        ^ (MalEnvironment new) parent: env
    ]

    MalEnvironment class >> from: bindings [
        | environment |

        environment := MalEnvironment new.
        bindings do: [ :association |
            environment set: (association key) to: (association value)
        ].

        ^ environment
    ]

    initialize [
        data := LookupTable new
    ]

    parent: env [
        parent := env
    ]

    set: symbol to: value [
        data at: symbol put: value.
        ^ value
    ]

    find: symbol [
        ^ data at: symbol ifAbsent: [
            parent isNil ifFalse: [
                parent find: symbol
            ]
        ]
    ]

    get: symbol [
        | value |

        value := self find: symbol.

        ^ value isNil ifFalse: [ value ] ifTrue: [
            self error: 'Undefined symbol ''%1''' % { symbol asString }
        ]
    ]
]
