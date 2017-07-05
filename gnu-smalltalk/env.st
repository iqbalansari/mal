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

    MalEnvironment class >> bind: parameters exprs: arguments inheritingFrom: env [
        | environment regularParams restParam |

        environment := (MalEnvironment new) parent: env.
        regularParams := parameters copyUpTo: #&.
        restParam := parameters at: (regularParams size + 2) ifAbsent: [ nil ].

        regularParams keysAndValuesDo: [ :index :parameter |
            | argument |

            argument := arguments at: index ifAbsent: [
                self error: 'Wrong number of arguments provided'
            ].
            environment set: parameter to: argument.
        ].

        restParam ifNotNil: [
            | argument |

            argument := List from: (arguments copyFrom: (regularParams size + 1)).
            environment set: restParam to: argument
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
