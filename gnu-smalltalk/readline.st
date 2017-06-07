DLD addLibrary: 'libreadline'.

Object subclass: ReadLine [
    read: prompt [
        <cCall: 'readline' returning: #string args: #(#string)>
    ]
]
