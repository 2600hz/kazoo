-define(DEPRECATED1(Old, New),
        Old(X) -> escalus_compat:deprecated(Old, New, New(X))).

-define(DEPRECATED2(Old, New),
        Old(X, Y) -> escalus_compat:deprecated(Old, New, New(X, Y))).

-define(DEPRECATED3(Old, New),
        Old(X, Y, Z) -> escalus_compat:deprecated(Old, New, New(X, Y, Z))).
