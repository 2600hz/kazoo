
-define(IN(X,Min,Max), X >= Min, X =< Max).
-define(LOWER(X), when X >= $a, X =< $z).
-define(DIGIT(X),  X >= $0, X =< $9).
