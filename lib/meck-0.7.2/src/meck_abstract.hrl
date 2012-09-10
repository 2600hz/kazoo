-define(call(Module, Function, Arguments),
        {call, ?LINE,
         {remote, ?LINE, ?atom(Module), ?atom(Function)},
        Arguments}).

-define(atom(Atom), {atom, ?LINE, Atom}).

-define(integer(Integer), {integer, ?LINE, Integer}).

-define(var(Name), {var, ?LINE, Name}).

-define(attribute(Attribute, Args), {attribute, ?LINE, Attribute, Args}).

-define(function(Name, Arity, Clauses),
        {function, ?LINE, Name, Arity, Clauses}).

-define(clause(Arguments, Body), {clause, ?LINE, Arguments, [], Body}).

-define(tuple(Elements), {tuple, ?LINE, Elements}).
