Merl - Metaprograming in Erlang
===============================

Merl is a more user friendly interface to the `erl_syntax` module in the
standard library `syntax_tools` application, making it easy both to build
new ASTs (abstract syntax trees) from scratch and to match and decompose
existing ASTs.

To enable the full power of Merl, your module needs to include the Merl
header file:

    -include_lib("merl/include/merl.hrl").

Then, you can use `?Q(Text)` macros in your code to create ASTs or match
on existing ASTs. For example:

    Tuple = ?Q("{foo, 42}"),
    ?Q("{foo, _@Number}") = Tuple,
    Call = ?Q("foo:bar(_@Number)")

Calling `merl:print(Call)` will then print the following code:

    foo:bar(42)

The `?Q` macros turn the quoted code fragments into ASTs, and lifts
metavariables such as `_@Tuple` and `_@Number` to the level of your Erlang
code, so you can use the corresponding Erlang variables `Tuple` and `Number`
directly. This is the most straightforward way to use Merl, and in many
cases it's all you need.

You can even write case switches using `?Q` macros as patterns. For example:

    case AST of
        ?Q("{foo, _@Foo}") -> handle(Foo);
        ?Q("{bar, _@Bar}") when erl_syntax:is_integer(Bar) -> handle(Bar);
        _ -> handle_default()
    end

For the full documentation, run `make docs` and open `doc/index.html` in a
browser. For a quick look at the user guide without generating the HTML
docs, see the file `doc/overview.edoc`.
