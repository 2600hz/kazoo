-module(meck_test_parametrized_module, [Var1, Var2]).
-export([which/0, var1/0, var2/0]).

which() -> original.

var1() -> {original, Var1}.
var2() -> {original, Var2}.
