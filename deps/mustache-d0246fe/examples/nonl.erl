-module(nonl).
-compile(export_all).

name() ->
  "Tom".

value() ->
  "10000".

taxed_value() ->
  integer_to_list(value() - (value() * 0.4)).

in_ca() ->
  true.
