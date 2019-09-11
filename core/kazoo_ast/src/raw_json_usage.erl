%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc Checks files to find raw usage of JSON.
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(raw_json_usage).

-export([process_project/0
        ,process_app/1
        ,process_module/1
        ]).

-include_lib("kazoo_ast/include/kz_ast.hrl").

-define(ACC(CurrentModuleLines, ModulesAndLines), {CurrentModuleLines, ModulesAndLines}).

-spec process_project() -> [{module(), list()}].
process_project() ->
    Options = [{'accumulator', ?ACC([], [])}
              ,{'module', fun maybe_skip_kz_json/2}
              ,{'after_module', fun print_dot/2}
              ,{'expression', fun raw_json_in_expression/2}
              ],
    io:format("processing raw_json: "),
    ?ACC(_, Raw) = kazoo_ast:walk_project(Options),
    io:format(" done~n"),
    lists:keysort(1, Raw).

-spec process_app(atom()) -> [{module(), list()}].
process_app(App) ->
    Options = [{'accumulator', ?ACC([], [])}
              ,{'module', fun maybe_skip_kz_json/2}
              ,{'after_module', fun print_dot/2}
              ,{'expression', fun raw_json_in_expression/2}
              ],
    ?ACC(_, Raw) = kazoo_ast:walk_app(App, Options),
    lists:keysort(1, Raw).

-spec process_module(module()) -> [{module(), list()}].
process_module(Module) when is_atom(Module) ->
    Options = [{'accumulator', ?ACC([], [])}
              ,{'module', fun maybe_skip_kz_json/2}
              ,{'after_module', fun print_dot/2}
              ,{'expression', fun raw_json_in_expression/2}
              ],
    ?ACC(_, Raw) = kazoo_ast:walk_modules([Module], Options),
    lists:keysort(1, Raw).

maybe_skip_kz_json('kz_json', Acc) ->
    {'skip', Acc};
maybe_skip_kz_json(_Module, Acc) ->
    Acc.

print_dot(_Module, ?ACC([], _)=Acc) ->
    io:format("."),
    Acc;
print_dot(Module, ?ACC(Lines, ModulesAndLines)) ->
    io:format("?"),
    ?ACC([], [{Module, lists:usort(Lines)} | ModulesAndLines]).

raw_json_in_expression(?TUPLE([?EMPTY_LIST])=Tuple, Acc) ->
    json_detected(Tuple, Acc);
raw_json_in_expression(?TUPLE([?LIST(H, T)]), Acc) ->
    is_list_json(H, T, Acc);
raw_json_in_expression(?TUPLE([?VAR(_Name)])=Tuple, Acc) ->
    json_detected(Tuple, Acc);
raw_json_in_expression(_Expression, Acc) ->
    Acc.

is_list_json(?TUPLE([_Key, _Value])=Tuple, ?EMPTY_LIST, Acc) ->
    json_detected(Tuple, Acc);
is_list_json(_El, ?EMPTY_LIST, Acc) -> Acc;
is_list_json(?TUPLE([_Key, _Value]), ?LIST(Head, Tail), Acc) ->
    is_list_json(Head, Tail, Acc);
is_list_json(?TUPLE([_Key, _Value])=Tuple, ?VAR(_Name), Acc) ->
    json_detected(Tuple, Acc).

json_detected(Tuple, ?ACC(Lines, ModulesWithLines)) ->
    ?ACC([element(2, Tuple) | Lines], ModulesWithLines).
