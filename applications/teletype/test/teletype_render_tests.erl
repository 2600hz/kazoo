%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(teletype_render_tests).

-include_lib("eunit/include/eunit.hrl").
-include("teletype.hrl").

render_preview_test_() ->
    [render(teletype_deregister, "notif__deregister.json")
    ].

render(Module, Fixture) ->
    DataJObj = kz_json:normalize(teletype_util:fixture(Fixture)),
    ?LOG_DEBUG(">>> normalized ~s", [kz_json:encode(DataJObj)]),
    Macros = Module:macros(DataJObj),
    ?LOG_DEBUG(">>> macros ~p", [Macros]),
    Rendered = teletype_templates:render(Module:id(), Macros, DataJObj),
    ?LOG_DEBUG(">>> rendered ~p", [Rendered]),
    {"Render " ++ atom_to_list(Module) ++ " using " ++ Fixture
    ,?_assertEqual(<<>>, Rendered)
    }.
