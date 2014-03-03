%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(caller10_maintenance).

-export([is_running/0
         ,refresh_contests_db/0
        ]).

is_running() ->
    case lists:keyfind('caller10', 1, application:which_applications()) of
        'false' -> io:format("Caller10 is not currently running on this node~n", []);
        {_App, _Desc, _Vsn} ->
            io:format("Caller10 (~s) is running~n", [_Vsn])
    end.

-spec refresh_contests_db() -> 'ok'.
refresh_contests_db() ->
    case couch_mgr:db_exists(<<"contests">>) of
        'true' -> refresh_view();
        'false' ->
            init_db(),
            refresh_view()
    end,
    'ok'.

-spec init_db() -> boolean().
init_db() ->
    io:format("creating 'contests' aggregate database~n", []),
    couch_mgr:db_create(<<"contests">>).

-spec refresh_view() -> {'ok', wh_json:object()} |
                        {'error', _}.
refresh_view() ->
    io:format("refreshing contests design doc~n", []),
    couch_mgr:revise_doc_from_file(<<"contests">>, 'crossbar', <<"views/contests.json">>).
