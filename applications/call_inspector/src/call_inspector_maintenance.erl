%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(call_inspector_maintenance).

-export([list_active_parsers/0
        ,stop_active_parser/1
        ,start_freeswitch_parser/3
        ,start_kamailio_parser/3
        ,start_hep_parser/2
        ]).
-export([flush/0
        ,flush/1
        ]).
-export([callid_details/1
        ,inspect_call_id/1
        ]).

-include("call_inspector.hrl").

%% API

-spec list_active_parsers() -> 'no_return'.
list_active_parsers() ->
    Ids = ci_parsers_sup:children(),
    lists:foreach(fun (Id) -> io:format("~p\n", [Id]) end, Ids),
    'no_return'.

-spec stop_active_parser(kz_term:text()) -> 'ok'.
stop_active_parser(Id)
  when not is_atom(Id) ->
    stop_active_parser(
      ci_parsers_util:make_name(
        kz_term:to_binary(Id)
       )
     );
stop_active_parser(Id)
  when is_atom(Id) ->
    ci_parsers_sup:stop_child(Id).

-spec start_freeswitch_parser(kz_term:text(), kz_term:text(), kz_term:text()) -> 'no_return'.
start_freeswitch_parser(Filename, LogIP, LogPort) ->
    Args = [{'parser_args', Filename, kz_term:to_binary(LogIP), kz_term:to_integer(LogPort)}],
    case ci_parsers_sup:start_child('ci_parser_freeswitch', Args) of
        {'ok', Name} ->
            io:format("started ~p\n", [Name]),
            'no_return';
        {'error', Reason} ->
            io:format("failed to start parser: ~p\n", [Reason]),
            'no_return'
    end.

-spec start_kamailio_parser(kz_term:text(), kz_term:text(), kz_term:text()) -> 'no_return'.
start_kamailio_parser(Filename, LogIP, LogPort) ->
    Args = [{'parser_args', Filename, kz_term:to_binary(LogIP), kz_term:to_integer(LogPort)}],
    case ci_parsers_sup:start_child('ci_parser_kamailio', Args) of
        {'ok', Name} ->
            io:format("started ~p\n", [Name]),
            'no_return';
        {'error', Reason} ->
            io:format("failed to start parser: ~p\n", [Reason]),
            'no_return'
    end.

-spec start_hep_parser(kz_term:text(), kz_term:text()) -> 'no_return'.
start_hep_parser(IP, Port) ->
    Args = [{'parser_args', kz_term:to_binary(IP), kz_term:to_integer(Port)}],
    case ci_parsers_sup:start_child('ci_parser_hep', Args) of
        {'ok', Name} ->
            io:format("started ~p\n", [Name]),
            'no_return';
        {'error', Reason} ->
            io:format("failed to start parser: ~p\n", [Reason]),
            'no_return'
    end.

-spec flush() -> 'ok'.
flush() -> ci_datastore:flush().

-spec flush(kz_term:text()) -> 'ok'.
flush(CallId) -> ci_datastore:flush(CallId).

-spec callid_details(kz_term:text()) -> 'no_return'.
callid_details(CallId) ->
    Props = [{<<"Call-ID">>, kz_term:to_binary(CallId)}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    case kz_amqp_worker:call_collect(Props
                                    ,fun kapi_inspector:publish_lookup_req/1
                                    ,{call_inspector, fun kapi_inspector:lookup_resp_v/1, true}
                                    )
    of
        {ok, JObjs} -> print_jobjs(JObjs);
        {timeout, []} ->
            io:format("Not found: \"~s\"\n", [CallId]);
        {timeout, JObjs} ->
            io:format("Partial results for \"~s\"\n", [CallId]),
            print_jobjs(JObjs);
        {error, _Reason}=Error ->
            io:format("Error: ~p\n", [Error])
    end,
    no_return.

-spec inspect_call_id(kz_term:ne_binary()) -> no_return.
inspect_call_id(CallId) ->
    Req = [{<<"Call-ID">>, CallId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kz_amqp_worker:call(Req
                            ,fun kapi_inspector:publish_lookup_req/1
                            ,fun kapi_inspector:lookup_resp_v/1
                            )
    of
        {ok, JObj} ->
            Chunks   = sanitize(kz_json:get_value(<<"Chunks">>, JObj, [])),
            Analysis = sanitize(kz_json:get_value(<<"Analysis">>, JObj, [])),
            Response = kz_json:from_list(
                         [{<<"call-id">>, CallId}
                         ,{<<"messages">>, Chunks}
                         ,{<<"dialog_entities">>, kz_json:get_value(<<"Dialog-Entities">>, JObj, [])}
                         ,{<<"analysis">>, Analysis}
                         ]
                        ),
            io:format("~s\n", [kz_json:encode(Response)]);
        {timeout, _Resp} ->
            io:format("timeout: ~s\n~s\n", [CallId, kz_json:encode(_Resp)]);
        {error, _E} ->
            io:format("error: ~s\n~p\n", [CallId, _E])
    end,
    no_return.

-spec sanitize(kz_json:objects()) -> kz_json:objects().
sanitize(JObjs) ->
    [kz_json:delete_key(<<"call-id">>, JObj) || JObj <- JObjs].

%% Internals

print_jobjs(JObjs) ->
    GetChunks = fun (JObj) -> kz_json:get_value(<<"Chunks">>, JObj, kz_json:new()) end,
    JSONArray = lists:flatmap(GetChunks, JObjs),
    ok = io:fwrite(io_lib:format("~ts\n", [kz_json:encode(JSONArray)])).

%% End of Module.
