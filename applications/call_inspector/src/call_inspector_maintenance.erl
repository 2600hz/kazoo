%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
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

-spec stop_active_parser(text()) -> 'ok'.
stop_active_parser(Id)
  when not is_atom(Id) ->
    stop_active_parser(
      ci_parsers_util:make_name(
        kz_util:to_binary(Id)
       )
     );
stop_active_parser(Id)
  when is_atom(Id) ->
    ci_parsers_sup:stop_child(Id).

-spec start_freeswitch_parser(text(), text(), text()) -> 'no_return'.
start_freeswitch_parser(Filename, LogIP, LogPort) ->
    Args = [{'parser_args', Filename, kz_util:to_binary(LogIP), kz_util:to_integer(LogPort)}],
    {'ok', Name} = ci_parsers_sup:start_child('ci_parser_freeswitch', Args),
    io:format("started ~p\n", [Name]),
    'no_return'.

-spec start_kamailio_parser(text(), text(), text()) -> 'no_return'.
start_kamailio_parser(Filename, LogIP, LogPort) ->
    Args = [{'parser_args', Filename, kz_util:to_binary(LogIP), kz_util:to_integer(LogPort)}],
    {'ok', Name} = ci_parsers_sup:start_child('ci_parser_kamailio', Args),
    io:format("started ~p\n", [Name]),
    'no_return'.

-spec start_hep_parser(text(), text()) -> 'no_return'.
start_hep_parser(IP, Port) ->
    Args = [{'parser_args', kz_util:to_binary(IP), kz_util:to_integer(Port)}],
    {'ok', Name} = ci_parsers_sup:start_child('ci_parser_hep', Args),
    io:format("started ~p\n", [Name]),
    'no_return'.

-spec flush() -> 'ok'.
flush() -> ci_datastore:flush().

-spec flush(text()) -> 'ok'.
flush(CallId) -> ci_datastore:flush(CallId).

-spec callid_details(text()) -> 'no_return'.
callid_details(CallId) ->
    Props = [{<<"Call-ID">>, kz_util:to_binary(CallId)}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    case kz_amqp_worker:call_collect(Props
                                    ,fun kapi_inspector:publish_lookup_req/1
                                    ,{'call_inspector', 'true'}
                                    )
    of
        {'ok', JObjs} -> print_jobjs(JObjs);
        {'timeout', []} ->
            io:format("Not found: \"~s\"\n", [CallId]);
        {timeout, JObjs} ->
            io:format("Partial results for \"~s\"\n", [CallId]),
            print_jobjs(JObjs);
        {'error', _Reason}=Error ->
            io:format("Error: ~p\n", [Error])
    end,
    'no_return'.

-spec inspect_call_id(ne_binary()) -> no_return.
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

%% @private
-spec sanitize(kz_json:objects()) -> kz_json:objects().
sanitize(JObjs) ->
    [kz_json:delete_key(<<"call-id">>, JObj) || JObj <- JObjs].

%% Internals

print_jobjs(JObjs) ->
    GetChunks = fun (JObj) -> kz_json:get_value(<<"Chunks">>, JObj, kz_json:new()) end,
    JSONArray = lists:flatmap(GetChunks, JObjs),
    ok = io:fwrite(io_lib:format("~ts\n", [kz_json:encode(JSONArray)])).

%% End of Module.
