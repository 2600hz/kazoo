%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
%%% @doc
%%% Preforms maintenance operations against the stepswitch dbs
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(stepswitch_maintenance).

-include("stepswitch.hrl").

-export([resources/0]).
-export([reverse_lookup/1]).
-export([flush/0]).
-export([refresh/0]).
-export([lookup_number/1]).
-export([reload_resources/0]).
-export([process_number/1
         ,process_number/2
        ]).
-export([emergency_cid/1
         ,emergency_cid/2
         ,emergency_cid/3
        ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
reverse_lookup(Thing) when not is_binary(Thing) ->
    reverse_lookup(wh_util:to_binary(Thing));
reverse_lookup(Thing) ->
    JObj = wh_json:from_list([{<<"From-Network-Addr">>, Thing}
                              ,{<<"Auth-Realm">>, Thing}
                             ]),
    case stepswitch_resources:reverse_lookup(JObj) of
        {'ok', Props} -> pretty_print_lookup(Props);
        {'error', 'not_found'} -> io:format("resource not found~n")
    end.

pretty_print_lookup([]) -> 'ok';
pretty_print_lookup([{Key, Value}|Props]) -> 
    io:format("~-19s: ~s~n", [Key, wh_util:to_binary(Value)]),
    pretty_print_lookup(Props).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
resources() ->
    Props = stepswitch_resources:get_props(),
    pretty_print_resources(Props).

pretty_print_resources([]) -> 'ok';
pretty_print_resources([Resource|Resources]) ->
    _ = pretty_print_resource(Resource),
    io:format("~n"),
    pretty_print_resources(Resources).

pretty_print_resource([]) -> 'ok';
pretty_print_resource([{_, []}|Props]) -> 
    pretty_print_resource(Props);    
pretty_print_resource([{Key, Values}|Props]) when is_list(Values) -> 
    _ = pretty_print_resource(Props),
    io:format("~s~n", [Key]),
    print_condensed_list(Values);
pretty_print_resource([{Key, Value}|Props]) -> 
    io:format("~-19s: ~s~n", [Key, wh_util:to_binary(Value)]),
    pretty_print_resource(Props).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Flush the stepswitch local cache
%% @end
%%--------------------------------------------------------------------
-spec flush() -> 'ok'.
flush() -> wh_cache:flush_local(?STEPSWITCH_CACHE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Lookup a number in the route db and return the account ID if known
%% @end
%%--------------------------------------------------------------------
-spec refresh() -> 'ok'.
refresh() ->
    lager:debug("ensuring database ~s exists", [?RESOURCES_DB]),
    couch_mgr:db_create(?RESOURCES_DB),
    Views = whapps_util:get_views_json('stepswitch', "views"),
    whapps_util:update_views(?RESOURCES_DB, Views, 'true'),
    case catch couch_mgr:all_docs(?RESOURCES_DB, ['include_docs']) of
        {'error', _} -> 'ok';
        {'EXIT', _E} ->
            lager:debug("failure looking up all docs in ~s: ~p", [?RESOURCES_DB, _E]);
        {'ok', JObjs} ->
            _ = couch_mgr:del_docs(?RESOURCES_DB
                                   ,[Doc
                                     || JObj <- JObjs,
                                        begin
                                            Doc = wh_json:get_value(<<"doc">>, JObj),
                                            wh_json:get_value(<<"pvt_type">>, Doc) =:= <<"route">>
                                        end
                                    ]),
            'ok'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Lookup a number in the route db and return the account ID if known
%% @end
%%--------------------------------------------------------------------
-spec lookup_number(string()) -> any().
lookup_number(Number) ->
    case stepswitch_util:lookup_number(Number) of
        {'ok', AccountId, Props} ->
            io:format("~-19s: ~s~n", [<<"Account-ID">>, AccountId]),
            pretty_print_number_props(Props);
        {'error', 'not_found'} ->
            io:format("number not found~n")
    end.

-spec pretty_print_number_props(wh_proplist()) -> 'ok'.
pretty_print_number_props([]) -> 'ok';
pretty_print_number_props([{Key, Value}|Props]) -> 
    io:format("~-19s: ~s~n", [wh_util:to_binary(Key)
                              ,wh_util:to_binary(Value)
                             ]),
    pretty_print_number_props(Props).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Instructs stepswitch_outbound to re-scan the resource db and
%% refresh the cache.
%% @end
%%--------------------------------------------------------------------
-spec reload_resources() -> 'ok'.
reload_resources() -> 'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec process_number(string()) -> any().
process_number(Number) -> process_number(Number, 'undefined').
    
-spec process_number(string(), string()) -> any().
process_number(Number, AccountId) when not is_binary(Number) ->
    process_number(wh_util:to_binary(Number), AccountId);
process_number(Number, 'undefined') ->
    Endpoints = stepswitch_resources:endpoints(Number, wh_json:new()),
    pretty_print_endpoints(Endpoints);
process_number(Number, AccountId) when not is_binary(AccountId) ->
    process_number(Number, wh_util:to_binary(AccountId));
process_number(Number, AccountId) ->
    JObj = wh_json:from_list([{<<"Account-ID">>, AccountId}
                              ,<<"Hunt-Account-ID">>, AccountId
                             ]),
    Endpoints = stepswitch_resources:endpoints(Number, JObj),
    pretty_print_endpoints(Endpoints).

-spec pretty_print_endpoints(wh_proplists()) -> any(). 
pretty_print_endpoints([]) -> 'ok';
pretty_print_endpoints([Endpoint|Endpoints]) ->
    _ = pretty_print_endpoint(Endpoint),
    io:format("~n"),
    pretty_print_endpoints(Endpoints).

-spec pretty_print_endpoint(wh_proplist()) -> any(). 
pretty_print_endpoint([]) -> 'ok';
pretty_print_endpoint([{_, []}|Props]) -> 
    pretty_print_endpoint(Props);
pretty_print_endpoint([{Key, Values}|Props]) when is_list(Values) ->
    _ = pretty_print_endpoint(Props),
    io:format("~s~n", [Key]),
    print_condensed_list(Values);
pretty_print_endpoint([{<<"Custom-Channel-Vars">>, JObj}|Props]) ->
    _ = pretty_print_endpoint(Props),
    io:format("Custom-Channel-Vars~n"),
    [io:format("    ~-15s: ~s~n", [Key
                                   ,wh_util:to_binary(Value)
                                  ])
     || {Key, Value} <- wh_json:to_proplist(JObj)
    ];
pretty_print_endpoint([{Key, Value}|Props]) -> 
    io:format("~-19s: ~s~n", [Key, wh_util:to_binary(Value)]),
    pretty_print_endpoint(Props).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec emergency_cid(ne_binary()) -> 'ok'.
-spec emergency_cid(ne_binary(), text()) -> 'ok'.
-spec emergency_cid(ne_binary(), 'undefined' | text(), text()) -> 'ok'.

emergency_cid(Account) -> emergency_cid(Account, 'undefined').
emergency_cid(Account, ECID) -> emergency_cid(Account, ECID, 'undefined').
emergency_cid(Account, ECID, OCID) ->
    wh_cache:flush(),
    Props = [{<<"Account-ID">>, wh_util:to_binary(Account)}
             ,{<<"Emergency-Caller-ID-Number">>, ECID}
             ,{<<"Outbound-Caller-ID-Number">>, OCID}
            ],
    JObj = wh_json:from_list(props:filter_empty(Props)),
    CID = stepswitch_outbound:get_emergency_cid_number(JObj),
    lager:info("Emergency offnet requests for account ~s (given the following CIDs ~s and ~s) will use ~s"
               ,[Account, ECID, OCID, CID]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
print_condensed_list([E1, E2, E3]) ->
    io:format("    | ~-20s | ~-20s | ~-20s|~n"
              ,[wh_util:to_binary(E1)
                ,wh_util:to_binary(E2)
                ,wh_util:to_binary(E3)
               ]);
print_condensed_list([E1, E2]) ->
    io:format("    | ~-20s | ~-20s | ~-20s|~n"
              ,[wh_util:to_binary(E1)
                ,wh_util:to_binary(E2)
                ,<<>>
               ]);
print_condensed_list([E1]) ->
    io:format("    | ~-20s | ~-20s | ~-20s|~n"
              ,[wh_util:to_binary(E1)
                ,<<>>
                ,<<>>
               ]);
print_condensed_list([E1, E2, E3 | Rest]) ->
    io:format("    | ~-20s | ~-20s | ~-20s|~n"
              ,[wh_util:to_binary(E1)
                ,wh_util:to_binary(E2)
                ,wh_util:to_binary(E3)
               ]),
    print_condensed_list(Rest).
