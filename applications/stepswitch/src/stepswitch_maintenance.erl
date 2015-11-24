%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2014, 2600Hz INC
%%% @doc
%%% Preforms maintenance operations against the stepswitch dbs
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(stepswitch_maintenance).

-export([resources/0]).
-export([reverse_lookup/1]).
-export([flush/0
         ,cnam_flush/0
        ]).
-export([refresh/0]).
-export([lookup_number/1
         ,number_tree/1
        ]).
-export([reload_resources/0, reload_resources/1]).
-export([process_number/1
         ,process_number/2
        ]).

-include("stepswitch.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reverse_lookup(text()) -> 'ok'.
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

-spec pretty_print_lookup(wh_proplist()) -> 'ok'.
pretty_print_lookup([]) -> 'ok';
pretty_print_lookup([{Key, Value}|Props]) ->
    io:format("~-19s: ~s~n", [Key, wh_util:to_binary(Value)]),
    pretty_print_lookup(Props).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Displays account tree when given a DID
%% @end
%%--------------------------------------------------------------------
-spec number_tree(ne_binary()) -> 'ok'.
number_tree(DID) ->
    case stepswitch_util:lookup_number(DID) of
        {'error', _} -> io:format("DID ~s was not found~n", [DID]);
        {'ok', AccountId, _Props} ->
            case couch_mgr:open_doc(?WH_ACCOUNTS_DB, AccountId) of
                {'ok', AccountDoc} -> number_tree(DID, AccountDoc);
                {'error', _E} -> io:format("failed to find account doc for ~s(~s)~n", [AccountId, DID])
            end
    end.

-spec number_tree(ne_binary(), wh_json:object()) -> 'ok'.
number_tree(DID, AccountDoc) ->
    io:format("~s tree ", [DID]),
    print_tree(kz_account:tree(AccountDoc)),
    io:format(" ~s(~s)~n", [kz_account:name(AccountDoc), wh_doc:id(AccountDoc)]).

-spec print_tree(ne_binaries()) -> 'ok'.
print_tree([]) -> 'ok';
print_tree([AccountId|Tree]) ->
    {'ok', AccountDoc} = couch_mgr:open_cache_doc(<<"accounts">>, AccountId),
    io:format(" ~s(~s) ->", [kz_account:name(AccountDoc), wh_doc:id(AccountDoc)]),
    print_tree(Tree).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec resources() -> 'ok'.
resources() ->
    Props = stepswitch_resources:get_props(),
    pretty_print_resources(Props).

-spec pretty_print_resources(list()) -> 'ok'.
pretty_print_resources([]) -> 'ok';
pretty_print_resources([Resource|Resources]) ->
    _ = pretty_print_resource(Resource),
    io:format("~n"),
    pretty_print_resources(Resources).

-spec pretty_print_resource(list()) -> 'ok'.
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

-spec cnam_flush() -> 'ok'.
cnam_flush() ->
    io:format("flushed ~p entries from cnam cache~n", [stepswitch_cnam:flush()]).

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
    Views = [whapps_util:get_view_json('crossbar', <<"views/resources.json">>)
             | whapps_util:get_views_json('stepswitch', "views")
            ],
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
                                            wh_doc:type(Doc) =:= <<"route">>
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
-spec lookup_number(text()) -> 'ok'.
lookup_number(Number) ->
    case stepswitch_util:lookup_number(Number) of
        {'ok', AccountId, Props} ->
            io:format("~-19s: ~s~n", [<<"Account-ID">>, AccountId]),
            pretty_print_number_props(
              props:insert_value(<<"classification">>
                                 ,wnm_util:classify_number(Number, AccountId)
                                 ,Props
                                )
             );
        {'error', 'not_found'} ->
            io:format("number not found~n")
    end.

-spec pretty_print_number_props(wh_proplist()) -> 'ok'.
pretty_print_number_props([]) -> 'ok';
pretty_print_number_props([{Key, Value}|Props]) ->
    io:format("~-19s: ~s~n", [wh_util:to_binary(Key), wh_util:to_binary(Value)]),
    pretty_print_number_props(Props).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Instructs stepswitch_outbound to re-scan the resource db and
%% refresh the cache.
%% @end
%%--------------------------------------------------------------------
-spec reload_resources() -> 'ok'.
reload_resources() ->
    stepswitch_resources:fetch_global_resources(),
    'ok'.

-spec reload_resources(ne_binary()) -> 'ok'.
reload_resources(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    stepswitch_resources:fetch_local_resources(AccountId),
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec process_number(text()) -> any().
process_number(Number) -> process_number(Number, 'undefined').

-spec process_number(text(), text() | 'undefined') -> any().
process_number(Num, 'undefined') ->
    JObj = wh_json:from_list([{<<"To-DID">>, wh_util:to_binary(Num)}]),
    Number = stepswitch_util:get_outbound_destination(JObj),
    Endpoints = stepswitch_resources:endpoints(Number
                                               ,wapi_offnet_resource:jobj_to_req(JObj)
                                              ),
    pretty_print_endpoints(Endpoints);
process_number(Num, AccountId) ->
    JObj = wh_json:from_list([{<<"Account-ID">>, wh_util:to_binary(AccountId)}
                              ,{<<"Hunt-Account-ID">>, wh_util:to_binary(AccountId)}
                              ,{<<"To-DID">>, wh_util:to_binary(Num)}
                             ]),
    Number = stepswitch_util:get_outbound_destination(JObj),
    Endpoints = stepswitch_resources:endpoints(Number
                                               ,wapi_offnet_resource:jobj_to_req(JObj)
                                              ),
    pretty_print_endpoints(Endpoints).

-spec pretty_print_endpoints(wh_json:objects()) -> any().
pretty_print_endpoints([]) -> 'ok';
pretty_print_endpoints([Endpoint|Endpoints]) ->
    _ = pretty_print_endpoint(wh_json:to_proplist(Endpoint)),
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
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec print_condensed_list(list()) -> 'ok'.
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
