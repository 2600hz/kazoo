%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Preforms maintenance operations against the stepswitch dbs
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(stepswitch_maintenance).

-export([resources/0]).
-export([reverse_lookup/1]).
-export([flush/0
        ]).
-export([lookup_number/1
        ,number_tree/1
        ]).
-export([register_views/0]).
-export([reload_resources/0, reload_resources/1]).
-export([process_number/1
        ,process_number/2
        ]).

-include("stepswitch.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reverse_lookup(kz_term:text()) -> 'ok'.
reverse_lookup(Thing) when not is_binary(Thing) ->
    reverse_lookup(kz_term:to_binary(Thing));
reverse_lookup(Thing) ->
    JObj = kz_json:from_list([{<<"From-Network-Addr">>, Thing}
                             ,{<<"Auth-Realm">>, Thing}
                             ]),
    case stepswitch_resources:reverse_lookup(JObj) of
        {'ok', Props} -> pretty_print_lookup(Props);
        {'error', 'not_found'} -> io:format("resource not found~n")
    end.

-spec pretty_print_lookup(kz_term:proplist()) -> 'ok'.
pretty_print_lookup([]) -> 'ok';
pretty_print_lookup([{Key, Value}|Props]) ->
    io:format("~-19s: ~s~n", [Key, kz_term:to_binary(Value)]),
    pretty_print_lookup(Props).

%%------------------------------------------------------------------------------
%% @doc Displays account tree when given a DID
%% @end
%%------------------------------------------------------------------------------
-spec number_tree(kz_term:ne_binary()) -> 'ok'.
number_tree(DID) ->
    case knm_number:lookup_account(DID) of
        {'error', _} -> io:format("DID ~s was not found~n", [DID]);
        {'ok', AccountId, _Props} ->
            case kz_datamgr:open_doc(?KZ_ACCOUNTS_DB, AccountId) of
                {'ok', AccountDoc} -> number_tree(DID, AccountDoc);
                {'error', _E} -> io:format("failed to find account doc for ~s(~s)~n", [AccountId, DID])
            end
    end.

-spec number_tree(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
number_tree(DID, AccountDoc) ->
    io:format("~s tree ", [DID]),
    print_tree(kzd_accounts:tree(AccountDoc)),
    io:format(" ~s(~s)~n", [kzd_accounts:name(AccountDoc), kz_doc:id(AccountDoc)]).

-spec print_tree(kz_term:ne_binaries()) -> 'ok'.
print_tree([]) -> 'ok';
print_tree([AccountId|Tree]) ->
    io:format(" ~s(~s) ->", [kzd_accounts:fetch_name(AccountId), AccountId]),
    print_tree(Tree).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
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

-spec pretty_print_resource(kz_term:proplist()) -> 'ok'.
pretty_print_resource([]) -> 'ok';
pretty_print_resource([{_, []}|Props]) ->
    pretty_print_resource(Props);
pretty_print_resource([{Key, Values}|Props]) when is_list(Values) ->
    _ = pretty_print_resource(Props),
    io:format("~s~n", [Key]),
    print_condensed_list(Values);
pretty_print_resource([{Key, Value}|Props]) ->
    case kz_json:is_json_object(Value) of
        'false' -> io:format("~-19s: ~s~n", [Key, kz_term:to_binary(Value)]);
        'true' -> io:format("~-19s: ~s~n", [Key, kz_json:encode(Value)])
    end,
    pretty_print_resource(Props).

%%------------------------------------------------------------------------------
%% @doc Flush the stepswitch local cache
%% @end
%%------------------------------------------------------------------------------
-spec flush() -> 'ok'.
flush() -> kz_cache:flush_local(?CACHE_NAME).

-spec register_views() -> 'ok'.
register_views() ->
    kz_datamgr:register_views_from_folder('stepswitch').

%%------------------------------------------------------------------------------
%% @doc Lookup a number in the route db and return the account ID if known
%% @end
%%------------------------------------------------------------------------------
-spec lookup_number(kz_term:text()) -> 'ok'.
lookup_number(Number) ->
    case knm_number:lookup_account(Number) of
        {'ok', AccountId, Props} ->
            io:format("~-19s: ~s~n", [<<"Account-ID">>, AccountId]),
            Classification = knm_converters:classify(Number),
            pretty_print_number_props(
              props:insert_value(<<"classification">>, Classification, Props)
             );
        {'error', 'not_found'} ->
            io:format("number not found~n")
    end.

-spec pretty_print_number_props(kz_term:proplist()) -> 'ok'.
pretty_print_number_props([]) -> 'ok';
pretty_print_number_props([{Key, Value}|Props]) ->
    io:format("~-19s: ~s~n", [kz_term:to_binary(Key), kz_term:to_binary(Value)]),
    pretty_print_number_props(Props).

%%------------------------------------------------------------------------------
%% @doc Instructs stepswitch_outbound to re-scan the resource db and
%% refresh the cache.
%% @end
%%------------------------------------------------------------------------------
-spec reload_resources() -> 'ok'.
reload_resources() ->
    _ = stepswitch_resources:fetch_global_resources(),
    'ok'.

-spec reload_resources(kz_term:ne_binary()) -> 'ok'.
reload_resources(Account) ->
    AccountId = kzs_util:format_account_id(Account),
    _ = stepswitch_resources:fetch_local_resources(AccountId),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec process_number(kz_term:text()) -> any().
process_number(Number) -> process_number(Number, 'undefined').

-spec process_number(kz_term:text(), kz_term:text() | 'undefined') -> any().
process_number(Num, 'undefined') ->
    JObj = kz_json:from_list([{<<"To-DID">>, kz_term:to_binary(Num)}]),
    Number = stepswitch_util:get_outbound_destination(JObj),
    Endpoints = stepswitch_resources:endpoints(Number
                                              ,kapi_offnet_resource:jobj_to_req(JObj)
                                              ),
    pretty_print_endpoints(Endpoints);
process_number(Num, AccountId) ->
    JObj = kz_json:from_list([{<<"Account-ID">>, kz_term:to_binary(AccountId)}
                             ,{<<"Hunt-Account-ID">>, kz_term:to_binary(AccountId)}
                             ,{<<"To-DID">>, kz_term:to_binary(Num)}
                             ]),
    Number = stepswitch_util:get_outbound_destination(JObj),
    Endpoints = stepswitch_resources:endpoints(Number
                                              ,kapi_offnet_resource:jobj_to_req(JObj)
                                              ),
    pretty_print_endpoints(Endpoints).

-spec pretty_print_endpoints(kz_json:objects()) -> any().
pretty_print_endpoints([]) -> 'ok';
pretty_print_endpoints([Endpoint|Endpoints]) ->
    _ = pretty_print_endpoint(kz_json:to_proplist(Endpoint)),
    io:format("~n"),
    pretty_print_endpoints(Endpoints).

-spec pretty_print_endpoint(kz_term:proplist()) -> any().
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
                                  ,kz_term:to_binary(Value)
                                  ])
     || {Key, Value} <- kz_json:to_proplist(JObj)
    ];
pretty_print_endpoint([{Key, Value}|Props]) ->
    io:format("~-19s: ~s~n", [Key, kz_term:to_binary(Value)]),
    pretty_print_endpoint(Props).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec print_condensed_list(list()) -> 'ok'.
print_condensed_list([E1, E2, E3]) ->
    io:format("    | ~-20s | ~-20s | ~-20s|~n"
             ,[kz_term:to_binary(E1)
              ,kz_term:to_binary(E2)
              ,kz_term:to_binary(E3)
              ]);
print_condensed_list([E1, E2]) ->
    io:format("    | ~-20s | ~-20s | ~-20s|~n"
             ,[kz_term:to_binary(E1)
              ,kz_term:to_binary(E2)
              ,<<>>
              ]);
print_condensed_list([E1]) ->
    io:format("    | ~-20s | ~-20s | ~-20s|~n"
             ,[kz_term:to_binary(E1)
              ,<<>>
              ,<<>>
              ]);
print_condensed_list([E1, E2, E3 | Rest]) ->
    io:format("    | ~-20s | ~-20s | ~-20s|~n"
             ,[kz_term:to_binary(E1)
              ,kz_term:to_binary(E2)
              ,kz_term:to_binary(E3)
              ]),
    print_condensed_list(Rest).
