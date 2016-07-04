%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2016, 2600Hz INC
%%% @doc
%%% utility functions for Trunkstore
%%%
%%% Some functions make use of the inet_parse module. This is an undocumented
%%% module, and as such the functions may change or be removed.
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ts_util).

-export([find_ip/1
         ,filter_active_calls/2
         ,get_media_handling/1
        ]).
-export([constrain_weight/1]).

-export([get_call_duration/1
         ,lookup_user_flags/3
         ,lookup_user_flags/4
         ,lookup_did/2
        ]).
-export([invite_format/2]).

%% Cascading settings
-export([sip_headers/1
         ,failover/1
         ,progress_timeout/1
         ,bypass_media/1
         ,delay/1
         ,ignore_early_media/1
         ,ep_timeout/1
         ,caller_id/1
         ,offnet_flags/1
        ]).

-export([maybe_ensure_cid_valid/4
         ,maybe_restrict_call/2
        ]).

-include("ts.hrl").
-include_lib("kernel/include/inet.hrl"). %% for hostent record, used in find_ip/1

-define(VALIDATE_CALLER_ID, kapps_config:get_is_true(?CONFIG_CAT, <<"ensure_valid_caller_id">>, 'false')).

-spec find_ip(ne_binary() | nonempty_string()) -> nonempty_string().
find_ip(Domain) when is_binary(Domain) ->
    find_ip(binary_to_list(Domain));
find_ip(Domain) when is_list(Domain) ->
    case inet_parse:address(Domain) of
        {'ok', _I} -> Domain;
        _Huh ->
            %% eventually we'll want to support both IPv4 and IPv6
            case inet:gethostbyname(Domain, 'inet') of
                {'error', _Err} -> Domain;
                {'ok', #hostent{h_addr_list=[]}} -> Domain;
                {'ok', #hostent{h_addr_list=[Addr | _Rest]}} ->
                    inet_parse:ntoa(Addr)
            end
    end.

%% FilterOn: CallID | flat_rate | per_min
%% Remove active call entries based on what Filter criteria is passed in
-spec filter_active_calls(ne_binary() | 'flat_rate' | 'per_min', active_calls()) -> active_calls().
filter_active_calls('flat_rate', ActiveCalls) ->
    lists:filter(fun({_,'flat_rate'}) -> 'false';
                    (_) -> 'true'
                 end, ActiveCalls);
filter_active_calls('per_min', ActiveCalls) ->
    lists:filter(fun({_,'per_min'}) -> 'false';
                    (_) -> 'true'
                 end, ActiveCalls);
filter_active_calls(CallID, ActiveCalls) ->
    lists:filter(fun({CallID1,_}) when CallID =:= CallID1 -> 'false';
                    (CallID1) when CallID =:= CallID1 -> 'false';
                    (_) -> 'true'
                 end, ActiveCalls).

-spec get_media_handling(kz_json:objects() | api_binaries()) -> ne_binary().
get_media_handling(L) ->
    case simple_extract(L) of
        <<"process">> -> <<"process">>;
        _ -> <<"bypass">>
    end.

-spec constrain_weight(ne_binary() | integer()) -> integer().
constrain_weight(W) when not is_integer(W) ->
    constrain_weight(kz_util:to_integer(W));
constrain_weight(W) when W > 100 -> 100;
constrain_weight(W) when W < 1 -> 1;
constrain_weight(W) -> W.

-spec lookup_did(ne_binary(), ne_binary()) ->
                        {'ok', kz_json:object()} |
                        {'error', 'no_did_found' | atom()}.
lookup_did(DID, AccountId) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case kz_cache:fetch_local(?CACHE_NAME
                              ,{'lookup_did', DID, AccountId}
                             )
    of
        {'ok', _}=Resp ->
            lager:info("cache hit for ~s", [DID]),
            Resp;
        {'error', 'not_found'} ->
            Options = [{'key', DID}],
            CacheProps = [{'origin', [{'db', knm_converters:to_db(DID), DID}, {'type', <<"number">>}]}],
            case kz_datamgr:get_results(AccountDb, ?TS_VIEW_DIDLOOKUP, Options) of
                {'ok', []} ->
                    lager:info("cache miss for ~s, no results", [DID]),
                    {'error', 'no_did_found'};
                {'ok', [ViewJObj]} ->
                    lager:info("cache miss for ~s, found result with id ~s", [DID, kz_doc:id(ViewJObj)]),
                    ValueJObj = kz_json:get_value(<<"value">>, ViewJObj),
                    Resp = kz_json:set_value(<<"id">>, kz_doc:id(ViewJObj), ValueJObj),
                    kz_cache:store_local(?CACHE_NAME
                                         ,{'lookup_did', DID, AccountId}
                                         ,Resp
                                         ,CacheProps
                                        ),
                    {'ok', Resp};
                {'ok', [ViewJObj | _Rest]} ->
                    lager:notice("multiple results for did ~s in acct ~s", [DID, AccountId]),
                    lager:info("cache miss for ~s, found multiple results, using first with id ~s", [DID, kz_doc:id(ViewJObj)]),
                    ValueJObj = kz_json:get_value(<<"value">>, ViewJObj),
                    Resp = kz_json:set_value(<<"id">>, kz_doc:id(ViewJObj), ValueJObj),
                    kz_cache:store_local(?CACHE_NAME
                                         ,{'lookup_did', DID, AccountId}
                                         ,Resp
                                         ,CacheProps
                                        ),
                    {'ok', Resp};
                {'error', _}=E ->
                    lager:info("cache miss for ~s, error ~p", [DID, E]),
                    E
            end
    end.

-spec lookup_user_flags(ne_binary(), ne_binary(), ne_binary()) ->
                               {'ok', kz_json:object()} |
                               {'error', atom()}.
lookup_user_flags(Name, Realm, AccountId) ->
    lookup_user_flags(Name, Realm, AccountId, 'undefined').

-spec lookup_user_flags(ne_binary(), ne_binary(), ne_binary(), api_binary()) ->
                               {'ok', kz_json:object()} |
                               {'error', atom()}.
lookup_user_flags('undefined', _, _, 'undefined') ->
    {'error', 'insufficient_info'};
lookup_user_flags('undefined', _, AccountId, DID) ->
    case lookup_did(DID, AccountId) of
        {'error', _}=E -> E;
        {'ok', JObj} ->
            DIDs = kz_json:from_list([{DID, kz_json:get_value(<<"DID_Opts">>, JObj, kz_json:new())}]),
            Server = kz_json:from_list(
                       [{<<"DIDs">>, DIDs}
                       ,{<<"options">>, kz_json:get_value(<<"server">>, JObj, kz_json:new())}
                       ,{<<"permissions">>, kz_json:new()}
                       ,{<<"monitor">>, kz_json:from_list([{<<"monitor_enabled">>, 'false'}])}
                       ,{<<"auth">>, kz_json:get_value(<<"auth">>, JObj, kz_json:new())}
                       ,{<<"server_name">>, <<>>}
                       ,{<<"server_type">>, <<>>}
                       ]),
            {'ok', kz_json:from_list(
                     [{<<"server">>, Server}
                     ,{<<"account">>, kz_json:get_value(<<"account">>, JObj, kz_json:new())}
                     ,{<<"call_restriction">>, kz_json:new()}
                     ])
            }
    end;
lookup_user_flags(Name, Realm, AccountId, _) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case kz_cache:fetch_local(?CACHE_NAME
                              ,{'lookup_user_flags', Realm, Name, AccountId}
                             )
    of
        {'ok', _}=Result ->
            lager:info("cache hit for ~s@~s", [Name, Realm]),
            Result;
        {'error', 'not_found'} ->
            Options = [{'key', [kz_util:to_lower_binary(Realm)
                                ,kz_util:to_lower_binary(Name)
                               ]
                       }],
            case kz_datamgr:get_results(AccountDb, <<"trunkstore/lookup_user_flags">>, Options) of
                {'error', _}=E ->
                    lager:info("cache miss for ~s@~s, err: ~p", [Name, Realm, E]),
                    E;
                {'ok', []} ->
                    lager:info("cache miss for ~s@~s, no results", [Name, Realm]),
                    {'ok', kz_json:new()};
                {'ok', [User|_]} ->
                    lager:info("cache miss, found view result for ~s@~s with id ~s", [Name, Realm, kz_doc:id(User)]),
                    ValJObj = kz_json:get_value(<<"value">>, User),
                    JObj = kz_json:set_value(<<"id">>, kz_doc:id(User), ValJObj),

                    {'ok', AccountJObj} = kz_account:fetch(AccountId),
                    Restriction = kz_json:get_value(<<"call_restriction">>, AccountJObj, kz_json:new()),
                    FlagsJObj = kz_json:set_value(<<"call_restriction">>, Restriction, JObj),
                    kz_cache:store_local(?CACHE_NAME
                                         ,{'lookup_user_flags', Realm, Name, AccountId}
                                         ,FlagsJObj
                                        ),
                    {'ok', FlagsJObj}
            end
    end.

-spec get_call_duration(kz_json:object()) -> integer().
get_call_duration(JObj) ->
    kz_util:to_integer(kz_json:get_value(<<"Billing-Seconds">>, JObj)).

-spec invite_format(ne_binary(), ne_binary()) -> kz_proplist().
invite_format(<<"e.164">>, To) ->
    [{<<"Invite-Format">>, <<"e164">>}
     ,{<<"To-DID">>, knm_converters:normalize(To)}
    ];
invite_format(<<"e164">>, To) ->
    [{<<"Invite-Format">>, <<"e164">>}
     ,{<<"To-DID">>, knm_converters:normalize(To)}
    ];
invite_format(<<"e164_without_plus">>, To) ->
    case knm_converters:normalize(To) of
        <<$+, PluslessDID/binary>> ->
            lager:info("while processing 'e164_without_plus' flag, DID ~s converted to E.164 with truncated '+': ~s",[To, PluslessDID]),
            [{<<"Invite-Format">>, <<"e164">>}
             ,{<<"To-DID">>, PluslessDID}
            ];
        AsIsDID ->
            [{<<"Invite-Format">>, <<"e164">>}
             ,{<<"To-DID">>, AsIsDID}
            ]
    end;
invite_format(<<"1npanxxxxxx">>, To) ->
    [{<<"Invite-Format">>, <<"1npan">>}
     ,{<<"To-DID">>, knm_converters:to_1npan(To)}
    ];
invite_format(<<"1npan">>, To) ->
    [{<<"Invite-Format">>, <<"1npan">>}
     ,{<<"To-DID">>, knm_converters:to_1npan(To)}
    ];
invite_format(<<"npanxxxxxx">>, To) ->
    [{<<"Invite-Format">>, <<"npan">>}
     ,{<<"To-DID">>, knm_converters:to_npan(To)}
    ];
invite_format(<<"npan">>, To) ->
    [{<<"Invite-Format">>, <<"npan">>}
     ,{<<"To-DID">>, knm_converters:to_npan(To)}
    ];
invite_format(_, _) ->
    [{<<"Invite-Format">>, <<"username">>}].

-spec caller_id(api_objects()) -> {api_binary(), api_binary()}.
caller_id([]) -> {'undefined', 'undefined'};
caller_id(['undefined'|T]) -> caller_id(T);
caller_id([CID|T]) ->
    case {kz_json:get_value(<<"cid_name">>, CID)
          ,kz_json:get_value(<<"cid_number">>, CID)
         }
    of
        {'undefined', 'undefined'} -> caller_id(T);
        CallerID -> CallerID
    end.

-spec sip_headers(api_objects()) -> api_object().
sip_headers([]) -> 'undefined';
sip_headers(L) when is_list(L) ->
    case [Headers || Headers <- L,
                     kz_json:is_json_object(Headers),
                     not kz_json:is_empty(Headers)
         ]
    of
        [Res] -> Res;
        _ -> 'undefined'
    end.

-spec failover(kz_json:objects() | api_binaries()) ->
                      api_object().
%% cascade from DID to Srv to Account
failover(L) ->
    case simple_extract(L) of
        B when is_binary(B) -> 'undefined';
        Other -> Other
    end.

-spec progress_timeout(kz_json:objects() | api_binaries()) ->
                              kz_json:object() | api_binary().
progress_timeout(L) -> simple_extract(L).

-spec bypass_media(kz_json:objects() | api_binaries()) -> ne_binary().
bypass_media(L) ->
    case simple_extract(L) of
        <<"process">> -> <<"false">>;
        _ -> <<"true">>
    end.

-spec delay(kz_json:objects() | api_binaries()) ->
                   kz_json:object() | api_binary().
delay(L) -> simple_extract(L).

-spec ignore_early_media(kz_json:objects() | api_binaries()) ->
                                kz_json:object() | api_binary().
ignore_early_media(L) -> simple_extract(L).

-spec ep_timeout(kz_json:objects() | api_binaries()) ->
                        kz_json:object() | api_binary().
ep_timeout(L) -> simple_extract(L).

-spec offnet_flags(list()) -> 'undefined' | list().
offnet_flags([]) -> 'undefined';
offnet_flags([H|_]) when is_list(H) -> H;
offnet_flags([_|T]) -> offnet_flags(T).

-spec simple_extract(kz_json:objects() | api_binaries()) ->
                            kz_json:object() | api_binary().
simple_extract([]) -> 'undefined';
simple_extract(['undefined'|T]) -> simple_extract(T);
simple_extract([<<>> | T]) -> simple_extract(T);
simple_extract([B | _T]) when is_binary(B) -> B;
simple_extract([JObj | T]) ->
    case kz_json:is_json_object(JObj)
        andalso (not kz_json:is_empty(JObj))
    of
        'true' -> JObj;
        'false' -> simple_extract(T)
    end.

-type cid_type() :: 'external' | 'emergency'.

-spec maybe_ensure_cid_valid(cid_type(), api_binary(), ne_binary(), ne_binary()) ->
                                    ne_binary().
maybe_ensure_cid_valid('external', CIDNum, FromUser, AccountId) ->
    case ?VALIDATE_CALLER_ID of
        'true' -> validate_external_cid(CIDNum, FromUser, AccountId);
        'false' -> CIDNum
    end;
maybe_ensure_cid_valid('emergency', ECIDNum, _FromUser, _AccountId) ->
    ECIDNum.

-spec validate_external_cid(api_binary(), ne_binary(), ne_binary()) -> ne_binary().
validate_external_cid(CIDNum, FromUser, AccountId) ->
    lager:info("ensure_valid_caller_id flag detected, will check whether CID is legal..."),
    case knm_number:lookup_account(CIDNum) of
        {'ok', AccountId, _} -> CIDNum;
        _Else -> validate_from_user(FromUser, AccountId)
    end.

-spec validate_from_user(ne_binary(), ne_binary()) -> ne_binary().
validate_from_user(FromUser, AccountId) ->
    NormalizedFromUser = knm_converters:normalize(FromUser),
    case knm_number:lookup_account(NormalizedFromUser) of
        {'ok', AccountId, _} ->
            lager:info("CID Number derived from CID Name, normalized and set to: ~s", [NormalizedFromUser]),
            NormalizedFromUser;
        _NothingLeft ->
            DefaultCID = kapps_config:get(?CONFIG_CAT, <<"default_caller_id_number">>, kz_util:anonymous_caller_id_number()),
            lager:info("no valid caller id identified! Will use default trunkstore caller id: ~s", [DefaultCID]),
            DefaultCID
    end.

-spec maybe_restrict_call(ts_callflow:state(), kz_proplist()) -> boolean().
maybe_restrict_call(#ts_callflow_state{acctid=AccountId
                                       ,route_req_jobj=RRObj
                                      }
                    ,Command) ->
    Number = props:get_value(<<"To-DID">>, Command),
    Classification = knm_converters:classify(Number),
    lager:debug("Trunkstore classified number as ~p", [Classification]),
    Username = kz_json:get_value([<<"Custom-Channel-Vars">>,<<"Username">>], RRObj),
    Realm = kz_json:get_value([<<"Custom-Channel-Vars">>,<<"Realm">>], RRObj),
    {'ok', Opts} = ?MODULE:lookup_user_flags(Username, Realm, AccountId),
    lager:debug("Trunkstore lookup_user_flag results: ~p", [Opts]),
    kz_json:get_value([<<"call_restriction">>, Classification, <<"action">>], Opts) =:= <<"deny">>.
