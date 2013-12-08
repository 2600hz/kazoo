%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz INC
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

-export([maybe_ensure_cid_valid/4]).

-include("ts.hrl").
-include_lib("kernel/include/inet.hrl"). %% for hostent record, used in find_ip/1

-define(VALIDATE_CALLER_ID, whapps_config:get_is_true(<<"trunkstore">>, <<"ensure_valid_caller_id">>, 'false')).
-define(VALIDATE_EMERGENCY_ID, whapps_config:get_is_true(<<"trunkstore">>, <<"ensure_valid_emergency_number">>, 'false')).

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

-spec get_media_handling(wh_json:objects() | api_binaries()) -> ne_binary().
get_media_handling(L) ->
    case simple_extract(L) of
        <<"process">> -> <<"process">>;
        _ -> <<"bypass">>
    end.

-spec constrain_weight(ne_binary() | integer()) -> integer().
constrain_weight(W) when not is_integer(W) ->
    constrain_weight(wh_util:to_integer(W));
constrain_weight(W) when W > 100 -> 100;
constrain_weight(W) when W < 1 -> 1;
constrain_weight(W) -> W.

-spec lookup_did(ne_binary(), ne_binary()) ->
                        {'ok', wh_json:object()} |
                        {'error', 'no_did_found' | atom()}.
lookup_did(DID, AccountId) ->
    Options = [{<<"key">>, DID}],
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),

    case wh_cache:fetch({'lookup_did', DID, AccountId}) of
        {'ok', _}=Resp ->
            lager:info("cache hit for ~s", [DID]),
            Resp;
        {'error', 'not_found'} ->
            case couch_mgr:get_results(AccountDb, ?TS_VIEW_DIDLOOKUP, Options) of
                {'ok', []} ->
                    lager:info("cache miss for ~s, no results", [DID]),
                    {'error', 'no_did_found'};
                {'ok', [ViewJObj]} ->
                    lager:info("cache miss for ~s, found result with id ~s", [DID, wh_json:get_value(<<"id">>, ViewJObj)]),
                    ValueJObj = wh_json:get_value(<<"value">>, ViewJObj),
                    Resp = wh_json:set_value(<<"id">>, wh_json:get_value(<<"id">>, ViewJObj), ValueJObj),
                    wh_cache:store({'lookup_did', DID, AccountId}, Resp),
                    {'ok', Resp};
                {'ok', [ViewJObj | _Rest]} ->
                    lager:notice("multiple results for did ~s in acct ~s", [DID, AccountId]),
                    lager:info("cache miss for ~s, found multiple results, using first with id ~s", [DID, wh_json:get_value(<<"id">>, ViewJObj)]),
                    ValueJObj = wh_json:get_value(<<"value">>, ViewJObj),
                    Resp = wh_json:set_value(<<"id">>, wh_json:get_value(<<"id">>, ViewJObj), ValueJObj),
                    wh_cache:store({'lookup_did', DID, AccountId}, Resp),
                    {'ok', Resp};
                {error, _}=E ->
                    lager:info("cache miss for ~s, error ~p", [DID, E]),
                    E
            end
    end.

-spec lookup_user_flags(ne_binary(), ne_binary(), ne_binary()) ->
                               {'ok', wh_json:object()} |
                               {'error', atom()}.
lookup_user_flags(Name, Realm, AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),

    case wh_cache:fetch({'lookup_user_flags', Realm, Name, AccountId}) of
        {'ok', _}=Result ->
            lager:info("cache hit for ~s@~s", [Name, Realm]),
            Result;
        {'error', 'not_found'} ->
            case couch_mgr:get_results(AccountDb, <<"trunkstore/LookUpUserFlags">>, [{<<"key">>, [Realm, Name]}]) of
                {'error', _}=E ->
                    lager:info("cache miss for ~s@~s, err: ~p", [Name, Realm, E]),
                    E;
                {'ok', []} ->
                    lager:info("cache miss for ~s@~s, no results", [Name, Realm]),
                    {'error', 'no_user_flags'};
                {'ok', [User|_]} ->
                    lager:info("cache miss, found view result for ~s@~s with id ~s", [Name, Realm, wh_json:get_value(<<"id">>, User)]),
                    ValJObj = wh_json:get_value(<<"value">>, User),
                    JObj = wh_json:set_value(<<"id">>, wh_json:get_value(<<"id">>, User), ValJObj),
                    wh_cache:store({'lookup_user_flags', Realm, Name, AccountId}, JObj),
                    {'ok', JObj}
            end
    end.

-spec get_call_duration(wh_json:object()) -> integer().
get_call_duration(JObj) ->
    wh_util:to_integer(wh_json:get_value(<<"Billing-Seconds">>, JObj)).

-spec invite_format(ne_binary(), ne_binary()) -> wh_proplist().
invite_format(<<"e.164">>, To) ->
    [{<<"Invite-Format">>, <<"e164">>}
     ,{<<"To-DID">>, wnm_util:to_e164(To)}
    ];
invite_format(<<"e164">>, To) ->
    [{<<"Invite-Format">>, <<"e164">>}
     ,{<<"To-DID">>, wnm_util:to_e164(To)}
    ];
invite_format(<<"e164_without_plus">>, To) ->
    case wnm_util:to_e164(To) of
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
     ,{<<"To-DID">>, wnm_util:to_1npan(To)}
    ];
invite_format(<<"1npan">>, To) ->
    [{<<"Invite-Format">>, <<"1npan">>}
     ,{<<"To-DID">>, wnm_util:to_1npan(To)}
    ];
invite_format(<<"npanxxxxxx">>, To) ->
    [{<<"Invite-Format">>, <<"npan">>}
     ,{<<"To-DID">>, wnm_util:to_npan(To)}
    ];
invite_format(<<"npan">>, To) ->
    [{<<"Invite-Format">>, <<"npan">>}
     ,{<<"To-DID">>, wnm_util:to_npan(To)}
    ];
invite_format(_, _) ->
    [{<<"Invite-Format">>, <<"username">>}].

-spec caller_id(api_objects()) -> {api_binary(), api_binary()}.
caller_id([]) -> {'undefined', 'undefined'};
caller_id(['undefined'|T]) -> caller_id(T);
caller_id([CID|T]) ->
    case {wh_json:get_value(<<"cid_name">>, CID)
          ,wh_json:get_value(<<"cid_number">>, CID)
         }
    of
        {'undefined', 'undefined'} -> caller_id(T);
        CallerID -> CallerID
    end.

-spec sip_headers(api_objects()) -> api_object().
sip_headers([]) -> 'undefined';
sip_headers(L) when is_list(L) ->
    case [Headers || Headers <- L,
                     wh_json:is_json_object(Headers),
                     not wh_json:is_empty(Headers)
         ]
    of
        [Res] -> Res;
        _ -> 'undefined'
    end.

-spec failover(wh_json:objects() | api_binaries()) ->
                      api_object().
%% cascade from DID to Srv to Account
failover(L) ->
    case simple_extract(L) of
        B when is_binary(B) -> 'undefined';
        Other -> Other
    end.

-spec progress_timeout(wh_json:objects() | api_binaries()) ->
                              wh_json:object() | api_binary().
progress_timeout(L) -> simple_extract(L).

-spec bypass_media(wh_json:objects() | api_binaries()) -> ne_binary().
bypass_media(L) ->
    case simple_extract(L) of
        <<"process">> -> <<"false">>;
        _ -> <<"true">>
    end.

-spec delay(wh_json:objects() | api_binaries()) ->
                   wh_json:object() | api_binary().
delay(L) -> simple_extract(L).

-spec ignore_early_media(wh_json:objects() | api_binaries()) ->
                                wh_json:object() | api_binary().
ignore_early_media(L) -> simple_extract(L).

-spec ep_timeout(wh_json:objects() | api_binaries()) ->
                        wh_json:object() | api_binary().
ep_timeout(L) -> simple_extract(L).

-spec offnet_flags(list()) -> 'undefined' | list().
offnet_flags([]) -> 'undefined';
offnet_flags([H|_]) when is_list(H) -> H;
offnet_flags([_|T]) -> offnet_flags(T).

-spec simple_extract(wh_json:objects() | api_binaries()) ->
                            wh_json:object() | api_binary().
simple_extract([]) -> 'undefined';
simple_extract(['undefined'|T]) -> simple_extract(T);
simple_extract([<<>> | T]) -> simple_extract(T);
simple_extract([B | _T]) when is_binary(B) -> B;
simple_extract([JObj | T]) ->
    case wh_json:is_json_object(JObj) andalso (not wh_json:is_empty(JObj)) of
        'true' -> JObj;
        'false' -> simple_extract(T)
    end.

-type cid_type() :: 'external' |
                    'emergency'.

-spec maybe_ensure_cid_valid(cid_type(), ne_binary(), ne_binary(), ne_binary()) ->
                                    ne_binary().
maybe_ensure_cid_valid('external', CIDNum, FromUser, AccountId) ->
    case ?VALIDATE_CALLER_ID of
        'true' -> validate_external_cid(CIDNum, FromUser, AccountId);
        'false' -> CIDNum
    end;
maybe_ensure_cid_valid('emergency', ECIDNum, _FromUser, AccountId) ->
    case ?VALIDATE_EMERGENCY_ID of
        'true' -> validate_emergency_number(ECIDNum, AccountId);
        'false' -> ECIDNum
    end.

-spec validate_emergency_number(ne_binary(), ne_binary()) -> ne_binary().
validate_emergency_number(ECIDNum, AccountId) ->
    lager:info("ensure_valid_emergency_number flag detected; will check whether ECID is legal..."),
    case wh_number_manager:lookup_account_by_number(ECIDNum) of
        {'ok', AccountId, _} -> ensure_valid_emergency_number(ECIDNum, AccountId);
        _Else -> valid_emergency_number(AccountId)
    end.

-spec validate_external_cid(ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
validate_external_cid(CIDNum, FromUser, AccountId) ->
    lager:info("ensure_valid_caller_id flag detected, will check whether CID is legal..."),
    case wh_number_manager:lookup_account_by_number(CIDNum) of
        {'ok', AccountId, _} -> CIDNum;
        _Else -> validate_from_user(FromUser, AccountId)
    end.

-spec validate_from_user(ne_binary(), ne_binary()) -> ne_binary().
validate_from_user(FromUser, AccountId) ->
    NormalizedFromUser = wnm_util:normalize_number(FromUser),
    case wh_number_manager:lookup_account_by_number(NormalizedFromUser) of
        {'ok', AccountId, _} ->
            lager:info("CID Number derived from CID Name, normalized and set to: ~s", [NormalizedFromUser]),
            NormalizedFromUser;
        _NothingLeft ->
            DefaultCID = whapps_config:get(<<"trunkstore">>, <<"default_caller_id_number">>, <<"00000000000000">>),
            lager:info("no valid caller id identified! Will use default trunkstore caller id: ~s", [DefaultCID]),
            DefaultCID
    end.

ensure_valid_emergency_number(ECIDNum, AccountId) ->
    Numbers = valid_emergency_numbers(AccountId),
    case lists:member(ECIDNum, Numbers) of
        'true' ->
            ECIDNum;
        'false' ->
            valid_emergency_number(AccountId)
    end.

valid_emergency_numbers(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, <<"phone_numbers">>) of
        {'ok', JObj} ->
            [Number
             || Number <- wh_json:get_keys(JObj),
                lists:member(<<"dash_e911">>, wh_json:get_value([Number, <<"features">>], JObj, []))
            ];
        {'error', _} ->
            DefaultECID = whapps_config:get_non_empty(<<"trunkstore">>, <<"default_emergency_number">>, <<>>),
            lager:info("No valid caller id identified! Will use default trunkstore caller id: ~p",[DefaultECID]),
           DefaultECID
    end.

valid_emergency_number(AccountId)->
    [H|_] = valid_emergency_numbers(AccountId),
    H.
