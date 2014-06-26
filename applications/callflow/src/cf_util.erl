%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_util).

-include("callflow.hrl").
-include_lib("whistle/src/wh_json.hrl").

-define(OWNER_KEY(Db, User), {?MODULE, 'owner_id', Db, User}).
-define(CF_FLOW_CACHE_KEY(Number, Db), {'cf_flow', Number, Db}).
-define(SIP_USER_OWNERS_KEY(Db, User), {?MODULE, 'sip_user_owners', Db, User}).
-define(SIP_ENDPOINT_ID_KEY(Db, User), {?MODULE, 'sip_endpoint_id', Db, User}).
-define(PARKING_PRESENCE_KEY(Db, Request), {?MODULE, 'parking_callflow', Db, Request}).
-define(MANUAL_PRESENCE_KEY(Db), {?MODULE, 'manual_presence', Db}).
-define(OPERATOR_KEY, whapps_config:get(?CF_CONFIG_CAT, <<"operator_key">>, <<"0">>)).

-define(ENCRYPTION_MAP, [{<<"srtp">>, [{<<"RTP-Secure-Media">>, <<"true">>}]}
                        ,{<<"zrtp">>, [{<<"ZRTP-Secure-Media">>, <<"true">>}
                                       ,{<<"ZRTP-Enrollment">>, <<"true">>}
                                      ]}
                        ]).

-export([presence_probe/2]).
-export([presence_mwi_query/2]).
-export([unsolicited_owner_mwi_update/2]).
-export([unsolicited_endpoint_mwi_update/2]).
-export([alpha_to_dialpad/1, ignore_early_media/1]).
-export([correct_media_path/2]).
-export([lookup_callflow/1, lookup_callflow/2]).
-export([handle_bridge_failure/2, handle_bridge_failure/3]).
-export([send_default_response/2]).
-export([get_sip_realm/2, get_sip_realm/3]).
-export([get_operator_callflow/1]).
-export([endpoint_id_by_sip_username/2]).
-export([owner_ids_by_sip_username/2]).
-export([apply_dialplan/2]).
-export([encryption_method_map/2]).
-export([maybe_start_metaflows/2]).

-export([caller_belongs_to_group/2
         ,maybe_belongs_to_group/3
         ,caller_belongs_to_user/2
         ,find_endpoints/3
         ,find_channels/2
         ,find_user_endpoints/3
         ,find_group_endpoints/2
         ,check_value_of_fields/4
        ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec presence_probe(wh_json:object(), wh_proplist()) -> any().
presence_probe(JObj, _Props) ->
    'true' = wapi_notifications:presence_probe_v(JObj),
    {ToUser, ToRealm} =
        case wh_json:get_value(<<"To">>, JObj) of
            'undefined' ->
                {wh_json:get_value(<<"To-User">>, JObj)
                 ,wh_json:get_value(<<"To-Realm">>, JObj)
                };
            To -> list_to_tuple(binary:split(To, <<"@">>))
        end,

    {FromUser, FromRealm} =
        case wh_json:get_value(<<"From">>, JObj) of
            'undefined' ->
                {wh_json:get_value(<<"From-User">>, JObj)
                 ,wh_json:get_value(<<"From-Realm">>, JObj)
                };
            From -> list_to_tuple(binary:split(From, <<"@">>))
        end,

    Subscription = wh_json:get_value(<<"Subscription">>, JObj),

    ProbeRepliers = [fun presence_mwi_update/4
                     ,fun presence_parking_slot/4
                     ,fun manual_presence/4
                    ],
    [Fun(Subscription, {FromUser, FromRealm}, {ToUser, ToRealm}, JObj)
     || Fun <- ProbeRepliers
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec presence_mwi_query(wh_json:object(), wh_proplist()) -> 'ok'.
presence_mwi_query(JObj, _Props) ->
    _ = wh_util:put_callid(JObj),
    Username = wh_json:get_value(<<"Username">>, JObj),
    Realm = wh_json:get_value(<<"Realm">>, JObj),
    case whapps_util:get_account_by_realm(Realm) of
        {'ok', AccountDb} ->
            lager:debug("replying to mwi query"),
            presence_mwi_resp(Username, Realm, AccountDb, JObj);
        _Else -> 'ok'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-type mwi_update_return() :: 'missing_account_db' |
                             'missing_owner_id'.
-spec unsolicited_owner_mwi_update(api_binary(), api_binary()) ->
                                          'ok' |
                                          {'error', mwi_update_return()} |
                                          couch_mgr:couchbeam_error().
unsolicited_owner_mwi_update('undefined', _) -> {'error', 'missing_account_db'};
unsolicited_owner_mwi_update(_, 'undefined') -> {'error', 'missing_owner_id'};
unsolicited_owner_mwi_update(AccountDb, OwnerId) ->
    ViewOptions = [{'key', [OwnerId, <<"device">>]}
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(AccountDb, <<"cf_attributes/owned">>, ViewOptions) of
        {'ok', JObjs} ->
            {New, Saved} = vm_count_by_owner(AccountDb, OwnerId),
            AccountId = wh_util:format_account_id(AccountDb, 'raw'),
            lists:foreach(
              fun(JObj) ->
                      J = wh_json:get_value(<<"doc">>, JObj),
                      Username = wh_json:get_value([<<"sip">>, <<"username">>], J),
                      Realm = get_sip_realm(J, AccountId),
                      OwnerId = get_endpoint_owner(J),
                      case wh_json:get_value([<<"sip">>, <<"method">>], J) =:= <<"password">>
                          andalso Username =/= 'undefined'
                          andalso Realm =/= 'undefined'
                          andalso OwnerId =/= 'undefined'
                      of
                          'true' -> send_mwi_update(New, Saved, Username, Realm);
                          'false' -> 'ok'
                      end
              end, JObjs),
            'ok';
        {'error', _R}=E ->
            lager:warning("failed to find devices owned by ~s: ~p", [OwnerId, _R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec unsolicited_endpoint_mwi_update(api_binary(), api_binary()) ->
                                             'ok' | {'error', _}.
unsolicited_endpoint_mwi_update('undefined', _) ->
    {'error', 'missing_account_db'};
unsolicited_endpoint_mwi_update(_, 'undefined') ->
    {'error', 'missing_owner_id'};
unsolicited_endpoint_mwi_update(AccountDb, EndpointId) ->
    case couch_mgr:open_cache_doc(AccountDb, EndpointId) of
        {'error', _}=E -> E;
        {'ok', JObj} ->
            maybe_send_endpoint_mwi_update(JObj, AccountDb)
    end.

maybe_send_endpoint_mwi_update(JObj, AccountDb) ->
    AccountId = wh_util:format_account_id(AccountDb, 'raw'),
    Username = wh_json:get_value([<<"sip">>, <<"username">>], JObj),
    Realm = get_sip_realm(JObj, AccountId),
    OwnerId = get_endpoint_owner(JObj),
    case wh_json:get_value([<<"sip">>, <<"method">>], JObj) =:= <<"password">>
        andalso Username =/= 'undefined'
        andalso Realm =/= 'undefined'
    of
        'true' ->
            {New, Saved} = vm_count_by_owner(AccountDb, OwnerId),
            send_mwi_update(New, Saved, Username, Realm);
        'false' -> {'error', 'not_appropriate'}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec alpha_to_dialpad(ne_binary()) -> ne_binary().
alpha_to_dialpad(Value) ->
    << <<(dialpad_digit(C))>> || <<C>> <= strip_nonalpha(wh_util:to_lower_binary(Value))>>.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec strip_nonalpha(ne_binary()) -> ne_binary().
strip_nonalpha(Value) ->
    re:replace(Value, <<"[^[:alpha:]]">>, <<>>, [{'return', 'binary'}
                                                 ,'global'
                                                ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec dialpad_digit(97..122) -> 50..57.
dialpad_digit(ABC) when ABC =:= $a orelse ABC =:= $b orelse ABC =:= $c -> $2;
dialpad_digit(DEF) when DEF =:= $d orelse DEF =:= $e orelse DEF =:= $f -> $3;
dialpad_digit(GHI) when GHI =:= $g orelse GHI =:= $h orelse GHI =:= $i -> $4;
dialpad_digit(JKL) when JKL =:= $j orelse JKL =:= $k orelse JKL =:= $l -> $5;
dialpad_digit(MNO) when MNO =:= $m orelse MNO =:= $n orelse MNO =:= $o -> $6;
dialpad_digit(PQRS) when PQRS =:= $p orelse PQRS =:= $q orelse PQRS =:= $r orelse PQRS =:= $s -> $7;
dialpad_digit(TUV) when TUV =:= $t orelse TUV =:= $u orelse TUV =:= $v -> $8;
dialpad_digit(WXYZ) when WXYZ =:= $w orelse WXYZ =:= $x orelse WXYZ =:= $y orelse WXYZ =:= $z -> $9.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Determine if we should ignore early media
%% @end
%%--------------------------------------------------------------------
-spec ignore_early_media(wh_json:objects()) -> api_binary().
ignore_early_media([]) -> 'undefined';
ignore_early_media(Endpoints) ->
    case lists:any(fun(Endpoint) ->
                           wh_json:is_true(<<"Ignore-Early-Media">>, Endpoint)
                   end, Endpoints)
    of
        'true' -> <<"true">>;
        'false' -> 'undefined'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% given a media path that is just a media id correct it to include
%% the account id
%% @end
%%--------------------------------------------------------------------
-spec correct_media_path(api_binary(), whapps_call:call()) -> api_binary().
correct_media_path(Media, Call) ->
    wh_media_util:media_path(Media, Call).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec owner_ids_by_sip_username(ne_binary(), ne_binary()) ->
                                       {'ok', ne_binaries()} |
                                       {'error', _}.
owner_ids_by_sip_username(AccountDb, Username) ->
    case wh_cache:peek_local(?CALLFLOW_CACHE, ?SIP_USER_OWNERS_KEY(AccountDb, Username)) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} ->
            get_owner_ids_by_sip_username(AccountDb, Username)
    end.

-spec get_owner_ids_by_sip_username(ne_binary(), ne_binary()) -> {'ok', ne_binaries()} | {'error', _}.
get_owner_ids_by_sip_username(AccountDb, Username) ->
    ViewOptions = [{'key', Username}],
    case couch_mgr:get_results(AccountDb, <<"cf_attributes/sip_username">>, ViewOptions) of
        {'ok', [JObj]} ->
            EndpointId = wh_json:get_value(<<"id">>, JObj),
            OwnerIds = wh_json:get_value(<<"value">>, JObj, []),
            CacheProps = [{'origin', {'db', AccountDb, EndpointId}}],
            wh_cache:store_local(?CALLFLOW_CACHE, ?SIP_USER_OWNERS_KEY(AccountDb, Username), OwnerIds, CacheProps),
            {'ok', OwnerIds};
        {'ok', []} ->
            lager:debug("sip username ~s not in account db ~s", [Username, AccountDb]),
            {'error', 'not_found'};
        {'error', _R}=E ->
            lager:warning("unable to lookup sip username ~s for owner ids: ~p", [Username, _R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec endpoint_id_by_sip_username(ne_binary(), ne_binary()) ->
                                         {'ok', ne_binary()} |
                                         {'error', 'not_found'}.
endpoint_id_by_sip_username(AccountDb, Username) ->
    case wh_cache:peek_local(?CALLFLOW_CACHE, ?SIP_ENDPOINT_ID_KEY(AccountDb, Username)) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} ->
           get_endpoint_id_by_sip_username(AccountDb, Username)
    end.

-spec get_endpoint_id_by_sip_username(ne_binary(), ne_binary()) ->
                                             {'ok', ne_binary()} |
                                             {'error', 'not_found'}.
get_endpoint_id_by_sip_username(AccountDb, Username) ->
    ViewOptions = [{'key', Username}],
    case couch_mgr:get_results(AccountDb, <<"cf_attributes/sip_username">>, ViewOptions) of
        {'ok', [JObj]} ->
            EndpointId = wh_json:get_value(<<"id">>, JObj),
            CacheProps = [{'origin', {'db', AccountDb, EndpointId}}],
            wh_cache:store_local(?CALLFLOW_CACHE, ?SIP_ENDPOINT_ID_KEY(AccountDb, Username), EndpointId, CacheProps),
            {'ok', EndpointId};
        {'ok', []} ->
            lager:debug("sip username ~s not in account db ~s", [Username, AccountDb]),
            {'error', 'not_found'};
        {'error', _R} ->
            lager:warning("unable to lookup sip username ~s for owner ids: ~p", [Username, _R]),
            {'error', 'not_found'}
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec get_operator_callflow(ne_binary()) -> wh_jobj_return().
get_operator_callflow(Account) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    Options = [{'key', ?OPERATOR_KEY}, 'include_docs'],
    case couch_mgr:get_results(AccountDb, ?LIST_BY_NUMBER, Options) of
        {'ok', []} -> {'error', 'not_found'};
        {'ok', [JObj|_]} ->
            {'ok', wh_json:get_value([<<"doc">>, <<"flow">>], JObj, wh_json:new())};
        {'error', _R}=E ->
            lager:warning("unable to find operator callflow in ~s: ~p", [Account, _R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Look for children branches to handle the failure replies of
%% certain actions, like cf_offnet and cf_resources
%% @end
%%--------------------------------------------------------------------
-spec handle_bridge_failure({'fail', wh_json:object()} | api_binary(), whapps_call:call()) ->
                                   'ok' | 'not_found'.
handle_bridge_failure({'fail', Reason}, Call) ->
    {Cause, Code} = whapps_util:get_call_termination_reason(Reason),
    handle_bridge_failure(Cause, Code, Call);
handle_bridge_failure('undefined', _) ->
    'not_found';
handle_bridge_failure(Failure, Call) ->
    case cf_exe:attempt(Failure, Call) of
        {'attempt_resp', 'ok'} ->
            lager:info("found child branch to handle failure: ~s", [Failure]),
            'ok';
        {'attempt_resp', _} ->
            'not_found'
    end.

-spec handle_bridge_failure(api_binary(), api_binary(), whapps_call:call()) ->
                                   'ok' | 'not_found'.
handle_bridge_failure(Cause, Code, Call) ->
    lager:info("attempting to find failure branch for ~s:~s", [Code, Cause]),
    case (handle_bridge_failure(Cause, Call) =:= 'ok')
        orelse (handle_bridge_failure(Code, Call) =:= 'ok') of
        'true' -> 'ok';
        'false' -> 'not_found'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Send and wait for a call failure cause response
%% @end
%%--------------------------------------------------------------------
-spec send_default_response(ne_binary(), whapps_call:call()) -> 'ok'.
send_default_response(Cause, Call) ->
    case cf_exe:wildcard_is_empty(Call) of
        'false' -> 'ok';
        'true' ->
            CallId = cf_exe:callid(Call),
            CtrlQ = cf_exe:control_queue(Call),
            case wh_call_response:send_default(CallId, CtrlQ, Cause) of
                {'error', 'no_response'} -> 'ok';
                {'ok', NoopId} ->
                    _ = whapps_call_command:wait_for_noop(Call, NoopId),
                    'ok'
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get the sip realm
%% @end
%%--------------------------------------------------------------------
-spec get_sip_realm(wh_json:object(), ne_binary()) -> api_binary().
get_sip_realm(SIPJObj, AccountId) ->
    get_sip_realm(SIPJObj, AccountId, 'undefined').

-spec get_sip_realm(wh_json:object(), ne_binary(), Default) -> Default | ne_binary().
get_sip_realm(SIPJObj, AccountId, Default) ->
    case wh_json:get_ne_value([<<"sip">>, <<"realm">>], SIPJObj) of
        'undefined' ->
            case wh_util:get_account_realm(AccountId) of
                'undefined' -> Default;
                Else -> Else
            end;
        Realm -> Realm
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% lookup the callflow based on the requested number in the account
%% @end
%%-----------------------------------------------------------------------------
-type lookup_callflow_ret() :: {'ok', wh_json:object(), boolean()} |
                               {'error', term()}.

-spec lookup_callflow(whapps_call:call()) -> lookup_callflow_ret().
lookup_callflow(Call) ->
    lookup_callflow(whapps_call:request_user(Call), whapps_call:account_id(Call)).

-spec lookup_callflow(ne_binary(), ne_binary()) -> lookup_callflow_ret().
lookup_callflow(Number, AccountId) when not is_binary(Number) ->
    lookup_callflow(wh_util:to_binary(Number), AccountId);
lookup_callflow(<<>>, _) -> {'error', 'invalid_number'};
lookup_callflow(Number, AccountId) ->
    Db = wh_util:format_account_id(AccountId, 'encoded'),
    do_lookup_callflow(Number, Db).

do_lookup_callflow(Number, Db) ->
    lager:info("searching for callflow in ~s to satisfy '~s'", [Db, Number]),
    Options = [{'key', Number}, 'include_docs'],
    case couch_mgr:get_results(Db, ?LIST_BY_NUMBER, Options) of
        {'error', _}=E -> E;
        {'ok', []} when Number =/= ?NO_MATCH_CF ->
            case lookup_callflow_patterns(Number, Db) of
                {'error', _} -> maybe_use_nomatch(Number, Db);
                {'ok', {Flow, Capture}} ->
                    F = wh_json:set_value(<<"capture_group">>, Capture, Flow),
                    wh_cache:store_local(?CALLFLOW_CACHE, ?CF_FLOW_CACHE_KEY(Number, Db), F),
                    {'ok', F, 'false'}
            end;
        {'ok', []} -> {'error', 'not_found'};
        {'ok', [JObj]} ->
            Flow = wh_json:get_value(<<"doc">>, JObj),
            wh_cache:store_local(?CALLFLOW_CACHE, ?CF_FLOW_CACHE_KEY(Number, Db), Flow),
            {'ok', Flow, Number =:= ?NO_MATCH_CF};
        {'ok', [JObj | _Rest]} ->
            lager:info("lookup resulted in more than one result, using the first"),
            Flow = wh_json:get_value(<<"doc">>, JObj),
            wh_cache:store_local(?CALLFLOW_CACHE, ?CF_FLOW_CACHE_KEY(Number, Db), Flow),
            {'ok', Flow, Number =:= ?NO_MATCH_CF}
    end.

%% only route to nomatch when Number is all digits and/or +
maybe_use_nomatch(<<"+", Number/binary>>, Db) ->
    maybe_use_nomatch(Number, Db);
maybe_use_nomatch(Number, Db) ->
    case lists:all(fun is_digit/1, wh_util:to_list(Number)) of
        'true' -> do_lookup_callflow(?NO_MATCH_CF, Db);
        'false' ->
            lager:info("can't use no_match: number not all digits: ~s", [Number]),
            {'error', 'not_found'}
    end.

is_digit(X) when X >= $0, X =< $9 -> 'true';
is_digit(_) -> 'false'.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% send a route response for a route request that can be fulfilled by this
%% process
%% @end
%%-----------------------------------------------------------------------------
-spec lookup_callflow_patterns(ne_binary(), ne_binary()) ->
                                      {'ok', {wh_json:object(), ne_binary()}} |
                                      {'error', term()}.
lookup_callflow_patterns(Number, Db) ->
    lager:info("lookup callflow patterns for ~s in ~s", [Number, Db]),
    case couch_mgr:get_results(Db, ?LIST_BY_PATTERN, ['include_docs']) of
        {'ok', Patterns} ->
            case test_callflow_patterns(Patterns, Number, {'undefined', <<>>}) of
                {'undefined', <<>>} -> {'error', 'not_found'};
                {Flow, <<>>} -> {'ok', {Flow, 'undefined'}};
                Match -> {'ok', Match}
            end;
        {'error', _}=E ->
            E
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec test_callflow_patterns(wh_json:objects(), ne_binary()
                             ,{'undefined', <<>>} | {wh_json:object(), ne_binary()}
                            ) ->
                                    {'undefined', <<>>} |
                                    {wh_json:object(), ne_binary()}.
test_callflow_patterns([], _, Result) ->
    Result;
test_callflow_patterns([Pattern|T], Number, {_, Capture}=Result) ->
    Regex = wh_json:get_value(<<"key">>, Pattern),
    case re:run(Number, Regex) of
        {'match', [{Start,End}]} ->
            Match = binary:part(Number, Start, End),
            Flow = wh_json:get_value(<<"doc">>, Pattern),
            case binary:part(Number, Start, End) of
                <<>> when Capture =:= <<>> ->
                    test_callflow_patterns(T, Number, {Flow, <<>>});
                Match when size(Match) > size(Capture); size(Match) =:= 0 ->
                    test_callflow_patterns(T, Number, {Flow, Match});
                _ ->
                    test_callflow_patterns(T, Number, Result)
            end;
        {'match', CaptureGroups} ->
            %% find the largest matching group if present by sorting the position of the
            %% matching groups by list, reverse so head is largest, then take the head of the list
            {Start, End} = hd(lists:reverse(lists:keysort(2, tl(CaptureGroups)))),
            Flow = wh_json:get_value(<<"doc">>, Pattern),
            case binary:part(Number, Start, End) of
                <<>> when Capture =:= <<>> ->
                    test_callflow_patterns(T, Number, {Flow, <<>>});
                Match when size(Match) > size(Capture) ->
                    test_callflow_patterns(T, Number, {Flow, Match});
                _ ->
                    test_callflow_patterns(T, Number, Result)
            end;
        _ ->
            test_callflow_patterns(T, Number, Result)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-type vm_count() :: ne_binary() | non_neg_integer().
-spec send_mwi_update(vm_count(), vm_count(), ne_binary(), ne_binary()) -> 'ok'.
send_mwi_update(New, Saved, Username, Realm) ->
    send_mwi_update(New, Saved, Username, Realm, wh_json:new()).

-spec send_mwi_update(vm_count(), vm_count(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
send_mwi_update(New, Saved, Username, Realm, JObj) ->
    DefaultAccount = <<"sip:", Username/binary, "@", Realm/binary>>,
    Command = [{<<"Messages-New">>, New}
               ,{<<"Messages-Saved">>, Saved}
               ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
               ,{<<"Switch-Nodename">>, wh_json:get_value(<<"Switch-Nodename">>, JObj)}
               ,{<<"Subscription-Call-ID">>, wh_json:get_value(<<"Subscription-Call-ID">>, JObj)}
               ,{<<"Notify-User">>, Username}
               ,{<<"Notify-Realm">>, Realm}
               ,{<<"Message-Account">>, wh_json:get_value(<<"Message-Account">>, JObj, DefaultAccount)}
               | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    lager:debug("updating MWI for ~s (~b/~b)", [DefaultAccount, New, Saved]),
    whapps_util:amqp_pool_send(Command, fun wapi_notifications:publish_mwi_update/1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec presence_mwi_update(ne_binary(), {ne_binary(), ne_binary()}, {ne_binary(), ne_binary()}, wh_json:object()) -> 'ok'.
presence_mwi_update(<<"message-summary">>, {FromUser, FromRealm}, _, JObj) ->
    case whapps_util:get_account_by_realm(FromRealm) of
        {'ok', AccountDb} ->
            lager:debug("replying to mwi presence probe"),
            presence_mwi_resp(FromUser, FromRealm, AccountDb, JObj);
        _E ->
            lager:info("failed to find the account for realm ~s: ~p", [FromRealm, _E])
    end;
presence_mwi_update(_, _, _, _) -> 'ok'.

-spec presence_parking_slot(ne_binary(), {ne_binary(), ne_binary()}, {ne_binary(), ne_binary()}, wh_json:object()) -> 'ok'.
presence_parking_slot(<<"message-summary">>, _, _, _) -> 'ok';
presence_parking_slot(_, {_, FromRealm}, {ToUser, ToRealm}, _) ->
    case whapps_util:get_account_by_realm(FromRealm) of
        {'ok', AccountDb} ->
            maybe_presence_parking_slot_resp(ToUser, ToRealm, AccountDb);
        _E -> 'ok'
    end.

-spec manual_presence(ne_binary(), {ne_binary(), ne_binary()}, {ne_binary(), ne_binary()}, wh_json:object()) -> 'ok'.
manual_presence(<<"message-summary">>, _, _, _) -> 'ok';
manual_presence(_, {_, FromRealm}, {ToUser, ToRealm}, Event) ->
    case whapps_util:get_account_by_realm(FromRealm) of
        {'ok', AccountDb} ->
            check_manual_presence(AccountDb, ToUser, ToRealm, Event);
        _E -> 'ok'
    end.

-spec check_manual_presence(ne_binary(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
check_manual_presence(AccountDb, ToUser, ToRealm, Event) ->
    case wh_cache:fetch_local(?CALLFLOW_CACHE, ?MANUAL_PRESENCE_KEY(AccountDb)) of
        {'ok', JObj} ->
            manual_presence_resp(JObj, ToUser, ToRealm, Event);
        {'error', 'not_found'} ->
            fetch_manual_presence_doc(AccountDb, ToUser, ToRealm, Event)
    end.

-spec fetch_manual_presence_doc(ne_binary(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
fetch_manual_presence_doc(AccountDb, ToUser, ToRealm, Event) ->
    case couch_mgr:open_doc(AccountDb, ?MANUAL_PRESENCE_DOC) of
        {'ok', JObj} ->
            CacheProps = [{'origin', {'db', AccountDb, ?MANUAL_PRESENCE_DOC}}],
            wh_cache:store_local(?CALLFLOW_CACHE, ?MANUAL_PRESENCE_KEY(AccountDb), JObj, CacheProps),
            manual_presence_resp(JObj, ToUser, ToRealm, Event);
        {'error', 'not_found'} ->
            CacheProps = [{'origin', {'db', AccountDb, ?MANUAL_PRESENCE_DOC}}],
            wh_cache:store_local(?CALLFLOW_CACHE, ?MANUAL_PRESENCE_KEY(AccountDb), wh_json:new(), CacheProps);
        {'error', _} -> 'ok'
    end.

-spec manual_presence_resp(wh_json:object(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
manual_presence_resp(JObj, ToUser, ToRealm, Event) ->
    PresenceId = <<ToUser/binary, "@", ToRealm/binary>>,
    case wh_json:get_value(PresenceId, JObj) of
        'undefined' -> 'ok';
        State ->
            PresenceUpdate = [{<<"Presence-ID">>, PresenceId}
                              ,{<<"State">>, State}
                              ,{<<"Call-ID">>, wh_util:to_hex_binary(crypto:md5(PresenceId))}
                              ,{<<"Switch-Nodename">>, wh_json:get_ne_value(<<"Switch-Nodename">>, Event)}
                              ,{<<"Subscription-Call-ID">>, wh_json:get_ne_value(<<"Subscription-Call-ID">>, Event)}
                              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                             ],
            whapps_util:amqp_pool_send(PresenceUpdate, fun wapi_notifications:publish_presence_update/1)
    end.

-spec maybe_presence_parking_slot_resp(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
maybe_presence_parking_slot_resp(Request, Realm, AccountDb) ->
    case wh_cache:fetch_local(?CALLFLOW_CACHE, ?PARKING_PRESENCE_KEY(AccountDb, Request)) of
        {'ok', 'false'} -> 'ok';
        {'ok', SlotNumber} ->
            presence_parking_slot_resp(SlotNumber, Request, Realm, AccountDb);
        {'error', 'not_found'} ->
            maybe_presence_parking_flow(Request, Realm, AccountDb)
    end.

-spec maybe_presence_parking_flow(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
maybe_presence_parking_flow(Request, Realm, AccountDb) ->
    AccountId = wh_util:format_account_id(AccountDb, 'raw'),
    _ = lookup_callflow(Request, AccountId),
    case wh_cache:fetch_local(?CALLFLOW_CACHE, ?CF_FLOW_CACHE_KEY(Request, AccountDb)) of
        {'error', 'not_found'} -> 'ok';
        {'ok', Flow} ->
            case wh_json:get_value([<<"flow">>, <<"module">>], Flow) of
                <<"park">> ->
                    SlotNumber = wh_json:get_ne_value(<<"capture_group">>, Flow, Request),
                    wh_cache:store_local(?CALLFLOW_CACHE, ?PARKING_PRESENCE_KEY(AccountDb, Request), SlotNumber),
                    presence_parking_slot_resp(SlotNumber, Request, Realm, AccountDb);
                _Else ->
                    wh_cache:store_local(?CALLFLOW_CACHE, ?PARKING_PRESENCE_KEY(AccountDb, Request), 'false'),
                    'ok'
            end
    end.

-spec presence_parking_slot_resp(ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
presence_parking_slot_resp(SlotNumber, Request, Realm, AccountDb) ->
    cf_park:update_presence(SlotNumber, <<Request/binary, "@", Realm/binary>>, AccountDb).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec presence_mwi_resp(ne_binary(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
presence_mwi_resp(Username, Realm, AccountDb, JObj) ->
    case owner_ids_by_sip_username(AccountDb, Username) of
        {'ok', [OwnerId]} ->
            presence_mwi_resp(Username, Realm, OwnerId, AccountDb, JObj);
        _Else -> 'ok'
    end.

-spec presence_mwi_resp(ne_binary(), ne_binary(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
presence_mwi_resp(Username, Realm, OwnerId, AccountDb, JObj) ->
    {New, Saved} = vm_count_by_owner(AccountDb, OwnerId),
    send_mwi_update(New, Saved, Username, Realm, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec vm_count_by_owner(ne_binary(), api_binary()) -> {non_neg_integer(), non_neg_integer()}.
vm_count_by_owner(_, 'undefined') ->
    {0, 0};
vm_count_by_owner(AccountDb, OwnerId) ->
    ViewOptions = [{'reduce', 'true'}
                   ,{'group', 'true'}
                   ,{'group_level', 2}
                   ,{'startkey', [OwnerId]}
                   ,{'endkey', [OwnerId, "\ufff0"]}
                  ],
    case couch_mgr:get_results(AccountDb, <<"cf_attributes/vm_count_by_owner">>, ViewOptions) of
        {'ok', MessageCounts} ->
            Props = [{wh_json:get_value([<<"key">>, 2], MessageCount), wh_json:get_value(<<"value">>, MessageCount)}
                     || MessageCount <- MessageCounts
                    ],
            {props:get_value(<<"new">>, Props, 0), props:get_value(<<"saved">>, Props, 0)};
        {'error', _R} ->
            lager:info("unable to lookup vm counts by owner: ~p", [_R]),
            {0, 0}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_endpoint_owner(wh_json:object()) -> api_binary().
get_endpoint_owner(JObj) ->
    maybe_get_endpoint_hotdesk_owner(JObj).

-spec maybe_get_endpoint_hotdesk_owner(wh_json:object()) -> api_binary().
maybe_get_endpoint_hotdesk_owner(JObj) ->
    case wh_json:get_keys([<<"hotdesk">>, <<"users">>], JObj) of
        [] -> maybe_get_endpoint_assigned_owner(JObj);
        [OwnerId] -> OwnerId;
        [_|_] -> 'undefined'
    end.
-spec maybe_get_endpoint_assigned_owner(wh_json:object()) -> api_binary().
maybe_get_endpoint_assigned_owner(JObj) ->
    case wh_json:get_ne_value(<<"owner_id">>, JObj) of
        'undefined' -> 'undefined';
        OwnerId -> OwnerId
    end.

-spec apply_dialplan(ne_binary(), api_object()) -> ne_binary().
apply_dialplan(N, 'undefined') -> N;
apply_dialplan(Number, DialPlan) ->
    Regexs = wh_json:get_keys(DialPlan),
    case Regexs of
        [] -> Number;
        _ -> maybe_apply_dialplan(Regexs, DialPlan, Number)
    end.

maybe_apply_dialplan([], _, Number) -> Number;
maybe_apply_dialplan([Regex|Regexs], DialPlan, Number) ->
    case re:run(Number, Regex, [{'capture', 'all', 'binary'}]) of
        'nomatch' ->
            maybe_apply_dialplan(Regexs, DialPlan, Number);
        'match' ->
            Number;
        {'match', Captures} ->
            Root = lists:last(Captures),
            Prefix = wh_json:get_binary_value([Regex, <<"prefix">>], DialPlan, <<>>),
            Suffix = wh_json:get_binary_value([Regex, <<"suffix">>], DialPlan, <<>>),
            <<Prefix/binary, Root/binary, Suffix/binary>>
    end.

-spec encryption_method_map(api_object(), api_binaries() | wh_json:object()) -> api_object().
encryption_method_map(JObj, []) -> JObj;
encryption_method_map(JObj, [Method|Methods]) ->
    case props:get_value(Method, ?ENCRYPTION_MAP, []) of
        [] -> encryption_method_map(JObj, Methods);
        Values ->
            encryption_method_map(wh_json:set_values(Values , JObj), Method)
    end;
encryption_method_map(JObj, Endpoint) ->
    encryption_method_map(
      JObj
      ,wh_json:get_value([<<"media">>
                          ,<<"encryption">>
                          ,<<"methods">>
                         ], Endpoint, [])).

-spec maybe_start_metaflows(whapps_call:call(), wh_json:objects()) -> 'ok'.
-spec maybe_start_metaflow(whapps_call:call(), wh_json:object()) -> 'ok'.

maybe_start_metaflows(Call, Endpoints) ->
    [maybe_start_metaflow(Call, Endpoint) || Endpoint <- Endpoints],
    'ok'.

maybe_start_metaflow(Call, Endpoint) ->
    case wh_json:get_value(<<"Metaflows">>, Endpoint) of
        'undefined' -> 'ok';
        ?EMPTY_JSON_OBJECT -> 'ok';
        JObj ->
            API = props:filter_undefined(
                    [{<<"Endpoint-ID">>, wh_json:get_value(<<"Endpoint-ID">>, Endpoint)}
                     ,{<<"Call">>, whapps_call:to_json(Call)}
                     ,{<<"Numbers">>, wh_json:get_value(<<"numbers">>, JObj)}
                     ,{<<"Patterns">>, wh_json:get_value(<<"patterns">>, JObj)}
                     ,{<<"Binding-Digit">>, wh_json:get_value(<<"binding_digit">>, JObj)}
                     ,{<<"Digit-Timeout">>, wh_json:get_value(<<"digit_timeout">>, JObj)}
                     ,{<<"Listen-On">>, wh_json:get_value(<<"listen_on">>, JObj, <<"b">>)}
                     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                    ]),
            lager:debug("sending metaflow for endpoint: ~s: ~p", [wh_json:get_value(<<"Endpoint-ID">>, Endpoint), wh_json:get_value(<<"listen_on">>, JObj)]),
            whapps_util:amqp_pool_send(API, fun wapi_dialplan:publish_metaflow/1)
    end.

-spec caller_belongs_to_group(ne_binary(), whapps_call:call()) -> boolean().
caller_belongs_to_group(GroupId, Call) ->
    maybe_belongs_to_group(whapps_call:authorizing_id(Call), GroupId, Call).

-spec maybe_belongs_to_group(ne_binary(), ne_binary(), whapps_call:call()) -> boolean().
maybe_belongs_to_group(TargetId, GroupId, Call) ->
    lists:member(TargetId, find_group_endpoints(GroupId, Call)).

-spec caller_belongs_to_user(ne_binary(), whapps_call:call()) -> boolean().
caller_belongs_to_user(UserId, Call) ->
    lists:member(whapps_call:authorizing_id(Call), find_user_endpoints([UserId],[],Call)).

-spec find_group_endpoints(ne_binary(), whapps_call:call()) -> ne_binaries().
find_group_endpoints(GroupId, Call) ->
    GroupsJObj = cf_attributes:groups(Call),
    case [wh_json:get_value(<<"value">>, JObj)
          || JObj <- GroupsJObj,
             wh_json:get_value(<<"id">>, JObj) =:= GroupId
         ]
    of
        [] -> [];
        [GroupEndpoints] ->
            Ids = wh_json:get_keys(GroupEndpoints),
            find_endpoints(Ids, GroupEndpoints, Call)
    end.

-spec find_endpoints(ne_binaries(), wh_json:object(), whapps_call:call()) ->
                            ne_binaries().
find_endpoints(Ids, GroupEndpoints, Call) ->
    {DeviceIds, UserIds} =
        lists:partition(fun(Id) ->
                                wh_json:get_value([Id, <<"type">>], GroupEndpoints) =:= <<"device">>
                        end, Ids),
    find_user_endpoints(UserIds, lists:sort(DeviceIds), Call).

-spec find_user_endpoints(ne_binaries(), ne_binaries(), whapps_call:call()) ->
                                 ne_binaries().
find_user_endpoints([], DeviceIds, _) -> DeviceIds;
find_user_endpoints(UserIds, DeviceIds, Call) ->
    UserDeviceIds = cf_attributes:owned_by(UserIds, <<"device">>, Call),
    lists:merge(lists:sort(UserDeviceIds), DeviceIds).

-spec find_channels(ne_binaries(), whapps_call:call()) -> wh_json:objects().
find_channels(Usernames, Call) ->
    Realm = wh_util:get_account_realm(whapps_call:account_id(Call)),
    lager:debug("finding channels for realm ~s, usernames ~p", [Realm, Usernames]),
    Req = [{<<"Realm">>, Realm}
           ,{<<"Usernames">>, Usernames}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case whapps_util:amqp_pool_request(Req
                                       ,fun wapi_call:publish_query_user_channels_req/1
                                       ,fun wapi_call:query_user_channels_resp_v/1
                                      )
    of
        {'ok', Resp} -> wh_json:get_value(<<"Channels">>, Resp, []);
        {'error', _E} ->
            lager:debug("failed to get channels: ~p", [_E]),
            []
    end.

-spec check_value_of_fields(wh_proplist(), boolean(), wh_json:object(), whapps_call:call()) ->
                                   boolean().
check_value_of_fields(Perms, Def, Data, Call) ->
    case lists:dropwhile(fun({K, _F}) ->
                                 wh_json:get_value(K, Data) =:= 'undefined'
                         end
                         ,Perms
                        )
    of
        [] -> Def;
        [{K, F}|_] -> F(wh_json:get_value(K, Data), Call)
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

alpha_to_dialpad_test() ->
    ?assertEqual(<<"222">>, alpha_to_dialpad(<<"abc">>)),
    ?assertEqual(<<"23456789">>, alpha_to_dialpad(<<"behknqux">>)),
    ?assertEqual(<<"23456789">>, alpha_to_dialpad(<<"BeHkNqUx">>)),
    ?assertEqual(<<"23456789">>, alpha_to_dialpad(<<"1BeH@k(N$q-u+x=">>)).

-endif.
