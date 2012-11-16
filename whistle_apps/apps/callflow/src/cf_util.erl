%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_util).

-include("callflow.hrl").

-export([presence_probe/2]).
-export([presence_mwi_query/2]).
-export([update_mwi/1, update_mwi/2, update_mwi/4]).
-export([alpha_to_dialpad/1, ignore_early_media/1]).
-export([correct_media_path/2]).
-export([lookup_callflow/1, lookup_callflow/2]).
-export([handle_bridge_failure/2, handle_bridge_failure/3]).
-export([send_default_response/2]).
-export([get_sip_realm/2, get_sip_realm/3]).
-export([handle_doc_change/2]).
-export([get_operator_callflow/1]).
-export([default_caller_id_number/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec presence_probe/2 :: (wh_json:json_object(), proplist()) -> any().
presence_probe(JObj, _Props) ->
    ToUser = wh_json:get_value(<<"To-User">>, JObj),
    ToRealm = wh_json:get_value(<<"To-Realm">>, JObj),
    FromUser = wh_json:get_value(<<"From-User">>, JObj),
    FromRealm = wh_json:get_value(<<"From-Realm">>, JObj),
    Subscription = wh_json:get_value(<<"Subscription">>, JObj),
    ProbeRepliers = [fun presence_mwi_update/4
                     ,fun presence_parking_slot/4
                     ,fun manual_presence/4
                    ],
    [Fun(Subscription, {FromUser, FromRealm}, {ToUser, ToRealm}, JObj) || Fun <- ProbeRepliers].

-spec presence_mwi_update/4 :: (ne_binary(), {ne_binary(), ne_binary()}, {ne_binary(), ne_binary()}, wh_json:json_object()) -> ok.
presence_mwi_update(<<"message-summary">>, {FromUser, FromRealm}, _, JObj) ->
    case whapps_util:get_account_by_realm(FromRealm) of
        {ok, AccountDb} ->
            ViewOptions = [include_docs
                           ,{key, FromUser}
                          ],
            case couch_mgr:get_results(AccountDb, <<"cf_attributes/sip_credentials">>, ViewOptions) of
                {ok, []} ->
                    lager:debug("sip credentials not in account db ~s", [AccountDb]),
                    ok;
                {ok, [Device]} -> 
                    lager:debug("replying to mwi presence probe"),
                    OwnerId = wh_json:get_value([<<"doc">>, <<"owner_id">>], Device),
                    presence_mwi_resp(FromUser, FromRealm, OwnerId, AccountDb, JObj);
                {error, _R} -> 
                    lager:debug("unable to lookup sip credentials for owner id: ~p", [_R]),
                    ok
            end;
        _E ->
            lager:debug("failed to find the account for realm ~s: ~p", [FromRealm, _E]),
            ok
    end;
presence_mwi_update(_, _, _, _) ->
    ok.

-spec presence_parking_slot/4 :: (ne_binary(), {ne_binary(), ne_binary()}, {ne_binary(), ne_binary()}, wh_json:json_object()) -> ok.
presence_parking_slot(<<"message-summary">>, _, _, _) ->
    ok;
presence_parking_slot(_, {_, FromRealm}, {ToUser, ToRealm}, _) ->
    case whapps_util:get_account_by_realm(FromRealm) of
        {ok, AccountDb} ->
            AccountId = wh_util:format_account_id(AccountDb, raw),
            _ = lookup_callflow(ToUser, AccountId),
            case wh_cache:fetch_local(?CALLFLOW_CACHE, {cf_flow, ToUser, AccountDb}) of
                {error, not_found} -> ok;
                {ok, Flow} ->
                    case wh_json:get_value([<<"flow">>, <<"module">>], Flow) of
                        <<"park">> -> 
                            lager:debug("replying to presence query for a parking slot"),
                            SlotNumber = wh_json:get_ne_value(<<"capture_group">>, Flow, ToUser),
                            cf_park:update_presence(SlotNumber, <<ToUser/binary, "@", ToRealm/binary>>, AccountDb);
                        _Else -> ok
                    end
            end;
        _E -> ok
    end.

-spec manual_presence/4 :: (ne_binary(), {ne_binary(), ne_binary()}, {ne_binary(), ne_binary()}, wh_json:json_object()) -> ok.
manual_presence(<<"message-summary">>, _, _, _) ->
    ok;
manual_presence(_, {_, FromRealm}, {ToUser, ToRealm}, Event) ->
    case whapps_util:get_account_by_realm(FromRealm) of
        {ok, AccountDb} ->
            case couch_mgr:open_doc(AccountDb, ?MANUAL_PRESENCE_DOC) of
                {error, _} -> ok;
                {ok, JObj} ->
                    PresenceId = <<ToUser/binary, "@", ToRealm/binary>>,
                    case wh_json:get_value(PresenceId, JObj) of
                        undefined -> ok;
                        State ->
                            PresenceUpdate = [{<<"Presence-ID">>, PresenceId}
                                              ,{<<"State">>, State}
                                              ,{<<"Call-ID">>, wh_util:to_hex_binary(crypto:md5(PresenceId))}
                                              ,{<<"Switch-Nodename">>, wh_json:get_ne_value(<<"Switch-Nodename">>, Event)}
                                              ,{<<"Subscription-Call-ID">>, wh_json:get_ne_value(<<"Subscription-Call-ID">>, Event)}
                                              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                             ],
                            wapi_notifications:publish_presence_update(PresenceUpdate)
                        end
            end;
        _E -> ok
    end.
%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec presence_mwi_query/2 :: (wh_json:json_object(), proplist()) -> 'ok'.
presence_mwi_query(JObj, _Props) -> 
    _ = wh_util:put_callid(JObj),
    Username = wh_json:get_value(<<"Username">>, JObj), 
    Realm = wh_json:get_value(<<"Realm">>, JObj),
    case whapps_util:get_account_by_realm(Realm) of
        {ok, AccountDb} ->
            ViewOptions = [include_docs
                           ,{key, Username}
                          ],
            case couch_mgr:get_results(AccountDb, <<"cf_attributes/sip_credentials">>, ViewOptions) of
                {ok, []} ->  ok;
                {ok, [Device]} -> 
                    lager:debug("replying to mwi query"),
                    OwnerId = wh_json:get_value([<<"doc">>, <<"owner_id">>], Device),
                    presence_mwi_resp(Username, Realm, OwnerId, AccountDb, JObj);
                {error, _R} -> ok
            end;
        _Else -> ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec presence_mwi_resp/5 :: (ne_binary(), ne_binary(), 'undefined' | ne_binary(), ne_binary(), wh_json:json_object()) -> 'ok'.
presence_mwi_resp(_, _, undefined, _, _) -> ok;
presence_mwi_resp(Username, Realm, OwnerId, AccountDb, JObj) ->
    ViewOptions = [{reduce, true}
                   ,{group, true}
                   ,{group_level, 2}
                   ,{startkey, [OwnerId]}
                   ,{endkey, [OwnerId, "\ufff0"]}
                  ],
    case couch_mgr:get_results(AccountDb, <<"cf_attributes/vm_count_by_owner">>, ViewOptions) of
        {ok, MessageCounts} -> 
            Props = [{wh_json:get_value([<<"key">>, 2], MessageCount), wh_json:get_value(<<"value">>, MessageCount)}
                     || MessageCount <- MessageCounts
                    ],
            New = props:get_value(<<"new">>, Props, 0),
            Saved = props:get_value(<<"saved">>, Props, 0),
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
            lager:debug("updating MWI for owner ~s: (~b/~b)", [OwnerId, New, Saved]),
            wapi_notifications:publish_mwi_update(Command);
        {error, _R} ->
            lager:debug("unable to lookup vm counts by owner: ~p", [_R]),
            ok
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec update_mwi/1 :: (whapps_call:call()) -> ok.
-spec update_mwi/2 :: (undefined | ne_binary(), ne_binary()) -> ok.
-spec update_mwi/4 :: (non_neg_integer() | ne_binary(), non_neg_integer() | ne_binary(), ne_binary(), ne_binary()) -> ok.

update_mwi(Call) ->
    update_mwi(whapps_call:kvs_fetch(owner_id, Call), whapps_call:account_db(Call)).

update_mwi(undefined, _) ->
    ok;
update_mwi(OwnerId, AccountDb) ->
    ViewOptions = [{reduce, true}
                   ,{group, true}
                   ,{group_level, 2}
                   ,{startkey, [OwnerId]}
                   ,{endkey, [OwnerId, "\ufff0"]}
                  ],
    case couch_mgr:get_results(AccountDb, <<"cf_attributes/vm_count_by_owner">>, ViewOptions) of
        {ok, MessageCounts} -> 
            Props = [{wh_json:get_value([<<"key">>, 2], MessageCount), wh_json:get_value(<<"value">>, MessageCount)}
                     || MessageCount <- MessageCounts
                    ],
            update_mwi(props:get_value(<<"new">>, Props, 0), props:get_value(<<"saved">>, Props, 0), OwnerId, AccountDb);
        {error, _R} ->
            lager:debug("unable to lookup vm counts by owner: ~p", [_R]),
            ok
    end.

update_mwi(New, Saved, OwnerId, AccountDb) ->
    AccountId = wh_util:format_account_id(AccountDb, raw),
    ViewOptions = [{key, [OwnerId, <<"device">>]}
                   ,include_docs
                  ],
    case couch_mgr:get_results(AccountDb, <<"cf_attributes/owned">>, ViewOptions) of
        {ok, Devices} -> 
            lager:debug("updating MWI for owner ~s: (~b/~b) on ~b devices", [OwnerId, New, Saved, length(Devices)]),
            CommonHeaders = [{<<"Messages-New">>, New}
                             ,{<<"Messages-Saved">>, Saved}
                             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                            ],
            lists:foreach(fun(Result) ->
                                  Device = wh_json:get_value(<<"doc">>, Result, wh_json:new()),
                                  User = wh_json:get_value([<<"sip">>, <<"username">>], Device),
                                  Realm = cf_util:get_sip_realm(Device, AccountId),
                                  Command = wh_json:from_list([{<<"Notify-User">>, User}
                                                               ,{<<"Notify-Realm">>, Realm}
                                                               ,{<<"Message-Account">>, <<"sip:", User/binary, "@", Realm/binary>>}
                                                               | CommonHeaders
                                                              ]),
                                  catch (wapi_notifications:publish_mwi_update(Command))
                          end, Devices),
            ok;
        {error, _R} ->
            lager:debug("failed to find devices owned by ~s: ~p", [OwnerId, _R]),
            ok
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec alpha_to_dialpad/1 :: (ne_binary()) -> ne_binary().
alpha_to_dialpad(Value) ->
    << <<(dialpad_digit(C))>> || <<C>> <= strip_nonalpha(wh_util:to_lower_binary(Value))>>.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec strip_nonalpha/1 :: (ne_binary()) -> ne_binary().
strip_nonalpha(Value) ->
    re:replace(Value, <<"[^[:alpha:]]">>, <<>>, [{return,binary}, global]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec dialpad_digit/1 :: (Char) -> 50..57 when
      Char :: 97..122.
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
-spec ignore_early_media/1 :: (wh_json:json_objects()) -> 'undefined' | ne_binary().
ignore_early_media([]) -> undefined;
ignore_early_media(Endpoints) ->
    Ignore = lists:foldr(fun(Endpoint, Acc) ->
                                 wh_json:is_true(<<"Ignore-Early-Media">>, Endpoint)
                                     or Acc
                         end, false, Endpoints),
    wh_util:to_binary(Ignore).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% given a media path that is just a media id correct it to include
%% the account id
%% @end
%%--------------------------------------------------------------------
-spec correct_media_path/2 :: ('undefined' | ne_binary(), whapps_call:call()) -> 'undefined' | ne_binary().
correct_media_path(undefined, _) ->
    undefined;
correct_media_path(<<"silence_stream://", _/binary>> = Media, _) ->
    Media;
correct_media_path(<<"tone_stream://", _/binary>> = Media, _) ->
    Media;
correct_media_path(Media, Call) ->
    case binary:match(Media, <<"/">>) of
        nomatch ->
            <<$/, (whapps_call:account_id(Call))/binary, $/, Media/binary>>;
        _Else ->
            Media
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec get_operator_callflow/1 :: (ne_binary()) -> {'ok', wh_json:json_object()} |
                                                  {'error', _}.
get_operator_callflow(Account) ->
    AccountDb = wh_util:format_account_id(Account, encoded),
    Options = [{key, <<"0">>}, include_docs],
    case couch_mgr:get_results(AccountDb, ?LIST_BY_NUMBER, Options) of
        {ok, []} -> {error, not_found};
        {ok, [JObj|_]} ->
            {ok, wh_json:get_value([<<"doc">>, <<"flow">>], JObj, wh_json:new())};
        {error, _R}=E ->
            lager:debug("unable to find operator callflow in ~s: ~p", [Account, _R]),
            E
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% lookup the callflow based on the requested number in the account
%% @end
%%-----------------------------------------------------------------------------
-type lookup_callflow_ret() :: {'ok', wh_json:json_object(), boolean()} |
                               {'error', term()}.

-spec lookup_callflow/1 :: (whapps_call:call()) -> lookup_callflow_ret().
-spec lookup_callflow/2 :: (ne_binary(), ne_binary()) -> lookup_callflow_ret().
lookup_callflow(Call) ->
    lookup_callflow(whapps_call:request_user(Call), whapps_call:account_id(Call)).

lookup_callflow(Number, AccountId) ->
    case wh_util:is_empty(Number) of
        true -> {error, invalid_number};
        false ->
            Db = wh_util:format_account_id(AccountId, encoded),
            do_lookup_callflow(wh_util:to_binary(Number), Db)
    end.

do_lookup_callflow(Number, Db) ->
    lager:debug("searching for callflow in ~s to satisfy '~s'", [Db, Number]),
    Options = [{key, Number}, include_docs],
    case couch_mgr:get_results(Db, ?LIST_BY_NUMBER, Options) of
        {ok, []} when Number =/= ?NO_MATCH_CF ->
            case lookup_callflow_patterns(Number, Db) of
                {error, _} ->
                    maybe_use_nomatch(Number, Db);
                {ok, {Flow, Capture}} ->
                    F = wh_json:set_value(<<"capture_group">>, Capture, Flow),
                    wh_cache:store_local(?CALLFLOW_CACHE, {cf_flow, Number, Db}, F),
                    {ok, F, false}
            end;
        {ok, []} ->
            {error, not_found};
        {ok, [JObj]} ->
            Flow = wh_json:get_value(<<"doc">>, JObj),
            wh_cache:store_local(?CALLFLOW_CACHE, {cf_flow, Number, Db}, Flow),
            {ok, Flow, Number =:= ?NO_MATCH_CF};
        {ok, [JObj | _Rest]} ->
            lager:debug("lookup resulted in more than one result, using the first"),
            Flow = wh_json:get_value(<<"doc">>, JObj),
            wh_cache:store_local(?CALLFLOW_CACHE, {cf_flow, Number, Db}, Flow),
            {ok, Flow, Number =:= ?NO_MATCH_CF};
        {error, _}=E ->
            E
    end.

%% only route to nomatch when Number is all digits and/or +
maybe_use_nomatch(<<"+", Number/binary>>, Db) ->
    maybe_use_nomatch(Number, Db);
maybe_use_nomatch(Number, Db) ->
    case lists:all(fun is_digit/1, wh_util:to_list(Number)) of
        true -> do_lookup_callflow(?NO_MATCH_CF, Db);
        false ->
            lager:debug("can't use no_match: number not all digits: ~s", [Number]),
            {error, not_found}
    end.

is_digit(X) when X >= $0, X =< $9 -> true;
is_digit(_) -> false.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% send a route response for a route request that can be fulfilled by this
%% process
%% @end
%%-----------------------------------------------------------------------------
-spec lookup_callflow_patterns/2 :: (ne_binary(), ne_binary())
                                    -> {'ok', {wh_json:json_object(), ne_binary()}} | {'error', term()}.
lookup_callflow_patterns(Number, Db) ->
    lager:debug("lookup callflow patterns for ~s in ~s", [Number, Db]),
    case couch_mgr:get_results(Db, ?LIST_BY_PATTERN, [include_docs]) of
        {ok, Patterns} ->
            case test_callflow_patterns(Patterns, Number, {undefined, <<>>}) of
                {undefined, <<>>} -> {error, not_found};
                {Flow, <<>>} -> {ok, {Flow, undefined}};
                Match -> {ok, Match}
            end;
        {error, _}=E ->
            E
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec test_callflow_patterns/3 :: (wh_json:json_objects(), ne_binary()
                                   ,{'undefined', <<>>} | {wh_json:json_object(), ne_binary()})
                                  -> {'undefined', <<>>} | {wh_json:json_object(), ne_binary()}.
test_callflow_patterns([], _, Result) ->
    Result;
test_callflow_patterns([Pattern|T], Number, {_, Capture}=Result) ->
    Regex = wh_json:get_value(<<"key">>, Pattern),
    case re:run(Number, Regex) of
        {match, [{Start,End}]} ->
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
        {match, CaptureGroups} ->
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
%% @public
%% @doc
%% Look for children branches to handle the failure replies of
%% certain actions, like cf_offnet and cf_resources
%% @end
%%--------------------------------------------------------------------
-spec handle_bridge_failure/2 :: ({'fail', wh_json:json_object()} | ne_binary() | 'undefined', whapps_call:call()) -> 'ok' | 'not_found'.
-spec handle_bridge_failure/3 :: (ne_binary() | 'undefined', ne_binary() | 'undefined', whapps_call:call()) -> 'ok' | 'not_found'.

handle_bridge_failure({fail, Reason}, Call) ->
    {Cause, Code} = whapps_util:get_call_termination_reason(Reason),
    handle_bridge_failure(Cause, Code, Call);
handle_bridge_failure(undefined, _) ->
    not_found;
handle_bridge_failure(Failure, Call) ->
    case cf_exe:attempt(Failure, Call) of
        {attempt_resp, ok} ->
            lager:debug("found child branch to handle failure: ~s", [Failure]),
            ok;
        {attempt_resp, _} ->
            not_found
    end.

handle_bridge_failure(Cause, Code, Call) ->
    lager:debug("attempting to find failure branch for ~s:~s", [Code, Cause]),
    case (handle_bridge_failure(Cause, Call) =:= ok)
        orelse (handle_bridge_failure(Code, Call) =:= ok) of
        true -> ok;
        false ->
            cf_exe:continue(Call),
            not_found
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Send and wait for a call failure cause response
%% @end
%%--------------------------------------------------------------------
-spec send_default_response/2 :: (ne_binary(), whapps_call:call()) -> 'ok'.
send_default_response(Cause, Call) ->
    case cf_exe:wildcard_is_empty(Call) of
        false -> ok;
        true ->
            CallId = cf_exe:callid(Call),
            CtrlQ = cf_exe:control_queue(Call),
            case wh_call_response:send_default(CallId, CtrlQ, Cause) of
                {error, no_response} -> ok;
                {ok, NoopId} -> _ = whapps_call_command:wait_for_noop(NoopId), ok
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get the sip realm
%% @end
%%--------------------------------------------------------------------
-spec get_sip_realm/2 :: (wh_json:json_object(), ne_binary()) -> 'undefined' | ne_binary().
-spec get_sip_realm/3 :: (wh_json:json_object(), ne_binary(), Default) -> Default | ne_binary().

get_sip_realm(SIPJObj, AccountId) ->
    get_sip_realm(SIPJObj, AccountId, undefined).

get_sip_realm(SIPJObj, AccountId, Default) ->
    case wh_json:get_ne_value([<<"sip">>, <<"realm">>], SIPJObj) of
        undefined -> 
            case wh_util:get_account_realm(AccountId) of
                undefined -> Default;
                Else -> Else
            end;
        Realm -> Realm
    end.

-spec handle_doc_change/2 :: (wh_json:json_object(), wh_proplist()) -> any().
handle_doc_change(JObj, _Prop) ->
    Db = wh_json:get_value(<<"Account-DB">>, JObj),
    Id = wh_json:get_value(<<"ID">>, JObj),

    _ = cf_endpoint:flush(Db, Id),

    _ = cf_attributes:flush_attributes(Db),

    maybe_clear_flows(wh_json:get_value(<<"numbers">>, JObj, []), Db).

maybe_clear_flows([], _) -> ok;
maybe_clear_flows(Ns, Db) ->
    [wh_cache:erase_local(?CALLFLOW_CACHE, {cf_flow, N, Db}) || N <- Ns].

-spec default_caller_id_number/0 :: () -> ne_binary().
default_caller_id_number() ->
    whapps_config:get(?CF_CONFIG_CAT, <<"default_caller_id_number">>, ?DEFAULT_CALLER_ID_NUMBER).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

alpha_to_dialpad_test() ->
    ?assertEqual(<<"222">>, alpha_to_dialpad(<<"abc">>)),
    ?assertEqual(<<"23456789">>, alpha_to_dialpad(<<"behknqux">>)),
    ?assertEqual(<<"23456789">>, alpha_to_dialpad(<<"BeHkNqUx">>)),
    ?assertEqual(<<"23456789">>, alpha_to_dialpad(<<"1BeH@k(N$q-u+x=">>)).

-endif.
