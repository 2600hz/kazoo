%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_endpoint).

-include("callflow.hrl").

-export([get/1, get/2]).
-export([flush/2]).
-export([build/2, build/3]).

-define(NON_DIRECT_MODULES, [cf_ring_group, acdc_util]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetches a endpoint defintion from the database or cache
%% @end
%%--------------------------------------------------------------------
-spec get(whapps_call:call()) ->
                       {'ok', wh_json:object()} |
                       {'error', term()}.
-spec get(api_binary(), ne_binary() | whapps_call:call()) ->
                       {'ok', wh_json:object()} |
                       {'error', term()}.
get(Call) ->
    get(whapps_call:authorizing_id(Call), Call).

get('undefined', _Call) ->
    {'error', 'invalid_endpoint_id'};
get(EndpointId, AccountDb) when is_binary(AccountDb) ->
    case wh_cache:peek_local(?CALLFLOW_CACHE, {?MODULE, AccountDb, EndpointId}) of
        {'ok', Endpoint} -> {'ok', Endpoint};
        {'error', 'not_found'} ->
            maybe_fetch_endpoint(EndpointId, AccountDb)
    end;
get(EndpointId, Call) ->
    get(EndpointId, whapps_call:account_db(Call)).

-spec maybe_fetch_endpoint(ne_binary(), ne_binary()) -> wh_jobj_return().
maybe_fetch_endpoint(EndpointId, AccountDb) ->
    case couch_mgr:open_doc(AccountDb, EndpointId) of
        {'ok', JObj} ->
            maybe_have_endpoint(JObj, EndpointId, AccountDb);
        {'error', _R}=E ->
            lager:info("unable to fetch endpoint ~s: ~p", [EndpointId, _R]),
            E
    end.

-spec maybe_have_endpoint(wh_json:object(), ne_binary(), ne_binary()) ->  wh_jobj_return().
maybe_have_endpoint(JObj, EndpointId, AccountDb) ->
    case wh_json:get_value(<<"pvt_type">>, JObj) of
        <<"device">> ->
            Endpoint = merge_attributes(JObj),
            CacheProps = [{'origin', cache_origin(JObj, EndpointId, AccountDb)}],
            wh_cache:store_local(?CALLFLOW_CACHE, {?MODULE, AccountDb, EndpointId}, Endpoint, CacheProps),
            {'ok', Endpoint};
        _Else ->
            lager:info("endpoint module does not manage document type ~s", [_Else]),
            {'error', 'not_device'}
    end.

-spec cache_origin/3 :: (wh_json:object(), ne_binary(), ne_binary()) ->  list().
cache_origin(JObj, EndpointId, AccountDb) ->
    Routines = [fun(P) -> [{'db', AccountDb, EndpointId}|P] end
                ,fun(P) ->
                         [{'db', AccountDb, wh_util:format_account_id(AccountDb, 'raw')}
                          |P
                         ]
                 end
                ,fun(P) -> maybe_cached_owner_id(P, JObj, AccountDb) end
                ,fun(P) -> maybe_cached_hotdesk_ids(P, JObj, AccountDb) end
               ],
    lists:foldl(fun(F, P) -> F(P) end, [], Routines).

-spec maybe_cached_owner_id(wh_proplist(), wh_json:object(), ne_binary()) -> wh_proplist().
maybe_cached_owner_id(Props, JObj, AccountDb) ->
    case wh_json:get_value(<<"owner_id">>, JObj) of
        'undefined' -> Props;
        OwnerId -> [{'db', AccountDb, OwnerId}|Props]
    end.

-spec maybe_cached_hotdesk_ids(wh_proplist(), wh_json:object(), ne_binary()) -> wh_proplist().
maybe_cached_hotdesk_ids(Props, JObj, AccountDb) ->
    case wh_json:get_keys([<<"hotdesk">>, <<"users">>], JObj) of
        'undefined' -> Props;
         OwnerIds ->
            lists:foldl(fun(Id, P) ->
                                [{'db', AccountDb, Id}|P]
                        end, Props, OwnerIds)
    end.

-spec merge_attributes/1 :: (wh_json:object()) -> wh_json:object().
merge_attributes(Endpoint) ->
    Keys = [<<"call_restriction">>
            ,<<"music_on_hold">>
            ,<<"ringtones">>
            ,<<"caller_id">>
            ,<<"caller_id_options">>
            ,<<"name">>
            ,<<"do_not_disturb">>
            ,<<"call_forward">>
            ,?CF_ATTR_LOWER_KEY
           ],
    merge_attributes(Keys, 'undefined', Endpoint, 'undefined').

-spec merge_attributes/4 :: (ne_binaries(), 'undefined' | wh_json:object(), wh_json:object(), 'undefined' | wh_json:object()) ->
                                           wh_json:object().
merge_attributes(Keys, Account, Endpoint, 'undefined') ->
    AccountDb = wh_json:get_value(<<"pvt_account_db">>, Endpoint),
    JObj = get_user(AccountDb, Endpoint),
    Id = wh_json:get_value(<<"_id">>, JObj),
    merge_attributes(Keys
                     ,Account
                     ,wh_json:set_value(<<"owner_id">>, Id, Endpoint)
                     ,JObj);
merge_attributes(Keys, 'undefined', Endpoint, Owner) ->
    AccountDb = wh_json:get_value(<<"pvt_account_db">>, Endpoint),
    AccountId = wh_json:get_value(<<"pvt_account_id">>, Endpoint),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'ok', JObj} ->
            merge_attributes(Keys, JObj, Endpoint, Owner);
        {'error', _} ->
            merge_attributes(Keys, wh_json:new(), Endpoint, Owner)
    end;
merge_attributes([], _, Endpoint, _) -> Endpoint;


merge_attributes([<<"call_restriction">>|Keys], Account, Endpoint, Owner) ->
    Classifiers = wh_json:get_keys(wnm_util:available_classifiers()),
    Update = merge_call_restrictions(Classifiers, Account, Endpoint, Owner),
    merge_attributes(Keys, Account, Update, Owner);
merge_attributes([?CF_ATTR_LOWER_KEY|Keys], Account, Endpoint, Owner) ->
    FullKey = [?CF_ATTR_LOWER_KEY, ?CF_ATTR_UPPER_KEY],
    OwnerAttr = wh_json:get_integer_value(FullKey, Owner, 5),
    EndpointAttr = wh_json:get_integer_value(FullKey, Endpoint, 5),
    case EndpointAttr < OwnerAttr of
        'true' ->
            merge_attributes(Keys, Account, Endpoint, Owner);
        'false' ->
            Update = wh_json:set_value(FullKey, OwnerAttr, Endpoint),
            merge_attributes(Keys, Account, Update, Owner)
    end;
merge_attributes([<<"name">> = Key|Keys], Account, Endpoint, Owner) ->
    AccountName = wh_json:get_ne_value(Key, Account),
    EndpointName = wh_json:get_ne_value(Key, Endpoint),
    FirstName = wh_json:get_ne_value(<<"first_name">>, Owner),
    LastName = wh_json:get_ne_value(<<"last_name">>, Owner),
    Name = create_endpoint_name(FirstName, LastName, EndpointName, AccountName),
    merge_attributes(Keys, Account, wh_json:set_value(Key, Name, Endpoint), Owner);
merge_attributes([<<"call_forward">> = Key|Keys], Account, Endpoint, Owner) ->
    EndpointAttr = wh_json:get_ne_value(Key, Endpoint, wh_json:new()),
    case wh_json:is_true(<<"enabled">>, EndpointAttr) of
        'true' -> merge_attributes(Keys, Account, Endpoint, Owner);
        'false' ->
            AccountAttr = wh_json:get_ne_value(Key, Account, wh_json:new()),
            OwnerAttr = wh_json:get_ne_value(Key, Owner, wh_json:new()),
            Merged1 = wh_json:merge_recursive(AccountAttr, EndpointAttr),
            Merged2 = wh_json:merge_recursive(Merged1, OwnerAttr),
            merge_attributes(Keys, Account, wh_json:set_value(Key, Merged2, Endpoint), Owner)
    end;
merge_attributes([Key|Keys], Account, Endpoint, Owner) ->
    AccountAttr = wh_json:get_ne_value(Key, Account, wh_json:new()),
    EndpointAttr = wh_json:get_ne_value(Key, Endpoint, wh_json:new()),
    OwnerAttr = wh_json:get_ne_value(Key, Owner, wh_json:new()),
    Merged1 = wh_json:merge_recursive(AccountAttr, EndpointAttr),
    Merged2 = wh_json:merge_recursive(Merged1, OwnerAttr),
    merge_attributes(Keys, Account, wh_json:set_value(Key, Merged2, Endpoint), Owner).

-spec merge_call_restrictions/4 :: (ne_binaries(), wh_json:object(), wh_json:object(), wh_json:object()) -> wh_json:object().
merge_call_restrictions([], _, Endpoint, _) -> Endpoint;
merge_call_restrictions([Key|Keys], Account, Endpoint, Owner) ->
    case wh_json:get_value([<<"call_restriction">>, Key, <<"action">>], Account) =:= <<"deny">>
        orelse wh_json:get_value([<<"call_restriction">>, Key, <<"action">>], Owner)
    of
        'true' ->
            %% denied at the account level
            Update = wh_json:set_value([<<"call_restriction">>, Key, <<"action">>], <<"deny">>, Endpoint),
            merge_call_restrictions(Keys, Account, Update, Owner);
        <<"deny">> ->
            %% denied at the user level
            Update = wh_json:set_value([<<"call_restriction">>, Key, <<"action">>], <<"deny">>, Endpoint),
            merge_call_restrictions(Keys, Account, Update, Owner);
        <<"allow">> ->
            %% allowed at the user level
            Update = wh_json:set_value([<<"call_restriction">>, Key, <<"action">>], <<"allow">>, Endpoint),
            merge_call_restrictions(Keys, Account, Update, Owner);
        _Else ->
            %% user inherit or no user, either way use the device restrictions
            merge_call_restrictions(Keys, Account, Endpoint, Owner)
    end.



-spec get_user(ne_binary(), api_binary() | wh_json:object()) -> wh_json:object().
get_user(_, 'undefined') -> wh_json:new();
get_user(AccountDb, OwnerId) when is_binary(OwnerId) ->
    case couch_mgr:open_cache_doc(AccountDb, OwnerId) of
        {'ok', JObj} -> JObj;
        {'error', _R} ->
            lager:warning("failed to load endpoint owner ~s: ~p", [OwnerId, _R]),
            wh_json:new()
    end;
get_user(AccountDb, Endpoint) ->
    case wh_json:get_keys([<<"hotdesk">>, <<"users">>], Endpoint) of
        [] ->
            get_user(AccountDb, wh_json:get_value(<<"owner_id">>, Endpoint));
        [OwnerId] ->
            fix_user_restrictions(get_user(AccountDb, OwnerId));
        [_|_]=OwnerIds->
            J = convert_to_single_user(get_users(AccountDb, OwnerIds)),
            fix_user_restrictions(J)
    end.

-spec get_users(ne_binary(), ne_binaries()) -> wh_json:objects().
get_users(AccountDb, OwnerIds) ->
    get_users(AccountDb, OwnerIds, []).

-spec get_users(ne_binary(), ne_binaries(), wh_json:objects()) -> wh_json:objects().
get_users(_, [], Users) ->
    Users;
get_users(AccountDb, [OwnerId|OwnerIds], Users) ->
    case couch_mgr:open_cache_doc(AccountDb, OwnerId) of
        {'ok', JObj} ->
            get_users(AccountDb, OwnerIds, [JObj|Users]);
        {'error', _R} ->
            lager:warning("failed to load endpoint owner ~s: ~p", [OwnerId, _R]),
            get_users(AccountDb, OwnerIds, Users)
    end.

-spec fix_user_restrictions(wh_json:object()) -> wh_json:object().
fix_user_restrictions(JObj) ->
    lists:foldl(fun(Key, J) ->
                        case wh_json:get_value([<<"call_restriction">>
                                                ,Key
                                                ,<<"action">>
                                               ], J)
                        of
                            <<"deny">> -> J;
                            _Else ->
                                %% this ensures we override the device
                                %% but only when there is a user associated
                                wh_json:set_value([<<"call_restriction">>
                                                   ,Key
                                                   ,<<"action">>
                                                  ], <<"allow">>, J)
                        end
                end, JObj, wh_json:get_keys(wnm_util:available_classifiers())).

-spec convert_to_single_user(wh_json:objects()) -> wh_json:object().
convert_to_single_user(JObjs) ->
    Routines = [fun singlfy_user_attr_keys/2
                ,fun singlfy_user_restrictions/2
               ],
    lists:foldl(fun(F, J) -> F(JObjs, J) end, wh_json:new(), Routines).

-spec singlfy_user_attr_keys(wh_json:objects(), wh_json:object()) -> wh_json:object().
singlfy_user_attr_keys(JObjs, JObj) ->
    Value = lists:foldl(fun(J, V1) ->
                                case wh_json:get_integer_value([?CF_ATTR_LOWER_KEY
                                                                ,?CF_ATTR_UPPER_KEY
                                                               ], J, 5)
                                of
                                    V2 when V2 < V1 -> V2;
                                    _ -> V1
                                end
                        end, 5, JObjs),
    wh_json:set_value([?CF_ATTR_LOWER_KEY, ?CF_ATTR_UPPER_KEY], Value, JObj).

-spec singlfy_user_restrictions(wh_json:objects(), wh_json:object()) -> wh_json:object().
singlfy_user_restrictions(JObjs, JObj) ->
    lists:foldl(fun(Key, J) ->
                        Fun = fun(Elem) -> do_all_restrict(Key, Elem) end,
                        case lists:all(Fun, JObjs) of
                            'false' -> J;
                            'true' ->
                                 wh_json:set_value([<<"call_restriction">>
                                                    ,Key
                                                    ,<<"action">>
                                                   ], <<"deny">>, J)
                        end
                end, JObj, wh_json:get_keys(wnm_util:available_classifiers())).

-spec do_all_restrict(ne_binary(), wh_json:object()) -> boolean().
do_all_restrict(Key, JObj) ->
    wh_json:get_value([<<"call_restriction">>
                       ,Key
                       ,<<"action">>
                      ], JObj) =:= <<"deny">>.


-spec create_endpoint_name(api_binary(), api_binary(), api_binary(), api_binary()) -> api_binary().
create_endpoint_name('undefined', 'undefined', 'undefined', Account) -> Account;
create_endpoint_name('undefined', 'undefined', Endpoint, _) -> Endpoint;
create_endpoint_name(First, 'undefined', _, _) -> First;
create_endpoint_name('undefined', Last, _, _) -> Last;
create_endpoint_name(First, Last, _, _) -> <<First/binary, " ", Last/binary>>.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Flush the callflow cache
%% @end
%%--------------------------------------------------------------------
-spec flush(ne_binary(), ne_binary()) -> any().
flush(Db, Id) ->
    wh_cache:erase_local(?CALLFLOW_CACHE, {?MODULE, Db, Id}),
    {'ok', Rev} = couch_mgr:lookup_doc_rev(Db, Id),
    Props =
        [{<<"ID">>, Id}
         ,{<<"Database">>, Db}
         ,{<<"Rev">>, Rev}
         ,{<<"Type">>, <<"device">>}
         | wh_api:default_headers(<<"configuration">>, <<"doc_edited">>
                                      ,?APP_NAME, ?APP_VERSION)
        ],
    Fun = fun(P) -> wapi_conf:publish_doc_update(<<"edited">>, Db, <<"device">>, Id, P) end,
    whapps_util:amqp_pool_send(Props, Fun).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates one or more whistle API endpoints for use in a bridge string.
%% Takes into account settings on the callflow, the endpoint, call
%% forwarding, and ringtones.  More functionality to come, but as it is
%% added it will be implicit in all functions that 'ring an endpoing'
%% like devices, ring groups, and resources.
%% @end
%%--------------------------------------------------------------------
-type build_errors() :: 'db_not_reachable' | 'endpoint_disabled'
                      | 'endpoint_called_self' | 'endpoint_id_undefined'
                      | 'invalid_endpoint_id' | 'not_found' | 'owner_called_self'
                      | 'do_not_disturb'.

-spec build(api_binary() | wh_json:object(), whapps_call:call()) ->
                         {'ok', wh_json:objects()} |
                         {'error', build_errors()}.
-spec build(api_binary() | wh_json:object(), 'undefined' | wh_json:object(), whapps_call:call()) ->
                         {'ok', wh_json:objects()} |
                         {'error', build_errors()}.

build(EndpointId, Call) ->
    build(EndpointId, wh_json:new(), Call).

build('undefined', _Properties, _Call) ->
    {'error', 'endpoint_id_undefined'};
build(EndpointId, 'undefined', Call) when is_binary(EndpointId) ->
    build(EndpointId, wh_json:new(), Call);
build(EndpointId, Properties, Call) when is_binary(EndpointId) ->
    case ?MODULE:get(EndpointId, Call) of
        {'ok', Endpoint} -> build(Endpoint, Properties, Call);
        {'error', _}=E -> E
    end;
build(Endpoint, Properties, Call) ->
    case should_create_endpoint(Endpoint, Properties, Call) of
        'ok' -> create_endpoints(Endpoint, Properties, Call);
        {'error', _}=E -> E
    end.

-spec should_create_endpoint(wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                          'ok' | {'error', _}.
should_create_endpoint(Endpoint, Properties, Call) ->
    Routines = [fun maybe_owner_called_self/3
                ,fun maybe_endpoint_called_self/3
                ,fun maybe_endpoint_disabled/3
                ,fun maybe_do_not_disturb/3
               ],
    should_create_endpoint(Routines, Endpoint, Properties, Call).

-type ep_routine_v() :: fun((wh_json:object(), wh_json:object(), whapps_call:call()) -> 'ok' | any()).
-type ep_routines_v() :: [ep_routine_v(),...] | [].
-spec should_create_endpoint(ep_routines_v(), wh_json:object(), wh_json:object(),  whapps_call:call()) ->
                                          'ok' | {'error', _}.
should_create_endpoint([], _, _, _) -> 'ok';
should_create_endpoint([Routine|Routines], Endpoint, Properties, Call) when is_function(Routine, 3) ->
    case Routine(Endpoint, Properties, Call) of
        'ok' -> should_create_endpoint(Routines, Endpoint, Properties, Call);
        Else -> Else
    end.

-spec maybe_owner_called_self(wh_json:object(), wh_json:object(),  whapps_call:call()) ->
                                           'ok' |
                                           {'error', 'owner_called_self'}.
maybe_owner_called_self(Endpoint, Properties, Call) ->
    CanCallSelf = wh_json:is_true(<<"can_call_self">>, Properties),
    EndpointOwnerId = wh_json:get_value(<<"owner_id">>, Endpoint),
    OwnerId = whapps_call:kvs_fetch(owner_id, Call),
    case CanCallSelf
        orelse (not is_binary(OwnerId))
        orelse (not is_binary(EndpointOwnerId))
        orelse EndpointOwnerId =/= OwnerId
    of
        'true' -> 'ok';
        'false' ->
            lager:info("owner ~s stop calling your self...stop calling your self...", [OwnerId]),
            {'error', 'owner_called_self'}
    end.

-spec maybe_endpoint_called_self(wh_json:object(), wh_json:object(),  whapps_call:call()) ->
                                              'ok' |
                                              {'error', 'endpoint_called_self'}.
maybe_endpoint_called_self(Endpoint, Properties, Call) ->
    CanCallSelf = wh_json:is_true(<<"can_call_self">>, Properties),
    AuthorizingId = whapps_call:authorizing_id(Call),
    EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
    case CanCallSelf
        orelse (not is_binary(AuthorizingId))
        orelse (not is_binary(EndpointId))
        orelse AuthorizingId =/= EndpointId
    of
        'true' -> 'ok';
        'false' ->
            lager:info("endpoint ~s is calling self", [EndpointId]),
            {'error', 'endpoint_called_self'}
    end.

-spec maybe_endpoint_disabled(wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                           'ok' |
                                           {'error', 'endpoint_disabled'}.
maybe_endpoint_disabled(Endpoint, _, _) ->
    case wh_json:is_false(<<"enabled">>, Endpoint) of
        'false' -> 'ok';
        'true' ->
            EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
            lager:info("endpoint ~s is disabled", [EndpointId]),
            {'error', 'endpoint_disabled'}
    end.

-spec maybe_do_not_disturb(wh_json:object(), wh_json:object(),  whapps_call:call()) ->
                                        'ok' |
                                        {'error', 'do_not_disturb'}.
maybe_do_not_disturb(Endpoint, _, _) ->
    DND = wh_json:get_ne_value(<<"do_not_disturb">>, Endpoint, wh_json:new()),
    case wh_json:is_true(<<"enabled">>, DND) of
        'false' -> 'ok';
        'true' ->
            EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
            lager:info("do not distrub endpoint ~s", [EndpointId]),
            {'error', 'do_not_disturb'}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% creates the actual endpoint json objects for use in the whistle
%% bridge API.
%% @end
%%--------------------------------------------------------------------
-spec create_endpoints(wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                    {'ok', wh_json:objects()} |
                                    {'error', 'no_endpoints'}.
create_endpoints(Endpoint, Properties, Call) ->
    Routines = [fun maybe_create_fwd_endpoint/3
                ,fun maybe_create_endpoint/3
               ],
    Fun = fun(Routine, Endpoints) ->
                  try_create_endpoint(Routine, Endpoints, Endpoint, Properties, Call)
          end,
    case lists:foldl(Fun, [], Routines) of
        [] -> {'error', 'no_endpoints'};
        Endpoints -> {'ok', Endpoints}
    end.

-type ep_routine() :: fun((wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                 {'error', _} | wh_json:object()).
-spec try_create_endpoint(ep_routine(), wh_json:objects(), wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                       wh_json:objects().
try_create_endpoint(Routine, Endpoints, Endpoint, Properties, Call) when is_function(Routine, 3) ->
    try Routine(Endpoint, Properties, Call) of
        {'error', _} -> Endpoints;
        JObj -> [JObj|Endpoints]
    catch
        _E:_R ->
            lager:warning("unable to build endpoint(~s): ~p", [_E, _R]),
            wh_util:log_stacktrace(),
            Endpoints
    end.

-spec maybe_create_fwd_endpoint(wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                             wh_json:object() |
                                             {'error', 'cf_not_appropriate'}.
maybe_create_fwd_endpoint(Endpoint, Properties, Call) ->
    CallFowarding = wh_json:get_ne_value(<<"call_forward">>, Endpoint, wh_json:new()),
    Source = wh_json:get_value(<<"source">>, Properties),
    case wh_json:is_true(<<"enabled">>, CallFowarding)
        andalso (wh_json:is_false(<<"direct_calls_only">>, CallFowarding, 'true')
                 orelse
                   (not lists:member(Source, ?NON_DIRECT_MODULES)))
    of
        'false' -> {'error', 'cf_not_appropriate'};
        'true' ->
            lager:info("creating call forwarding endpoint", []),
            create_call_fwd_endpoint(Endpoint, Properties, CallFowarding, Call)
    end.

-spec maybe_create_endpoint(wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                         wh_json:object() |
                                         {'error', 'cf_substitute'}.
maybe_create_endpoint(Endpoint, Properties, Call) ->
    CallFowarding = wh_json:get_ne_value(<<"call_forward">>, Endpoint, wh_json:new()),
    case wh_json:is_true(<<"enabled">>, CallFowarding)
        andalso wh_json:is_true(<<"substitute">>, CallFowarding)
    of
        'true' -> {'error', 'cf_substitute'};
        'false' ->
            maybe_create_endpoint(get_endpoint_type(Endpoint), Endpoint, Properties, Call)
    end.

-spec maybe_create_endpoint(ne_binary(), wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                         wh_json:object().
maybe_create_endpoint(<<"sip">>, Endpoint, Properties, Call) ->
    lager:info("building a SIP endpoint"),
    create_sip_endpoint(Endpoint, Properties, Call);
maybe_create_endpoint(<<"skype">>, Endpoint, Properties, Call) ->
    lager:info("building a Skype endpoint"),
    create_skype_endpoint(Endpoint, Properties, Call).

-spec get_endpoint_type(wh_json:object()) -> ne_binary().
get_endpoint_type(Endpoint) ->
    case wh_json:get_value(<<"endpoint_type">>, Endpoint) of
        'undefined' -> guess_endpoint_type(Endpoint);
        Type -> Type
    end.

-spec guess_endpoint_type(wh_json:object()) -> ne_binary().
guess_endpoint_type(Endpoint) ->
    guess_endpoint_type(Endpoint, [<<"sip">>, <<"skype">>]).
guess_endpoint_type(Endpoint, [Type|Types]) ->
    case wh_json:get_value(Type, Endpoint) of
        'undefined' -> guess_endpoint_type(Endpoint, Types);
        _ -> Type
    end;
guess_endpoint_type(_Endpoint, []) -> <<"sip">>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates the whistle API endpoint for a bridge call command. This
%% endpoint is comprised of the endpoint definition (commonally a
%% device) and the properties of this endpoint in the callflow.
%% @end
%%--------------------------------------------------------------------
-spec create_sip_endpoint(wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                       wh_json:object().
create_sip_endpoint(Endpoint, Properties, Call) ->
    CIDName = whapps_call:caller_id_name(Call),
    CIDNum = whapps_call:caller_id_number(Call),

    {CalleeNum, CalleeName} = cf_attributes:callee_id(Endpoint, Call),

    {IntCIDNumber, IntCIDName} =
        case cf_attributes:caller_id(<<"internal">>, Call) of
            %% if both the internal name and number are the same as the current
            %% caller id then leave it alone
            {CIDNum, CIDName} -> {'undefined', 'undefined'};
            %% if both the internal name is the same as the current
            %% caller id then leave it alone
            {AltCIDNum, CIDName} -> {AltCIDNum, 'undefined'};
            %% if both the internal number is the same as the current
            %% caller id then leave it alone
            {CIDNum, AltCIDName} -> {'undefined', AltCIDName};
            %% if both the internal number and name are different, use them!
            {AltCIDNum, AltCIDName} -> {AltCIDNum, AltCIDName}
        end,

    OutgoingCIDNum = maybe_format_caller_id_number(Endpoint, IntCIDNumber, Call),

    MediaJObj = wh_json:get_value(<<"media">>, Endpoint),
    SIPJObj = wh_json:get_value(<<"sip">>, Endpoint),

    ForceFax = case wh_json:is_true(<<"fax_option">>, MediaJObj) of
                   'false' -> 'undefined';
                   'true' -> <<"self">>
               end,

    Prop =
        [{<<"Invite-Format">>, invite_format(SIPJObj)}
         ,{<<"To-User">>, to_user(SIPJObj, Properties)}
         ,{<<"To-Username">>, to_username(SIPJObj)}
         ,{<<"To-Realm">>, cf_util:get_sip_realm(Endpoint, whapps_call:account_id(Call))}
         ,{<<"To-DID">>, to_did(Endpoint, Call)}
         ,{<<"To-IP">>, wh_json:get_value(<<"ip">>, SIPJObj)}
         ,{<<"Route">>, wh_json:get_value(<<"route">>, SIPJObj)}
         ,{<<"Proxy-IP">>, wh_json:get_value(<<"proxy">>, SIPJObj)}
         ,{<<"Forward-IP">>, wh_json:get_value(<<"forward">>, SIPJObj)}
         ,{<<"Outgoing-Caller-ID-Number">>, OutgoingCIDNum}
         ,{<<"Outgoing-Caller-ID-Name">>, IntCIDName}
         ,{<<"Callee-ID-Number">>, CalleeNum}
         ,{<<"Callee-ID-Name">>, CalleeName}
         ,{<<"Ignore-Early-Media">>, wh_json:is_true(<<"ignore_early_media">>, MediaJObj)}
         ,{<<"Bypass-Media">>, wh_json:is_true(<<"bypass_media">>, MediaJObj)}
         ,{<<"Endpoint-Progress-Timeout">>, wh_json:is_true(<<"progress_timeout">>, MediaJObj)}
         ,{<<"Endpoint-Timeout">>, wh_json:get_binary_value(<<"timeout">>, Properties)}
         ,{<<"Endpoint-Delay">>, wh_json:get_binary_value(<<"delay">>, Properties)}
         %% TODO: MOVE FROM CF_ATTRIBUTES
%%         ,{<<"Codecs">>, cf_attributes:media_attributes(Endpoint, <<"codecs">>, Call)}
         ,{<<"Hold-Media">>, cf_attributes:moh_attributes(Endpoint, <<"media_id">>, Call)}
         ,{<<"Presence-ID">>, cf_attributes:presence_id(Endpoint, Call)}
         ,{<<"SIP-Headers">>, generate_sip_headers(Endpoint, Call)}
         ,{<<"Custom-Channel-Vars">>, generate_ccvs(Endpoint, Call)}
         ,{<<"Flags">>, wh_json:get_value(<<"outbound_flags">>, Endpoint)}
         ,{<<"Force-Fax">>, ForceFax}
        ],
    wh_json:from_list(props:filter_undefined(Prop)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates the whistle API endpoint for a bridge call command. This
%% endpoint is comprised of the endpoint definition (commonally a
%% device) and the properties of this endpoint in the callflow.
%% @end
%%--------------------------------------------------------------------
-spec create_skype_endpoint(wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                         wh_json:object().
create_skype_endpoint(Endpoint, Properties, _Call) ->
    SkypeJObj = wh_json:get_value(<<"skype">>, Endpoint),

    Prop =
        [{<<"Invite-Format">>, <<"username">>}
         ,{<<"To-User">>, to_user(SkypeJObj, Properties)}
         ,{<<"To-Username">>, to_username(SkypeJObj)}
         ,{<<"Endpoint-Type">>, <<"skype">>}
         ,{<<"Endpoint-Options">>, wh_json:from_list([{<<"Skype-RR">>, <<"true">>}])}
        ],
    wh_json:from_list(props:filter_undefined(Prop)).


-spec invite_format(wh_json:object()) -> ne_binary().
invite_format(SIPJObj) ->
    wh_json:get_value(<<"invite_format">>, SIPJObj, <<"username">>).

-spec to_did(wh_json:object(), whapps_call:call()) -> api_binary().
to_did(Endpoint, Call) ->
    wh_json:get_value([<<"sip">>, <<"number">>]
                      ,Endpoint
                      ,whapps_call:request_user(Call)
                     ).

-spec to_user(wh_json:object(), wh_json:object()) -> api_binary().
to_user(SIPJObj, Properties) ->
    case wh_json:get_ne_value(<<"static_invite">>, Properties) of
        'undefined' ->
            case wh_json:get_ne_value(<<"static_invite">>, SIPJObj) of
                'undefined' -> wh_json:get_value(<<"username">>, SIPJObj);
                To -> To
            end;
        To -> To
    end.

-spec to_username(wh_json:object()) -> api_binary().
to_username(SIPJObj) ->
    wh_json:get_value(<<"username">>, SIPJObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates the whistle API endpoint for a bridge call command when
%% the deivce (or owner) has forwarded their phone.  This endpoint
%% is comprised of a route based on CallFwd, the relevant settings
%% from the actuall endpoint, and the properties of this endpoint in
%% the callflow.
%% @end
%%--------------------------------------------------------------------
-spec create_call_fwd_endpoint(wh_json:object(), wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                            wh_json:object().
create_call_fwd_endpoint(Endpoint, Properties, CallFwd, Call) ->
    lager:info("call forwarding endpoint to ~s", [wh_json:get_value(<<"number">>, CallFwd)]),
    IgnoreEarlyMedia = case wh_json:is_true(<<"require_keypress">>, CallFwd)
                           orelse not wh_json:is_true(<<"substitute">>, CallFwd)
                       of
                           'true' -> <<"true">>;
                           'false' -> wh_json:get_binary_boolean(<<"ignore_early_media">>, CallFwd)
                       end,
    Prop = [{<<"Invite-Format">>, <<"route">>}
            ,{<<"To-DID">>, wh_json:get_value(<<"number">>, Endpoint, whapps_call:request_user(Call))}
            ,{<<"Route">>, <<"loopback/", (wh_json:get_value(<<"number">>, CallFwd, <<"unknown">>))/binary>>}
            ,{<<"Ignore-Early-Media">>, IgnoreEarlyMedia}
            ,{<<"Bypass-Media">>, <<"false">>}
            ,{<<"Endpoint-Progress-Timeout">>, wh_json:get_binary_value([<<"media">>, <<"progress_timeout">>], Endpoint)}
            ,{<<"Endpoint-Timeout">>, wh_json:get_binary_value(<<"timeout">>, Properties)}
            ,{<<"Endpoint-Delay">>, wh_json:get_binary_value(<<"delay">>, Properties)}
            ,{<<"Presence-ID">>, cf_attributes:presence_id(Endpoint, Call)}
            ,{<<"SIP-Headers">>, generate_sip_headers(Endpoint, Call)}
            ,{<<"Custom-Channel-Vars">>, generate_ccvs(Endpoint, Call, CallFwd)}
           ],
    wh_json:from_list(props:filter_undefined(Prop)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will return the sip headers that should be set for
%% the endpoint
%% @end
%%--------------------------------------------------------------------
generate_sip_headers(Endpoint, Call) ->
    Inception = whapps_call:inception(Call),
    HeaderFuns = [fun(J) ->
                          case wh_json:get_value([<<"sip">>, <<"custom_sip_headers">>], Endpoint) of
                              'undefined' -> J;
                              CustomHeaders ->
                                  wh_json:merge_jobjs(CustomHeaders, J)
                          end
                  end
                  ,fun(J) when Inception =:= <<"off-net">> ->
                           case wh_json:get_value([<<"ringtones">>, <<"external">>], Endpoint) of
                               'undefined' -> J;
                               Ringtone ->
                                   wh_json:set_value(<<"Alert-Info">>, Ringtone, J)
                           end;
                      (J) ->
                           case wh_json:get_value([<<"ringtones">>, <<"internal">>], Endpoint) of
                               'undefined' -> J;
                               Ringtone ->
                                   wh_json:set_value(<<"Alert-Info">>, Ringtone, J)
                           end
                   end
                 ],
    lists:foldr(fun(F, JObj) -> F(JObj) end, wh_json:new(), HeaderFuns).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will return the custom channel vars that should be
%% set for this endpoint depending on its settings, and the current
%% call.
%% @end
%%--------------------------------------------------------------------
-spec generate_ccvs(wh_json:object(), whapps_call:call()) -> wh_json:object().
-spec generate_ccvs(wh_json:object(), whapps_call:call(), 'undefined' | wh_json:object()) -> wh_json:object().

generate_ccvs(Endpoint, Call) ->
    generate_ccvs(Endpoint, Call, 'undefined').

generate_ccvs(Endpoint, Call, CallFwd) ->
    CCVFuns = [fun(J) ->
                       case wh_json:is_true(<<"keep_caller_id">>, CallFwd) of
                           'false' -> J;
                           'true' ->
                               lager:info("call forwarding configured to keep the caller id"),
                               wh_json:set_value(<<"Retain-CID">>, <<"true">>, J)
                       end
               end
               ,fun(J) ->
                        case wh_json:get_value(<<"_id">>, Endpoint) of
                            'undefined' -> J;
                            EndpointId ->
                                wh_json:set_value(<<"Authorizing-ID">>, EndpointId, J)
                        end
                end
               ,fun(J) ->
                        case wh_json:get_value(<<"owner_id">>, Endpoint) of
                            'undefined' -> J;
                            OwnerId ->
                                wh_json:set_value(<<"Owner-ID">>, OwnerId, J)
                        end
                end
               ,fun(J) ->
                        case wh_json:get_value(<<"pvt_account_id">>, Endpoint) of
                            'undefined' ->
                                wh_json:set_value(<<"Account-ID">>, whapps_call:account_id(Call), J);
                            PvtAccountId ->
                                wh_json:set_value(<<"Account-ID">>, PvtAccountId, J)
                        end
                end
               ,fun(J) ->
                        case CallFwd of
                            'undefined' -> J;
                            _ ->
                                wh_json:set_values([{<<"Call-Forward">>, <<"true">>}
                                                    ,{<<"Authorizing-Type">>, <<"device">>}
                                                   ], J)
                        end
                end
               ,fun(J) ->
                        case wh_json:is_true(<<"require_keypress">>, CallFwd) of
                            'false' -> J;
                            _ ->
                                lager:info("call forwarding configured to require key press"),
                                Confirm = [{<<"Confirm-Key">>, <<"1">>}
                                           ,{<<"Confirm-Cancel-Timeout">>, <<"2">>}
                                           ,{<<"Confirm-File">>, ?CONFIRM_FILE}],
                                wh_json:merge_jobjs(wh_json:from_list(Confirm), J)
                        end
                end
               ,fun(J) ->
                        case wh_json:get_value([<<"media">>, <<"fax_option">>], Endpoint) of
                            <<"auto">> -> wh_json:set_value(<<"Fax-Enabled">>, <<"true">>, J);
                            _Else -> J
                        end
                end
               ,fun(J) ->
                        case wh_json:get_value(<<"pvt_type">>, Endpoint) of
                            <<"device">> ->
                                lager:info("setting inherit_codec"),
                                wh_json:set_value(<<"Inherit-Codec">>, <<"true">>, J);
                            'false' -> J
                        end
                end
              ],
    lists:foldr(fun(F, J) -> F(J) end, wh_json:new(), CCVFuns).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Conditionally formats the caller id number
%% @end
%%--------------------------------------------------------------------
-spec maybe_format_caller_id_number(wh_json:object(), ne_binary(), whapps_call:call()) -> ne_binary().
maybe_format_caller_id_number(_Endpoint, CIDNum, _Call) ->
%% TODO: MOVE FROM CF_ATTRIBUTES
    CIDNum.
%%    case cf_attributes:caller_id_attributes(Endpoint, <<"format">>, Call) of
%%        'undefined' -> CIDNum;
%%        FormatObj ->
%%            case wh_json:is_json_object(FormatObj) of
%%                false -> CIDNum;
%%                true -> wh_json:foldl(fun(Key, Value, CIDNum1) ->
%%                                              format_caller_id_number_flag(Key, Value, CIDNum1)
%%                                      end, CIDNum, FormatObj)
%%            end
%%    end.
%%
%%-spec format_caller_id_number_flag(ne_binary(), term(), ne_binary()) -> ne_binary().
%%format_caller_id_number_flag(<<"remove_plus">>, true, <<$+, CIDNum/binary>>) -> CIDNum;
%%format_caller_id_number_flag(_Key, _Value, CIDNum) -> CIDNum.
