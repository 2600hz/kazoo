%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(cf_endpoint).

-include("callflow.hrl").

-export([get/1, get/2]).
-export([flush_account/1, flush/2]).
-export([build/2, build/3]).
-export([create_call_fwd_endpoint/3
         ,create_sip_endpoint/3
        ]).

-define(NON_DIRECT_MODULES, ['cf_ring_group', 'acdc_util']).

-define(CF_MOBILE_CONFIG_CAT, <<(?CF_CONFIG_CAT)/binary, ".mobile">>).
-define(DEFAULT_MOBILE_FORMATER, <<"^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})$">>).
-define(DEFAULT_MOBILE_PREFIX, <<>>).
-define(DEFAULT_MOBILE_SUFFIX, <<>>).
-define(DEFAULT_MOBILE_REALM, <<"mobile.k.zswitch.net">>).
-define(DEFAULT_MOBILE_PATH, <<>>).
-define(DEFAULT_MOBILE_CODECS, [<<"PCMU">>]).

-define(RESOURCE_TYPE_SMS, <<"sms">>).
-define(RESOURCE_TYPE_AUDIO, <<"audio">>).
-define(RESOURCE_TYPE_VIDEO, <<"video">>).

-define(DEFAULT_MOBILE_SMS_INTERFACE, <<"amqp">>).
-define(DEFAULT_MOBILE_SMS_BROKER, wh_amqp_connections:primary_broker()).
-define(DEFAULT_MOBILE_SMS_EXCHANGE, <<"sms">>).
-define(DEFAULT_MOBILE_SMS_EXCHANGE_TYPE, <<"topic">>).
-define(DEFAULT_MOBILE_SMS_EXCHANGE_OPTIONS
        ,wh_json:from_list([{'passive', 'true'}])
       ).
-define(DEFAULT_MOBILE_SMS_ROUTE, <<"sprint">>).
-define(DEFAULT_MOBILE_SMS_OPTIONS
        ,wh_json:from_list([{<<"Route-ID">>, ?DEFAULT_MOBILE_SMS_ROUTE}
                            ,{<<"System-ID">>, wh_util:node_name()}
                            ,{<<"Exchange-ID">>, ?DEFAULT_MOBILE_SMS_EXCHANGE}
                            ,{<<"Exchange-Type">>, ?DEFAULT_MOBILE_SMS_EXCHANGE_TYPE}
                           ])
       ).
-define(DEFAULT_MOBILE_AMQP_CONNECTION
        ,wh_json:from_list(
           [{<<"broker">>, ?DEFAULT_MOBILE_SMS_BROKER}
            ,{<<"route">>, ?DEFAULT_MOBILE_SMS_ROUTE}
            ,{<<"exchange">>, ?DEFAULT_MOBILE_SMS_EXCHANGE}
            ,{<<"type">>, ?DEFAULT_MOBILE_SMS_EXCHANGE_TYPE}
            ,{<<"options">>, ?DEFAULT_MOBILE_SMS_EXCHANGE_OPTIONS}
           ])
       ).
-define(DEFAULT_MOBILE_AMQP_CONNECTIONS,
        wh_json:from_list([{<<"default">>, ?DEFAULT_MOBILE_AMQP_CONNECTION}])
       ).

-type sms_route() :: {binary(), wh_proplist() }.
-type sms_routes() :: [sms_route(), ...].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetches a endpoint defintion from the database or cache
%% @end
%%--------------------------------------------------------------------
-spec get(whapps_call:call()) ->
                 {'ok', wh_json:object()} |
                 {'error', _}.
-spec get(api_binary(), ne_binary() | whapps_call:call()) ->
                 {'ok', wh_json:object()} |
                 {'error', _}.
get(Call) -> get(whapps_call:authorizing_id(Call), Call).

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

-spec maybe_fetch_endpoint(ne_binary(), ne_binary()) ->
                                  {'ok', wh_json:object()} |
                                  couch_mgr:couchbeam_error().
maybe_fetch_endpoint(EndpointId, AccountDb) ->
    case couch_mgr:open_cache_doc(AccountDb, EndpointId) of
        {'ok', JObj} ->
            maybe_have_endpoint(JObj, EndpointId, AccountDb);
        {'error', _R}=E ->
            lager:info("unable to fetch endpoint ~s: ~p", [EndpointId, _R]),
            E
    end.

-spec maybe_have_endpoint(wh_json:object(), ne_binary(), ne_binary()) ->
                                 {'ok', wh_json:object()} |
                                 {'error', 'not_device_nor_user'}.
maybe_have_endpoint(JObj, EndpointId, AccountDb) ->
    EndpointTypes = [<<"device">>, <<"user">>, <<"account">>],
    EndpointType = wh_doc:type(JObj),
    case lists:member(EndpointType, EndpointTypes) of
        'false' ->
            lager:info("endpoint module does not manage document type ~s", [EndpointType]),
            {'error', 'not_device_nor_user'};
        'true' ->
            has_endpoint(JObj, EndpointId, AccountDb, EndpointType)
    end.

-spec has_endpoint(wh_json:object(), ne_binary(), ne_binary(), ne_binary()) ->
                          {'ok', wh_json:object()}.
has_endpoint(JObj, EndpointId, AccountDb, EndpointType) ->
    Endpoint = wh_json:set_value(<<"Endpoint-ID">>, EndpointId, merge_attributes(JObj, EndpointType)),
    CacheProps = [{'origin', cache_origin(JObj, EndpointId, AccountDb)}],
    catch wh_cache:store_local(?CALLFLOW_CACHE, {?MODULE, AccountDb, EndpointId}, Endpoint, CacheProps),
    {'ok', Endpoint}.

-spec cache_origin(wh_json:object(), ne_binary(), ne_binary()) ->  list().
cache_origin(JObj, EndpointId, AccountDb) ->
    Routines = [fun(P) -> [{'db', AccountDb, EndpointId} | P] end
                ,fun(P) ->
                         [{'db', AccountDb, wh_util:format_account_id(AccountDb, 'raw')}
                          | P
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
        [] -> Props;
        OwnerIds ->
            lists:foldl(fun(Id, P) ->
                                [{'db', AccountDb, Id}|P]
                        end, Props, OwnerIds)
    end.

-spec merge_attributes(wh_json:object(), ne_binary()) -> wh_json:object().
-spec merge_attributes(wh_json:object(), ne_binary(), ne_binaries()) -> wh_json:object().
merge_attributes(Endpoint, Type) ->
    Keys = [<<"name">>
            ,<<"call_restriction">>
            ,<<"music_on_hold">>
            ,<<"ringtones">>
            ,<<"caller_id">>
            ,<<"caller_id_options">>
            ,<<"do_not_disturb">>
            ,<<"call_forward">>
            ,<<"dial_plan">>
            ,<<"metaflows">>
            ,<<"language">>
            ,<<"record_call">>
            ,<<"mobile">>
            ,<<"presence_id">>
            ,<<"call_waiting">>
            ,?CF_ATTR_LOWER_KEY
           ],
    merge_attributes(Endpoint, Type, Keys).

merge_attributes(Endpoint, <<"user">>, Keys) ->
    merge_attributes(Keys, 'undefined', 'undefined', Endpoint);
merge_attributes(Endpoint, _Type, Keys) ->
    merge_attributes(Keys, 'undefined', Endpoint, 'undefined').

-spec merge_attributes(ne_binaries(), api_object(), api_object(), api_object()) ->
                              wh_json:object().
merge_attributes([], _AccountDoc, Endpoint, _OwnerDoc) -> Endpoint;
merge_attributes(Keys, Account, Endpoint, 'undefined') ->
    AccountDb = wh_doc:account_db(Endpoint),
    JObj = get_user(AccountDb, Endpoint),
    merge_attributes(Keys
                     ,Account
                     ,wh_json:set_value(<<"owner_id">>, wh_doc:id(JObj), Endpoint)
                     ,JObj);
merge_attributes(Keys, Account, 'undefined', Owner) ->
    merge_attributes(Keys, Account, wh_json:new(), Owner);
merge_attributes(Keys, 'undefined', Endpoint, Owner) ->
    case kz_account:fetch(wh_doc:account_id(Endpoint)) of
        {'ok', JObj} -> merge_attributes(Keys, JObj, Endpoint, Owner);
        {'error', _} -> merge_attributes(Keys, wh_json:new(), Endpoint, Owner)
    end;
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
    Name = create_endpoint_name(wh_json:get_ne_value(<<"first_name">>, Owner)
                                ,wh_json:get_ne_value(<<"last_name">>, Owner)
                                ,wh_json:get_ne_value(Key, Endpoint)
                                ,wh_json:get_ne_value(Key, Account)),
    merge_attributes(Keys, Account, wh_json:set_value(Key, Name, Endpoint), Owner);
merge_attributes([<<"call_forward">> = Key|Keys], Account, Endpoint, Owner) ->
    EndpointAttr = wh_json:get_ne_value(Key, Endpoint, wh_json:new()),
    case wh_json:is_true(<<"enabled">>, EndpointAttr) of
        'true' -> merge_attributes(Keys, Account, Endpoint, Owner);
        'false' ->
            AccountAttr = wh_json:get_ne_value(Key, Account, wh_json:new()),
            OwnerAttr = wh_json:get_ne_value(Key, Owner, wh_json:new()),
            Merged = wh_json:merge_recursive([AccountAttr, EndpointAttr, OwnerAttr]),
            merge_attributes(Keys, Account, wh_json:set_value(Key, Merged, Endpoint), Owner)
    end;
merge_attributes([<<"call_waiting">> = Key|Keys], Account, Endpoint, Owner) ->
    AccountAttr = wh_json:get_ne_value(Key, Account, wh_json:new()),
    EndpointAttr = wh_json:get_ne_value(Key, Endpoint, wh_json:new()),
    OwnerAttr = wh_json:get_ne_value(Key, Owner, wh_json:new()),
    %% allow the device to override the owner preference (and vice versa) so
    %%  endpoints such as mobile device can disable call_waiting while sip phone
    %%  might still have it enabled
    Merged = wh_json:merge_recursive([AccountAttr, OwnerAttr, EndpointAttr]),
    merge_attributes(Keys, Account, wh_json:set_value(Key, Merged, Endpoint), Owner);
merge_attributes([<<"caller_id">> = Key|Keys], Account, Endpoint, Owner) ->
    AccountAttr = wh_json:get_ne_value(Key, Account, wh_json:new()),
    EndpointAttr = wh_json:get_ne_value(Key, Endpoint, wh_json:new()),
    OwnerAttr = caller_id_owner_attr(Owner),
    Merged = merge_attribute_caller_id(Account, AccountAttr, OwnerAttr, EndpointAttr),
    case wh_json:get_ne_value([<<"emergency">>, <<"number">>], EndpointAttr) of
        'undefined' ->
            merge_attributes(Keys, Account, wh_json:set_value(Key, Merged, Endpoint), Owner);
        Number ->
            CallerId = wh_json:set_value([<<"emergency">>, <<"number">>], Number, Merged),
            merge_attributes(Keys, Account, wh_json:set_value(Key, CallerId, Endpoint), Owner)
    end;
merge_attributes([<<"do_not_disturb">> = Key|Keys], Account, Endpoint, Owner) ->
    AccountAttr = wh_json:is_true([Key, <<"enabled">>], Account, 'false'),
    EndpointAttr = wh_json:is_true([Key, <<"enabled">>], Endpoint, 'false'),
    OwnerAttr = wh_json:is_true([Key, <<"enabled">>], Owner, 'false'),
    Dnd = AccountAttr orelse OwnerAttr orelse EndpointAttr,
    merge_attributes(Keys, Account, wh_json:set_value([Key, <<"enabled">>], Dnd, Endpoint), Owner);
merge_attributes([<<"language">>|_]=Keys, Account, Endpoint, Owner) ->
    merge_value(Keys, Account, Endpoint, Owner);
merge_attributes([<<"presence_id">>|_]=Keys, Account, Endpoint, Owner) ->
    merge_value(Keys, Account, Endpoint, Owner);
merge_attributes([<<"record_call">> = Key|Keys], Account, Endpoint, Owner) ->
    EndpointAttr = get_record_call_properties(Endpoint),
    AccountAttr = get_record_call_properties(Account),
    OwnerAttr = get_record_call_properties(Owner),
    Merged = wh_json:merge_recursive([AccountAttr, OwnerAttr, EndpointAttr]),
    merge_attributes(Keys, Account, wh_json:set_value(Key, Merged, Endpoint), Owner);
merge_attributes([Key|Keys], Account, Endpoint, Owner) ->
    AccountAttr = wh_json:get_ne_value(Key, Account, wh_json:new()),
    EndpointAttr = wh_json:get_ne_value(Key, Endpoint, wh_json:new()),
    OwnerAttr = wh_json:get_ne_value(Key, Owner, wh_json:new()),
    Merged = wh_json:merge_recursive([AccountAttr, EndpointAttr, OwnerAttr]
                                     ,fun(_, V) -> wh_util:is_not_empty(V) end
                                    ),
    merge_attributes(Keys, Account, wh_json:set_value(Key, Merged, Endpoint), Owner).

-spec merge_attribute_caller_id(api_object(), api_object(), api_object(), api_object()) -> api_object().
merge_attribute_caller_id(AccountJObj, AccountJAttr, UserJAttr, EndpointJAttr) ->
    Merging =
        case wh_json:is_true(<<"prefer_device_caller_id">>, AccountJObj, 'false') of
            'true' -> [AccountJAttr, UserJAttr, EndpointJAttr];
            'false' -> [AccountJAttr, EndpointJAttr, UserJAttr]
        end,
    wh_json:merge_recursive(Merging, fun(_, V) -> wh_util:is_not_empty(V) end).

-spec get_record_call_properties(wh_json:object()) -> wh_json:object().
get_record_call_properties(JObj) ->
    RecordCall = wh_json:get_ne_value(<<"record_call">>, JObj),
    case wh_json:is_json_object(RecordCall) of
        'true' -> RecordCall;
        'false' ->
            case wh_util:is_true(RecordCall) of
                'false' -> wh_json:new();
                'true' ->
                    wh_json:from_list(
                      [{<<"action">>, <<"start">>}
                       ,{<<"record_call">>, 'true'}
                      ]
                     )
            end
    end.

-spec merge_value(ne_binaries(), api_object(), wh_json:object(), api_object()) ->
                              wh_json:object().
merge_value([Key|Keys], Account, Endpoint, Owner) ->
    case wh_json:find(Key, [Owner, Endpoint, Account], 'undefined') of
        'undefined' -> merge_attributes(Keys, Account, Endpoint, Owner);
        Value -> merge_attributes(Keys, Account, wh_json:set_value(Key, Value, Endpoint), Owner)
    end.

-spec caller_id_owner_attr(wh_json:object()) -> wh_json:object().
caller_id_owner_attr(Owner) ->
    OwnerAttr = wh_json:get_ne_value(<<"caller_id">>, Owner, wh_json:new()),
    case wh_json:get_value([<<"internal">>, <<"name">>], OwnerAttr) of
        'undefined' ->
            Name = create_endpoint_name(wh_json:get_ne_value(<<"first_name">>, Owner)
                                        ,wh_json:get_ne_value(<<"last_name">>, Owner)
                                        ,'undefined'
                                        ,'undefined'),
            wh_json:set_value([<<"internal">>, <<"name">>], Name, OwnerAttr);
        _Else -> OwnerAttr
    end.

-spec merge_call_restrictions(ne_binaries(), wh_json:object(), wh_json:object(), wh_json:object()) ->
                                     wh_json:object().
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
-spec flush_account(ne_binary()) -> any().
-spec flush(ne_binary(), ne_binary()) -> any().
flush_account(AccountDb) ->
    ToRemove =
        wh_cache:filter_local(?CALLFLOW_CACHE, fun({?MODULE, Db, _Id}, _Value) ->
                                                       Db =:= AccountDb;
                                                  (_, _) -> 'false'
                                               end),
    _ = [flush(Db, Id)|| {{?MODULE, Db, Id}, _} <- ToRemove],
    'ok'.

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
    Fun = fun(P) ->
                  wapi_conf:publish_doc_update('edited', Db, <<"device">>, Id, P)
          end,
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
                      | 'do_not_disturb' | 'no_resource_type'.

-spec build(api_binary() | wh_json:object(), whapps_call:call()) ->
                   {'ok', wh_json:objects()} |
                   {'error', build_errors()}.
-spec build(api_binary() | wh_json:object(), api_object(), whapps_call:call()) ->
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
    Routines = [fun maybe_missing_resource_type/3
                ,fun maybe_owner_called_self/3
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

-spec maybe_missing_resource_type(wh_json:object(), wh_json:object(),  whapps_call:call()) ->
                                     'ok' |
                                     {'error', 'no_resource_type'}.
-spec maybe_missing_resource_type(api_binary()) ->
                                     'ok' |
                                     {'error', 'no_resource_type'}.
maybe_missing_resource_type(_, _, Call) ->
    maybe_missing_resource_type(whapps_call:resource_type(Call)).

maybe_missing_resource_type('undefined') ->
    lager:error("whapps_call resource type is undefined"),
    wh_util:log_stacktrace(),
    {'error', 'no_resource_type'};
maybe_missing_resource_type(_) -> 'ok'.

-spec maybe_owner_called_self(wh_json:object(), wh_json:object(),  whapps_call:call()) ->
                                     'ok' |
                                     {'error', 'owner_called_self'}.
maybe_owner_called_self(Endpoint, Properties, Call) ->
    maybe_owner_called_self(Endpoint, Properties, whapps_call:resource_type(Call), Call).

-spec maybe_owner_called_self(wh_json:object(), wh_json:object(), api_binary(), whapps_call:call()) ->
                                     'ok' |
                                     {'error', 'owner_called_self'}.
maybe_owner_called_self(Endpoint, Properties, <<"audio">>, Call) ->
    CanCallSelf = wh_json:is_true(<<"can_call_self">>, Properties),
    EndpointOwnerId = wh_json:get_value(<<"owner_id">>, Endpoint),
    OwnerId = whapps_call:kvs_fetch('owner_id', Call),
    case CanCallSelf
        orelse (not is_binary(OwnerId))
        orelse (not is_binary(EndpointOwnerId))
        orelse EndpointOwnerId =/= OwnerId
    of
        'true' -> 'ok';
        'false' ->
            lager:info("owner ~s stop calling your self...stop calling your self...", [OwnerId]),
            {'error', 'owner_called_self'}
    end;
maybe_owner_called_self(Endpoint, Properties, <<"sms">>, Call) ->
    AccountId = whapps_call:account_id(Call),
    DefTextSelf = whapps_account_config:get_global(AccountId, ?CF_CONFIG_CAT, <<"default_can_text_self">>, 'true'),
    CanTextSelf = wh_json:is_true(<<"can_text_self">>, Properties, DefTextSelf),
    EndpointOwnerId = wh_json:get_value(<<"owner_id">>, Endpoint),
    OwnerId = whapps_call:owner_id(Call),
    case CanTextSelf
        orelse (not is_binary(OwnerId))
        orelse (not is_binary(EndpointOwnerId))
        orelse EndpointOwnerId =/= OwnerId
    of
        'true' -> 'ok';
        'false' ->
            lager:info("owner ~s stop texting your self...stop texting your self...", [OwnerId]),
            {'error', 'owner_called_self'}
    end.

-spec maybe_endpoint_called_self(wh_json:object(), wh_json:object(),  whapps_call:call()) ->
                                        'ok' |
                                        {'error', 'endpoint_called_self'}.
maybe_endpoint_called_self(Endpoint, Properties, Call) ->
    maybe_endpoint_called_self(Endpoint, Properties, whapps_call:resource_type(Call), Call).

-spec maybe_endpoint_called_self(wh_json:object(), wh_json:object(), api_binary(), whapps_call:call()) ->
                                        'ok' |
                                        {'error', 'endpoint_called_self'}.
maybe_endpoint_called_self(Endpoint, Properties, <<"audio">>, Call) ->
    CanCallSelf = wh_json:is_true(<<"can_call_self">>, Properties),
    AuthorizingId = whapps_call:authorizing_id(Call),
    EndpointId = wh_doc:id(Endpoint),
    case CanCallSelf
        orelse (not is_binary(AuthorizingId))
        orelse (not is_binary(EndpointId))
        orelse AuthorizingId =/= EndpointId
    of
        'true' -> 'ok';
        'false' ->
            lager:info("endpoint ~s is calling self", [EndpointId]),
            {'error', 'endpoint_called_self'}
    end;
maybe_endpoint_called_self(Endpoint, Properties, <<"sms">>, Call) ->
    AccountId = whapps_call:account_id(Call),
    DefTextSelf = whapps_account_config:get_global(AccountId, ?CF_CONFIG_CAT, <<"default_can_text_self">>, 'true'),
    CanTextSelf = wh_json:is_true(<<"can_text_self">>, Properties, DefTextSelf),
    AuthorizingId = whapps_call:authorizing_id(Call),
    EndpointId = wh_doc:id(Endpoint),
    case CanTextSelf
        orelse (not is_binary(AuthorizingId))
        orelse (not is_binary(EndpointId))
        orelse AuthorizingId =/= EndpointId
    of
        'true' -> 'ok';
        'false' ->
            lager:info("endpoint ~s is texting self", [EndpointId]),
            {'error', 'endpoint_called_self'}
    end.

-spec maybe_endpoint_disabled(wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                           'ok' |
                                           {'error', 'endpoint_disabled'}.
maybe_endpoint_disabled(Endpoint, _, _) ->
    case wh_json:is_false(<<"enabled">>, Endpoint) of
        'false' -> 'ok';
        'true' ->
            lager:info("endpoint ~s is disabled", [wh_doc:id(Endpoint)]),
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
            lager:info("do not distrub endpoint ~s", [wh_doc:id(Endpoint)]),
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
        Endpoints ->
            cf_util:maybe_start_metaflows(Call, Endpoints),
            {'ok', Endpoints}
    end.

-type ep_routine() :: fun((wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                 {'error', _} | wh_json:object()).
-spec try_create_endpoint(ep_routine(), wh_json:objects(), wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                       wh_json:objects().
try_create_endpoint(Routine, Endpoints, Endpoint, Properties, Call) when is_function(Routine, 3) ->
    try Routine(Endpoint, Properties, Call) of
        {'error', _R} ->
            lager:warning("failed to create endpoint: ~p", [_R]),
            Endpoints;
        JObj -> [JObj|Endpoints]
    catch
        _E:_R ->
            lager:warning("unable to build endpoint(~s): ~p", [_E, _R]),
            wh_util:log_stacktrace(),
            Endpoints
    end.

-spec maybe_create_fwd_endpoint(wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                       wh_json:object() |
                                       {'error', 'call_forward_not_appropriate'}.
maybe_create_fwd_endpoint(Endpoint, Properties, Call) ->
    case is_call_forward_enabled(Endpoint, Properties) of
        'false' -> {'error', 'call_forward_not_appropriate'};
        'true' ->
            lager:info("creating call forwarding endpoint"),
            create_call_fwd_endpoint(Endpoint, Properties, Call)
    end.

-spec maybe_create_endpoint(wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                   wh_json:object() |
                                   {'error', 'call_forward_substitute'}.
maybe_create_endpoint(Endpoint, Properties, Call) ->
    case is_call_forward_enabled(Endpoint, Properties)
        andalso wh_json:is_true([<<"call_forward">>, <<"substitute">>], Endpoint)
    of
        'true' -> {'error', 'call_forward_substitute'};
        'false' ->
            EndpointType = get_endpoint_type(Endpoint),
            maybe_create_endpoint(EndpointType, Endpoint, Properties, Call)
    end.

-spec is_call_forward_enabled(wh_json:object(), wh_json:object()) -> boolean().
is_call_forward_enabled(Endpoint, Properties) ->
    CallForwarding = wh_json:get_ne_value(<<"call_forward">>, Endpoint, wh_json:new()),
    Source = wh_json:get_value(<<"source">>, Properties),
    Number = wh_json:get_value(<<"number">>, CallForwarding),
    wh_json:is_true(<<"enabled">>, CallForwarding)
        andalso not wh_util:is_empty(Number)
        andalso (wh_json:is_false(<<"direct_calls_only">>, CallForwarding, 'true')
                 orelse
                   (not lists:member(Source, ?NON_DIRECT_MODULES))).

-spec maybe_create_endpoint(ne_binary(), wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                   wh_json:object() | {'error', ne_binary()}.
maybe_create_endpoint(<<"sip">>, Endpoint, Properties, Call) ->
    lager:info("building a SIP endpoint"),
    create_sip_endpoint(Endpoint, Properties, Call);
maybe_create_endpoint(<<"mobile">>, Endpoint, Properties, Call) ->
    lager:info("building a mobile endpoint"),
    maybe_create_mobile_endpoint(Endpoint, Properties, Call);
maybe_create_endpoint(<<"skype">>, Endpoint, Properties, Call) ->
    lager:info("building a Skype endpoint"),
    create_skype_endpoint(Endpoint, Properties, Call);
maybe_create_endpoint(UnknownType, _, _, _) ->
    {'error', <<"unknown endpoint type ", (wh_util:to_binary(UnknownType))/binary>>}.

-spec maybe_create_mobile_endpoint(wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                          wh_json:object() |
                                          {'error', ne_binary()}.
maybe_create_mobile_endpoint(Endpoint, Properties, Call) ->
    case whapps_config:get_is_true(?CF_MOBILE_CONFIG_CAT, <<"create_sip_endpoint">>, 'false') of
        'true' ->
            lager:info("building mobile as SIP endpoint"),
            create_sip_endpoint(Endpoint, Properties, Call);
        'false' ->
            create_mobile_endpoint(Endpoint, Properties, Call)
    end.

-spec get_endpoint_type(wh_json:object()) -> ne_binary().
get_endpoint_type(Endpoint) ->
    Type = wh_json:get_first_defined([<<"endpoint_type">>
                                      ,<<"device_type">>
                                     ], Endpoint),
    case convert_endpoint_type(Type) of
        'undefined' -> maybe_guess_endpoint_type(Endpoint);
        Else -> Else
    end.

-spec convert_endpoint_type(ne_binary()) -> api_binary().
convert_endpoint_type(<<"sip_", _/binary>>) -> <<"sip">>;
convert_endpoint_type(<<"smartphone">>) -> <<"sip">>;
convert_endpoint_type(<<"softphone">>) -> <<"sip">>;
convert_endpoint_type(<<"cellphone">>) -> <<"sip">>;
convert_endpoint_type(<<"landline">>) -> <<"sip">>;
convert_endpoint_type(<<"fax">>) -> <<"sip">>;
convert_endpoint_type(<<"skype">>) -> <<"skype">>;
convert_endpoint_type(<<"mobile">>) -> <<"mobile">>;
convert_endpoint_type(_Else) -> 'undefined'.

-spec maybe_guess_endpoint_type(wh_json:object()) -> ne_binary().
maybe_guess_endpoint_type(Endpoint) ->
    case whapps_config:get_is_true(?CF_CONFIG_CAT, <<"restrict_to_known_types">>, 'false') of
        'false' -> guess_endpoint_type(Endpoint);
        'true' ->
            lager:info("unknown endpoint type and callflows restrictued to known types", []),
            <<"unknown">>
    end.

-spec guess_endpoint_type(wh_json:object()) -> ne_binary().
guess_endpoint_type(Endpoint) ->
    guess_endpoint_type(Endpoint, [<<"mobile">>
                                   ,<<"sip">>
                                   ,<<"skype">>
                                  ]).
-spec guess_endpoint_type(wh_json:object(), ne_binaries()) -> ne_binary().
guess_endpoint_type(Endpoint, [Type|Types]) ->
    case wh_json:get_ne_value(Type, Endpoint) of
        'undefined' -> guess_endpoint_type(Endpoint, Types);
        _ -> Type
    end;
guess_endpoint_type(Endpoint, []) ->
    case wh_json:get_ne_value(<<"sip">>, Endpoint) of
        'undefined' -> <<"unknown">>;
        _Else -> <<"sip">>
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates the whistle API endpoint for a bridge call command. This
%% endpoint is comprised of the endpoint definition (commonally a
%% device) and the properties of this endpoint in the callflow.
%% @end
%%--------------------------------------------------------------------
-record(clid, {caller_number :: api_binary()
               ,caller_name :: api_binary()
               ,callee_name :: api_binary()
               ,callee_number :: api_binary()
              }).
-type clid() :: #clid{}.

-spec get_clid(wh_json:object(), wh_json:object(), whapps_call:call()) -> clid().
get_clid(Endpoint, Properties, Call) ->
    get_clid(Endpoint, Properties, Call, <<"internal">>).

get_clid(Endpoint, Properties, Call, Type) ->
    case wh_json:is_true(<<"suppress_clid">>, Properties) of
        'true' -> #clid{};
        'false' ->
            {Number, Name} = cf_attributes:caller_id(Type, Call),
            CallerNumber = case whapps_call:caller_id_number(Call) of
                               Number -> 'undefined';
                               _Number -> Number
                           end,
            CallerName = case whapps_call:caller_id_name(Call) of
                             Name -> 'undefined';
                             _Name -> Name
                         end,
            {CalleeNumber, CalleeName} = cf_attributes:callee_id(Endpoint, Call),
            #clid{caller_number=CallerNumber
                  ,caller_name=CallerName
                  ,callee_number=CalleeNumber
                  ,callee_name=CalleeName
                 }
    end.

-spec maybe_record_call(wh_json:object(), whapps_call:call()) -> 'ok'.
maybe_record_call(Endpoint, Call) ->
    case is_call_recording(Call) orelse is_sms(Call) of
        'true' -> 'ok';
        'false' -> maybe_start_call_recording(wh_json:get_value(<<"record_call">>, Endpoint, wh_json:new()), Call)
    end.

-spec is_call_recording(whapps_call:call()) -> boolean().
is_call_recording(Call) ->
    case wh_cache:peek_local(?CALLFLOW_CACHE, call_recording_cache_key(Call)) of
        {'ok', _} -> 'true';
        {'error', 'not_found'} -> 'false'
    end.

-spec call_recording_cache_key(whapps_call:call()) ->
                                      {?MODULE, 'recording', ne_binary()}.
call_recording_cache_key(Call) ->
    {?MODULE, 'recording', whapps_call:call_id(Call)}.

-spec maybe_start_call_recording(wh_json:object(), whapps_call:call()) -> 'ok'.
maybe_start_call_recording(RecordCall, Call) ->
    case wh_util:is_empty(RecordCall) of
        'true' -> 'ok';
        'false' -> start_call_recording(RecordCall, Call)
    end.

-spec start_call_recording(wh_json:object(), whapps_call:call()) -> 'ok'.
start_call_recording(RecordCall, Call) ->
    wh_cache:store_local(
      ?CALLFLOW_CACHE
      ,call_recording_cache_key(Call)
      ,'true'
      ,[{'expires', 60}]
     ),
    Data = wh_json:set_value(<<"spawned">>, 'true', RecordCall),
    _P = wh_util:spawn('cf_record_call', 'handle', [Data, Call]),
    lager:debug("spawned record_call handler in ~p", [_P]).

-spec create_sip_endpoint(wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                 wh_json:object().
create_sip_endpoint(Endpoint, Properties, Call) ->
    Clid = get_clid(Endpoint, Properties, Call),
    _ = maybe_record_call(Endpoint, Call),
    create_sip_endpoint(Endpoint, Properties, Clid, Call).

-spec create_sip_endpoint(wh_json:object(), wh_json:object(), clid(), whapps_call:call()) ->
                                 wh_json:object().
create_sip_endpoint(Endpoint, Properties, #clid{}=Clid, Call) ->
    SIPJObj = wh_json:get_value(<<"sip">>, Endpoint),
    _ = maybe_record_call(Endpoint, Call),
    wh_json:from_list(
      props:filter_empty(
        [{<<"Invite-Format">>, get_invite_format(SIPJObj)}
         ,{<<"To-User">>, get_to_user(SIPJObj, Properties)}
         ,{<<"To-Username">>, get_to_username(SIPJObj)}
         ,{<<"To-Realm">>, cf_util:get_sip_realm(Endpoint, whapps_call:account_id(Call))}
         ,{<<"To-DID">>, get_to_did(Endpoint, Call)}
         ,{<<"To-IP">>, wh_json:get_value(<<"ip">>, SIPJObj)}
         ,{<<"SIP-Transport">>, get_sip_transport(SIPJObj)}
         ,{<<"Route">>, wh_json:get_value(<<"route">>, SIPJObj)}
         ,{<<"Proxy-IP">>, wh_json:get_value(<<"proxy">>, SIPJObj)}
         ,{<<"Forward-IP">>, wh_json:get_value(<<"forward">>, SIPJObj)}
         ,{<<"Callee-ID-Name">>, Clid#clid.callee_name}
         ,{<<"Callee-ID-Number">>, Clid#clid.callee_number}
         ,{<<"Outbound-Callee-ID-Name">>, Clid#clid.callee_name}
         ,{<<"Outbound-Callee-ID-Number">>, Clid#clid.callee_number}
         ,{<<"Outbound-Caller-ID-Number">>, Clid#clid.caller_number}
         ,{<<"Outbound-Caller-ID-Name">>, Clid#clid.caller_name}
         ,{<<"Ignore-Early-Media">>, get_ignore_early_media(Endpoint)}
         ,{<<"Bypass-Media">>, get_bypass_media(Endpoint)}
         ,{<<"Endpoint-Progress-Timeout">>, get_progress_timeout(Endpoint)}
         ,{<<"Endpoint-Timeout">>, get_timeout(Properties)}
         ,{<<"Endpoint-Delay">>, get_delay(Properties)}
         ,{<<"Endpoint-ID">>, wh_doc:id(Endpoint)}
         ,{<<"Codecs">>, get_codecs(Endpoint)}
         ,{<<"Hold-Media">>, cf_attributes:moh_attributes(Endpoint, <<"media_id">>, Call)}
         ,{<<"Presence-ID">>, cf_attributes:presence_id(Endpoint, Call)}
         ,{<<"Custom-SIP-Headers">>, generate_sip_headers(Endpoint, Call)}
         ,{<<"Custom-Channel-Vars">>, generate_ccvs(Endpoint, Call)}
         ,{<<"Flags">>, get_outbound_flags(Endpoint)}
         ,{<<"Ignore-Completed-Elsewhere">>, get_ignore_completed_elsewhere(Endpoint)}
         ,{<<"Failover">>, maybe_build_failover(Endpoint, Clid, Call)}
         ,{<<"Metaflows">>, wh_json:get_value(<<"metaflows">>, Endpoint)}
         | maybe_get_t38(Endpoint, Call)
        ])).

-spec maybe_get_t38(wh_json:object(), whapps_call:call()) -> wh_proplist().
maybe_get_t38(Endpoint, Call) ->
    Opt =
        case cf_endpoint:get(Call) of
            {'ok', JObj} -> wh_json:is_true([<<"media">>, <<"fax_option">>], JObj);
            {'error', _} -> 'undefined'
        end,
    DeviceType = wh_json:get_value(<<"device_type">>, Endpoint),
    case DeviceType =:= <<"fax">> of
        'false' -> [];
        'true' ->
            whapps_call_command:get_inbound_t38_settings(
              Opt
              ,wh_json:get_value(<<"Fax-T38-Enabled">>, Endpoint)
             )
    end.

-spec maybe_build_failover(wh_json:object(), clid(), whapps_call:call()) -> api_object().
maybe_build_failover(Endpoint, Clid, Call) ->
    CallForward = wh_json:get_value(<<"call_forward">>, Endpoint),
    Number = wh_json:get_value(<<"number">>, CallForward),
    case wh_json:is_true(<<"failover">>, CallForward)
        andalso not wh_util:is_empty(Number)
    of
        'false' -> maybe_build_push_failover(Endpoint, Clid, Call);
        'true' -> create_call_fwd_endpoint(Endpoint, wh_json:new(), Call)
    end.

-spec maybe_build_push_failover(wh_json:object(), clid(), whapps_call:call()) -> api_object().
maybe_build_push_failover(Endpoint, Clid, Call) ->
    case wh_json:get_value(<<"push">>, Endpoint) of
        'undefined' -> 'undefined';
        PushJObj -> build_push_failover(Endpoint, Clid, PushJObj, Call)
    end.

-spec build_push_failover(wh_json:object(), clid(), wh_json:object(), whapps_call:call()) -> api_object().
build_push_failover(Endpoint, Clid, PushJObj, Call) ->
    lager:debug("building push failover"),
    SIPJObj = wh_json:get_value(<<"sip">>, Endpoint),
    ToUsername = get_to_username(SIPJObj),
    ToRealm = cf_util:get_sip_realm(Endpoint, whapps_call:account_id(Call)),
    ToUser = <<ToUsername/binary, "@", ToRealm/binary>>,
    Proxy = wh_json:get_value(<<"Token-Proxy">>, PushJObj),
    PushHeaders = wh_json:foldl(fun(K, V, Acc) ->
                                        wh_json:set_value(<<"X-KAZOO-PUSHER-", K/binary>>, V, Acc)
                                end, wh_json:new(), PushJObj),
    wh_json:from_list(
      props:filter_empty(
        [{<<"Invite-Format">>, <<"route">>}
         ,{<<"To-User">>, ToUser}
         ,{<<"To-Username">>, ToUsername}
         ,{<<"To-Realm">>, ToRealm}
         ,{<<"To-DID">>, get_to_did(Endpoint, Call)}
         ,{<<"SIP-Transport">>, get_sip_transport(SIPJObj)}
         ,{<<"Route">>, <<"sip:", ToUser/binary, ";fs_path='", Proxy/binary, "'">> }
         ,{<<"Callee-ID-Name">>, Clid#clid.callee_name}
         ,{<<"Callee-ID-Number">>, Clid#clid.callee_number}
         ,{<<"Outbound-Callee-ID-Name">>, Clid#clid.callee_name}
         ,{<<"Outbound-Callee-ID-Number">>, Clid#clid.callee_number}
         ,{<<"Outbound-Caller-ID-Number">>, Clid#clid.caller_number}
         ,{<<"Outbound-Caller-ID-Name">>, Clid#clid.caller_name}
         ,{<<"Ignore-Early-Media">>, get_ignore_early_media(Endpoint)}
         ,{<<"Bypass-Media">>, get_bypass_media(Endpoint)}
         ,{<<"Endpoint-Progress-Timeout">>, get_progress_timeout(Endpoint)}
         ,{<<"Endpoint-ID">>, wh_doc:id(Endpoint)}
         ,{<<"Codecs">>, get_codecs(Endpoint)}
         ,{<<"Hold-Media">>, cf_attributes:moh_attributes(Endpoint, <<"media_id">>, Call)}
         ,{<<"Presence-ID">>, cf_attributes:presence_id(Endpoint, Call)}
         ,{<<"Custom-SIP-Headers">>, generate_sip_headers(Endpoint, PushHeaders, Call)}
         ,{<<"Custom-Channel-Vars">>, generate_ccvs(Endpoint, Call)}
         ,{<<"Flags">>, get_outbound_flags(Endpoint)}
         ,{<<"Ignore-Completed-Elsewhere">>, get_ignore_completed_elsewhere(Endpoint)}
         ,{<<"Metaflows">>, wh_json:get_value(<<"metaflows">>, Endpoint)}
        ])).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_sip_transport(wh_json:object()) -> ne_binary() | 'undefined'.
get_sip_transport(SIPJObj) ->
    case validate_sip_transport(wh_json:get_value(<<"transport">>, SIPJObj)) of
        'undefined' ->
            validate_sip_transport(whapps_config:get(?CF_CONFIG_CAT, <<"sip_transport">>));
        Transport -> Transport
    end.

-spec validate_sip_transport(any()) -> ne_binary() | 'undefined'.
validate_sip_transport(<<"tcp">>) -> <<"tcp">>;
validate_sip_transport(<<"udp">>) -> <<"udp">>;
validate_sip_transport(<<"tls">>) -> <<"tls">>;
validate_sip_transport(<<"sctp">>) -> <<"sctp">>;
validate_sip_transport(_) -> 'undefined'.

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
         ,{<<"To-User">>, get_to_user(SkypeJObj, Properties)}
         ,{<<"To-Username">>, get_to_username(SkypeJObj)}
         ,{<<"Endpoint-Type">>, <<"skype">>}
         ,{<<"Endpoint-Options">>, wh_json:from_list([{<<"Skype-RR">>, <<"true">>}])}
        ],
    wh_json:from_list(props:filter_undefined(Prop)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates the Kazoo API endpoint for a bridge call command when
%% the device (or owner) has forwarded their phone.  This endpoint
%% is comprised of a route based on CallFwd, the relevant settings
%% from the actuall endpoint, and the properties of this endpoint in
%% the callflow.
%% @end
%%--------------------------------------------------------------------
-spec create_call_fwd_endpoint(wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                      wh_json:object().
create_call_fwd_endpoint(Endpoint, Properties, Call) ->
    CallForward = wh_json:get_ne_value(<<"call_forward">>, Endpoint, wh_json:new()),
    lager:info("call forwarding endpoint to ~s", [wh_json:get_value(<<"number">>, CallForward)]),
    IgnoreEarlyMedia = case wh_json:is_true(<<"require_keypress">>, CallForward)
                           orelse not wh_json:is_true(<<"substitute">>, CallForward)
                       of
                           'true' -> <<"true">>;
                           'false' -> wh_json:get_binary_boolean(<<"ignore_early_media">>, CallForward)
                       end,
    Clid = case whapps_call:inception(Call) of
               'undefined' -> get_clid(Endpoint, Properties, Call, <<"external">>);
               _Else -> #clid{}
           end,
    Prop = [{<<"Invite-Format">>, <<"route">>}
            ,{<<"To-DID">>, wh_json:get_value(<<"number">>, Endpoint, whapps_call:request_user(Call))}
            ,{<<"Route">>, <<"loopback/", (wh_json:get_value(<<"number">>, CallForward, <<"unknown">>))/binary, "/context_2">>}
            ,{<<"Ignore-Early-Media">>, IgnoreEarlyMedia}
            ,{<<"Bypass-Media">>, <<"false">>}
            ,{<<"Endpoint-Progress-Timeout">>, get_progress_timeout(Endpoint)}
            ,{<<"Endpoint-Timeout">>, get_timeout(Properties)}
            ,{<<"Endpoint-Delay">>, get_delay(Properties)}
            ,{<<"Presence-ID">>, cf_attributes:presence_id(Endpoint, Call)}
            ,{<<"Callee-ID-Name">>, Clid#clid.callee_name}
            ,{<<"Callee-ID-Number">>, Clid#clid.callee_number}
            ,{<<"Outbound-Callee-ID-Name">>, Clid#clid.callee_name}
            ,{<<"Outbound-Callee-ID-Number">>, Clid#clid.callee_number}
            ,{<<"Outbound-Caller-ID-Number">>, Clid#clid.caller_number}
            ,{<<"Outbound-Caller-ID-Name">>, Clid#clid.caller_name}
            ,{<<"Custom-SIP-Headers">>, generate_sip_headers(Endpoint, Call)}
            ,{<<"Custom-Channel-Vars">>, generate_ccvs(Endpoint, Call, CallForward)}
           ],
    wh_json:from_list(props:filter_undefined(Prop)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec create_mobile_endpoint(wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                    wh_json:object() |
                                    {'error', ne_binary()}.
create_mobile_endpoint(Endpoint, Properties, Call) ->
    case whapps_call:resource_type(Call) of
        ?RESOURCE_TYPE_SMS -> create_mobile_sms_endpoint(Endpoint, Properties, Call);
        _Other -> create_mobile_audio_endpoint(Endpoint, Properties, Call)
    end.

-spec create_mobile_audio_endpoint(wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                          wh_json:object() |
                                          {'error', ne_binary()}.
create_mobile_audio_endpoint(Endpoint, Properties, Call) ->
    case maybe_build_mobile_route(Endpoint) of
        {'error', _R}=Error ->
            lager:info("unable to build mobile endpoint: ~s", [_R]),
            Error;
        Route ->
            Codecs = whapps_config:get(?CF_MOBILE_CONFIG_CAT, <<"codecs">>, ?DEFAULT_MOBILE_CODECS),
            Prop = [{<<"Invite-Format">>, <<"route">>}
                    ,{<<"Ignore-Early-Media">>, <<"true">>}
                    ,{<<"Route">>, Route}
                    ,{<<"Ignore-Early-Media">>, <<"true">>}
                    ,{<<"Endpoint-Timeout">>, get_timeout(Properties)}
                    ,{<<"Endpoint-Delay">>, get_delay(Properties)}
                    ,{<<"Presence-ID">>, cf_attributes:presence_id(Endpoint, Call)}
                    ,{<<"Custom-SIP-Headers">>, generate_sip_headers(Endpoint, Call)}
                    ,{<<"Codecs">>, Codecs}
                    ,{<<"Custom-Channel-Vars">>, generate_ccvs(Endpoint, Call, wh_json:new())}
                   ],
            wh_json:from_list(props:filter_undefined(Prop))
    end.

-spec maybe_build_mobile_route(wh_json:object()) ->
                                      ne_binary() |
                                      {'error', 'mdn_missing'}.
maybe_build_mobile_route(Endpoint) ->
    case wh_json:get_ne_value([<<"mobile">>, <<"mdn">>], Endpoint) of
        'undefined' ->
            lager:info("unable to build mobile endpoint, MDN missing", []),
            {'error', 'mdn_missing'};
        MDN -> build_mobile_route(MDN)
    end.

-spec build_mobile_route(ne_binary()) ->
                                ne_binary() |
                                {'error', 'invalid_mdn'}.
build_mobile_route(MDN) ->
    Regex = whapps_config:get_binary(?CF_MOBILE_CONFIG_CAT, <<"formatter">>, ?DEFAULT_MOBILE_FORMATER),
    case re:run(MDN, Regex, [{'capture', 'all', 'binary'}]) of
        'nomatch' ->
            lager:info("unable to build mobile endpoint, invalid MDN ~s", [MDN]),
            {'error', 'invalid_mdn'};
        {'match', Captures} ->
            Root = lists:last(Captures),
            Prefix = whapps_config:get_binary(?CF_MOBILE_CONFIG_CAT, <<"prefix">>, ?DEFAULT_MOBILE_PREFIX),
            Suffix = whapps_config:get_binary(?CF_MOBILE_CONFIG_CAT, <<"suffix">>, ?DEFAULT_MOBILE_SUFFIX),
            Realm = whapps_config:get_binary(?CF_MOBILE_CONFIG_CAT, <<"realm">>, ?DEFAULT_MOBILE_REALM),
            Route = <<"sip:"
                      ,Prefix/binary, Root/binary, Suffix/binary
                      ,"@", Realm/binary
                    >>,
            maybe_add_mobile_path(Route)
    end.

-spec maybe_add_mobile_path(ne_binary()) -> ne_binary().
maybe_add_mobile_path(Route) ->
    Path = whapps_config:get_binary(?CF_MOBILE_CONFIG_CAT, <<"path">>, ?DEFAULT_MOBILE_PATH),
    case wh_util:is_empty(Path) of
        'false' -> <<Route/binary, ";fs_path=sip:", Path/binary>>;
        'true' -> Route
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will return the sip headers that should be set for
%% the endpoint
%% @end
%%--------------------------------------------------------------------
-spec generate_sip_headers(wh_json:object(), whapps_call:call()) ->
                                  wh_json:object().
generate_sip_headers(Endpoint, Call) ->
    generate_sip_headers(Endpoint, wh_json:new(), Call).

-spec generate_sip_headers(wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                  wh_json:object().
generate_sip_headers(Endpoint, Acc, Call) ->
    Inception = whapps_call:inception(Call),

    HeaderFuns = [fun maybe_add_sip_headers/1
                  ,fun(J) -> maybe_add_alert_info(J, Endpoint, Inception) end
                  ,fun(J) -> maybe_add_aor(J, Endpoint, Call) end
                 ],
    lists:foldr(fun(F, JObj) -> F(JObj) end, Acc, HeaderFuns).

-spec maybe_add_sip_headers(wh_json:object()) -> wh_json:object().
maybe_add_sip_headers(JObj) ->
    case kz_device:custom_sip_headers(JObj) of
        'undefined' -> JObj;
        CustomHeaders -> wh_json:merge_jobjs(CustomHeaders, JObj)
    end.

-spec maybe_add_alert_info(wh_json:object(), wh_json:object(), api_binary()) -> wh_json:object().
maybe_add_alert_info(JObj, Endpoint, 'undefined') ->
    case wh_json:get_value([<<"ringtones">>, <<"internal">>], Endpoint) of
        'undefined' -> JObj;
        Ringtone -> wh_json:set_value(<<"Alert-Info">>, Ringtone, JObj)
    end;
maybe_add_alert_info(JObj, Endpoint, _Inception) ->
    case wh_json:get_value([<<"ringtones">>, <<"external">>], Endpoint) of
        'undefined' -> JObj;
        Ringtone -> wh_json:set_value(<<"Alert-Info">>, Ringtone, JObj)
    end.

-spec maybe_add_aor(wh_json:object(), wh_json:object(), whapps_call:call()) -> wh_json:object().
-spec maybe_add_aor(wh_json:object(), wh_json:object(), api_binary(), ne_binary()) -> wh_json:object().
maybe_add_aor(JObj, Endpoint, Call) ->
    Realm = kz_device:sip_realm(Endpoint, whapps_call:account_realm(Call)),
    Username = kz_device:sip_username(Endpoint),
    maybe_add_aor(JObj, Endpoint, Username, Realm).

maybe_add_aor(JObj, _, 'undefined', _Realm) -> JObj;
maybe_add_aor(JObj, _, Username, Realm) ->
    wh_json:set_value(<<"X-KAZOO-AOR">>, <<"sip:", Username/binary, "@", Realm/binary>> , JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will return the custom channel vars that should be
%% set for this endpoint depending on its settings, and the current
%% call.
%% @end
%%--------------------------------------------------------------------
-spec generate_ccvs(wh_json:object(), whapps_call:call()) -> wh_json:object().
-spec generate_ccvs(wh_json:object(), whapps_call:call(), api_object()) -> wh_json:object().

generate_ccvs(Endpoint, Call) ->
    generate_ccvs(Endpoint, Call, 'undefined').

generate_ccvs(Endpoint, Call, CallFwd) ->
    CCVFuns = [fun maybe_retain_caller_id/1
               ,fun maybe_set_endpoint_id/1
               ,fun maybe_set_owner_id/1
               ,fun maybe_set_account_id/1
               ,fun maybe_set_call_forward/1
               ,fun maybe_set_confirm_properties/1
               ,fun maybe_enable_fax/1
               ,fun maybe_enforce_security/1
               ,fun maybe_set_encryption_flags/1
               ,fun set_sip_invite_domain/1
               ,fun maybe_set_call_waiting/1
              ],
    Acc0 = {Endpoint, Call, CallFwd, wh_json:new()},
    {_Endpoint, _Call, _CallFwd, JObj} = lists:foldr(fun(F, Acc) -> F(Acc) end, Acc0, CCVFuns),
    JObj.

-type ccv_acc() :: {wh_json:object(), whapps_call:call(), api_object(), wh_json:object()}.

-spec maybe_retain_caller_id(ccv_acc()) -> ccv_acc().
maybe_retain_caller_id({_Endpoint, _Call, 'undefined', _JObj}=Acc) ->
    Acc;
maybe_retain_caller_id({Endpoint, Call, CallFwd, JObj}) ->
    {Endpoint, Call, CallFwd
    ,case whapps_call:inception(Call) =:= 'undefined'
         orelse wh_json:is_true(<<"keep_caller_id">>, CallFwd)
     of
         'true' ->
             lager:info("call forwarding will keep set retain caller id"),
             wh_json:set_value(<<"Retain-CID">>, <<"true">>, JObj);
         'false' -> JObj
     end
    }.

-spec maybe_set_endpoint_id(ccv_acc()) -> ccv_acc().
maybe_set_endpoint_id({Endpoint, Call, CallFwd, JObj}) ->
    {Endpoint, Call, CallFwd
     ,case wh_doc:id(Endpoint) of
          'undefined' -> JObj;
          EndpointId ->
              wh_json:set_value(<<"Authorizing-ID">>, EndpointId, JObj)
      end
    }.

-spec maybe_set_owner_id(ccv_acc()) -> ccv_acc().
maybe_set_owner_id({Endpoint, Call, CallFwd, JObj}) ->
    {Endpoint, Call, CallFwd
     ,case wh_json:get_value(<<"owner_id">>, Endpoint) of
          'undefined' -> JObj;
          OwnerId -> wh_json:set_value(<<"Owner-ID">>, OwnerId, JObj)
      end
    }.

-spec maybe_set_account_id(ccv_acc()) -> ccv_acc().
maybe_set_account_id({Endpoint, Call, CallFwd, JObj}) ->
    AccountId = wh_doc:account_id(Endpoint, whapps_call:account_id(Call)),
    {Endpoint, Call, CallFwd
     ,wh_json:set_value(<<"Account-ID">>, AccountId, JObj)
    }.

-spec maybe_set_call_forward(ccv_acc()) -> ccv_acc().
maybe_set_call_forward({_Endpoint, _Call, 'undefined', _JObj}=Acc) ->
    Acc;
maybe_set_call_forward({Endpoint, Call, CallFwd, JObj}) ->
    {Endpoint, Call, CallFwd
     ,wh_json:set_values([{<<"Call-Forward">>, <<"true">>}
                          ,{<<"Authorizing-Type">>, <<"device">>}
                         ], JObj)
    }.

-spec maybe_set_confirm_properties(ccv_acc()) -> ccv_acc().
maybe_set_confirm_properties({Endpoint, Call, CallFwd, JObj}=Acc) ->
    case wh_json:is_true(<<"require_keypress">>, CallFwd) of
        'false' -> Acc;
        'true' ->
            lager:info("call forwarding configured to require key press"),
            Confirm = [{<<"Confirm-Key">>, <<"1">>}
                       ,{<<"Confirm-Cancel-Timeout">>, <<"2">>}
                       ,{<<"Confirm-File">>, ?CONFIRM_FILE(Call)}
                      ],
            {Endpoint, Call, CallFwd
             ,wh_json:merge_jobjs(wh_json:from_list(Confirm), JObj)
            }
    end.

-spec maybe_enable_fax(ccv_acc()) -> ccv_acc().
maybe_enable_fax({Endpoint, Call, CallFwd, JObj}=Acc) ->
    case wh_json:get_value([<<"media">>, <<"fax_option">>], Endpoint) of
        <<"auto">> ->
            {Endpoint, Call, CallFwd
             ,wh_json:set_value(<<"Fax-Enabled">>, <<"true">>, JObj)
            };
        _Else -> Acc
    end.

-spec maybe_enforce_security(ccv_acc()) -> ccv_acc().
maybe_enforce_security({Endpoint, Call, CallFwd, JObj}) ->
    EnforceSecurity = wh_json:is_true([<<"media">>, <<"encryption">>, <<"enforce_security">>], Endpoint, 'true'),
    {Endpoint, Call, CallFwd
     ,wh_json:set_value(<<"Media-Encryption-Enforce-Security">>, EnforceSecurity, JObj)
    }.

-spec maybe_set_encryption_flags(ccv_acc()) -> ccv_acc().
maybe_set_encryption_flags({Endpoint, Call, CallFwd, JObj}) ->
    {Endpoint, Call, CallFwd
     ,cf_util:encryption_method_map(JObj, Endpoint)
    }.

-spec set_sip_invite_domain(ccv_acc()) -> ccv_acc().
set_sip_invite_domain({Endpoint, Call, CallFwd, JObj}) ->
    {Endpoint, Call, CallFwd
     ,wh_json:set_value(<<"SIP-Invite-Domain">>, whapps_call:request_realm(Call), JObj)
    }.

-spec maybe_set_call_waiting(ccv_acc()) -> ccv_acc().
maybe_set_call_waiting({Endpoint, Call, CallFwd, JObj}) ->
    NewJobj = case wh_json:is_true([<<"call_waiting">>, <<"enabled">>], Endpoint, 'true') of
                  'true' -> JObj;
                  'false' -> wh_json:set_value(<<"Call-Waiting-Disabled">>, 'true', JObj)
              end,
    {Endpoint, Call, CallFwd, NewJobj}.

-spec get_invite_format(wh_json:object()) -> ne_binary().
get_invite_format(SIPJObj) ->
    wh_json:get_value(<<"invite_format">>, SIPJObj, <<"username">>).

-spec get_to_did(wh_json:object(), whapps_call:call()) -> api_binary().
get_to_did(Endpoint, Call) ->
    wh_json:get_value([<<"sip">>, <<"number">>]
                      ,Endpoint
                      ,whapps_call:request_user(Call)
                     ).

-spec get_to_user(wh_json:object(), wh_json:object()) -> api_binary().
get_to_user(SIPJObj, Properties) ->
    case wh_json:get_ne_value(<<"static_invite">>, Properties) of
        'undefined' ->
            case wh_json:get_ne_value(<<"static_invite">>, SIPJObj) of
                'undefined' -> wh_json:get_value(<<"username">>, SIPJObj);
                To -> To
            end;
        To -> To
    end.

-spec get_to_username(wh_json:object()) -> api_binary().
get_to_username(SIPJObj) ->
    wh_json:get_value(<<"username">>, SIPJObj).

-spec get_timeout(wh_json:object()) -> api_binary().
get_timeout(JObj) ->
    case wh_json:get_integer_value(<<"timeout">>, JObj, 0) of
        Timeout when Timeout > 0 -> wh_util:to_binary(Timeout);
        _Else -> 'undefined'
    end.

-spec get_delay(wh_json:object()) -> api_binary().
get_delay(JObj) ->
    case wh_json:get_integer_value(<<"delay">>, JObj, 0) of
        Delay when Delay > 0 -> wh_util:to_binary(Delay);
        _Else -> 'undefined'
    end.

-spec get_outbound_flags(wh_json:object()) -> api_binary().
get_outbound_flags(JObj) ->
    wh_json:get_ne_value(<<"outbound_flags">>, JObj).

-spec get_progress_timeout(wh_json:object()) -> api_binary().
get_progress_timeout(JObj) ->
    case wh_json:get_integer_value([<<"media">>, <<"progress_timeout">>], JObj, 0) of
        Timeout when Timeout > 0 -> wh_util:to_binary(Timeout);
        _Else -> 'undefined'
    end.

-spec get_ignore_early_media(wh_json:object()) -> api_binary().
get_ignore_early_media(JObj) ->
    case wh_json:is_true([<<"media">>, <<"ignore_early_media">>], JObj) of
        'true' -> <<"true">>;
        'false' -> 'undefined'
    end.

-spec get_bypass_media(wh_json:object()) -> api_binary().
get_bypass_media(JObj) ->
    case wh_json:is_true([<<"media">>, <<"peer_to_peer">>], JObj) of
        'true' -> <<"true">>;
        'false' -> 'undefined'
    end.

-spec get_codecs(wh_json:object()) -> 'undefined' | ne_binaries().
get_codecs(JObj) ->
    case wh_json:get_value([<<"media">>, <<"audio">>, <<"codecs">>], JObj, [])
        ++ wh_json:get_value([<<"media">>, <<"video">>, <<"codecs">>], JObj, [])
    of
        [] -> 'undefined';
        Codecs -> Codecs
    end.

-spec get_ignore_completed_elsewhere(wh_json:object()) -> boolean().
get_ignore_completed_elsewhere(JObj) ->
    case wh_json:get_first_defined([[<<"caller_id_options">>, <<"ignore_completed_elsewhere">>]
                                    ,[<<"sip">>, <<"ignore_completed_elsewhere">>]
                                    ,<<"ignore_completed_elsewhere">>
                                   ], JObj)
    of
        'undefined' -> whapps_config:get_is_true(?CF_CONFIG_CAT, <<"default_ignore_completed_elsewhere">>, 'true');
        IgnoreCompletedElsewhere -> wh_util:is_true(IgnoreCompletedElsewhere)
    end.

-spec is_sms(whapps_call:call()) -> boolean().
is_sms(Call) ->
    whapps_call:resource_type(Call) =:= <<"sms">>.

-spec create_mobile_sms_endpoint(wh_json:object(), wh_json:object(), whapps_call:call()) ->
                                        wh_json:object() |
                                        {'error', ne_binary()}.
create_mobile_sms_endpoint(Endpoint, Properties, Call) ->
    case maybe_build_mobile_sms_route(Endpoint) of
        {'error', _R}=Error ->
            lager:info("unable to build mobile sms endpoint: ~s", [_R]),
            Error;
        {Type, [{Route, Options} | Failover]} ->
            Clid = get_clid(Endpoint, Properties, Call),
            Prop = props:filter_undefined(
                     [{<<"Invite-Format">>, <<"route">>}
                      ,{<<"Endpoint-Type">>, Type}
                      ,{<<"Route">>, Route}
                      ,{<<"Endpoint-Options">>, wh_json:from_list(Options)}
                      ,{<<"To-DID">>, get_to_did(Endpoint, Call)}
                      ,{<<"Callee-ID-Name">>, Clid#clid.callee_name}
                      ,{<<"Callee-ID-Number">>, Clid#clid.callee_number}
                      ,{<<"Presence-ID">>, cf_attributes:presence_id(Endpoint, Call)}
                      ,{<<"Custom-SIP-Headers">>, generate_sip_headers(Endpoint, Call)}
                      ,{<<"Custom-Channel-Vars">>, generate_ccvs(Endpoint, Call, wh_json:new())}
                     ]),
            EP = create_mobile_sms_endpoint_failover(Prop, Failover),
            wh_json:from_list(EP)
    end.

-spec create_mobile_sms_endpoint_failover(wh_proplist(), sms_routes()) -> wh_proplist().
create_mobile_sms_endpoint_failover(Endpoint, []) -> Endpoint;
create_mobile_sms_endpoint_failover(Endpoint, [{Route, Options} | Failover]) ->
    EP = props:set_values(
                      [{<<"Route">>, Route}
                       ,{<<"Endpoint-Options">>, wh_json:from_list(Options)}
                      ], Endpoint),
    props:set_value(<<"Failover">>, wh_json:from_list(create_mobile_sms_endpoint_failover(EP, Failover)), Endpoint).


-spec maybe_build_mobile_sms_route(wh_json:object()) ->
                                          ne_binary() |
                                          {'error', 'mdn_missing'}.
maybe_build_mobile_sms_route(Endpoint) ->
    case wh_json:get_ne_value([<<"mobile">>, <<"mdn">>], Endpoint) of
        'undefined' ->
            lager:info("unable to build mobile sms endpoint, MDN missing"),
            {'error', 'mdn_missing'};
        MDN -> build_mobile_sms_route(MDN)
    end.

-spec build_mobile_sms_route(ne_binary()) ->
                                    {ne_binary(), sms_routes()} |
                                    {'error', 'invalid_mdn'}.
build_mobile_sms_route(MDN) ->
    Type = whapps_config:get(?CF_MOBILE_CONFIG_CAT, <<"sms_interface">>, ?DEFAULT_MOBILE_SMS_INTERFACE),
    build_mobile_sms_route(Type, MDN).

-spec build_mobile_sms_route(ne_binary(), ne_binary()) ->
                                    {ne_binary(), sms_routes()} |
                                    {'error', 'invalid_mdn'}.
build_mobile_sms_route(<<"sip">>, MDN) ->
    {<<"sip">>, [{build_mobile_route(MDN), 'undefined'}]};
build_mobile_sms_route(<<"amqp">>, _MDN) ->
    Connections = whapps_config:get(?CF_MOBILE_CONFIG_CAT, [<<"sms">>, <<"connections">>], ?DEFAULT_MOBILE_AMQP_CONNECTIONS),
    {<<"amqp">>, wh_json:foldl(fun build_mobile_sms_amqp_route/3 , [], Connections)}.

-spec build_mobile_sms_amqp_route(wh_json:key(), wh_json:json_term(), wh_proplist()) -> sms_routes().
build_mobile_sms_amqp_route(K, JObj, Acc) ->
    Broker = wh_json:get_value(<<"broker">>, JObj),
    Acc ++ [{Broker, [{<<"Broker-Name">>, K} | build_mobile_sms_amqp_route_options(JObj)]}].

-spec build_mobile_sms_amqp_route_options(wh_json:object()) -> wh_proplist().
build_mobile_sms_amqp_route_options(JObj) ->
    [{<<"Route-ID">>, wh_json:get_value(<<"route">>, JObj, ?DEFAULT_MOBILE_SMS_ROUTE)}
     ,{<<"Exchange-ID">>, wh_json:get_value(<<"exchange">>, JObj, ?DEFAULT_MOBILE_SMS_EXCHANGE)}
     ,{<<"Exchange-Type">>, wh_json:get_value(<<"type">>, JObj, ?DEFAULT_MOBILE_SMS_EXCHANGE_TYPE)}
     ,{<<"Exchange-Options">>, wh_json:get_value(<<"options">>, JObj, ?DEFAULT_MOBILE_SMS_EXCHANGE_OPTIONS)}
    ].
