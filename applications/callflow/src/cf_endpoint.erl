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

-export([get/1, get/2]).
-export([flush_account/1, flush/2]).
-export([build/2, build/3]).
-export([create_call_fwd_endpoint/3
         ,create_sip_endpoint/3
        ]).

-include("callflow.hrl").
-include_lib("kazoo/include/kapi_conf.hrl").

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
-define(DEFAULT_MOBILE_SMS_BROKER, kz_amqp_connections:primary_broker()).
-define(DEFAULT_MOBILE_SMS_EXCHANGE, <<"sms">>).
-define(DEFAULT_MOBILE_SMS_EXCHANGE_TYPE, <<"topic">>).
-define(DEFAULT_MOBILE_SMS_EXCHANGE_OPTIONS
        ,kz_json:from_list([{'passive', 'true'}])
       ).
-define(DEFAULT_MOBILE_SMS_ROUTE, <<"sprint">>).
-define(DEFAULT_MOBILE_SMS_OPTIONS
        ,kz_json:from_list([{<<"Route-ID">>, ?DEFAULT_MOBILE_SMS_ROUTE}
                            ,{<<"System-ID">>, kz_util:node_name()}
                            ,{<<"Exchange-ID">>, ?DEFAULT_MOBILE_SMS_EXCHANGE}
                            ,{<<"Exchange-Type">>, ?DEFAULT_MOBILE_SMS_EXCHANGE_TYPE}
                           ])
       ).
-define(DEFAULT_MOBILE_AMQP_CONNECTION
        ,kz_json:from_list(
           [{<<"broker">>, ?DEFAULT_MOBILE_SMS_BROKER}
            ,{<<"route">>, ?DEFAULT_MOBILE_SMS_ROUTE}
            ,{<<"exchange">>, ?DEFAULT_MOBILE_SMS_EXCHANGE}
            ,{<<"type">>, ?DEFAULT_MOBILE_SMS_EXCHANGE_TYPE}
            ,{<<"options">>, ?DEFAULT_MOBILE_SMS_EXCHANGE_OPTIONS}
           ])
       ).
-define(DEFAULT_MOBILE_AMQP_CONNECTIONS,
        kz_json:from_list([{<<"default">>, ?DEFAULT_MOBILE_AMQP_CONNECTION}])
       ).

-type sms_route() :: {binary(), kz_proplist()}.
-type sms_routes() :: [sms_route(), ...].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetches a endpoint defintion from the database or cache
%% @end
%%--------------------------------------------------------------------
-spec get(kapps_call:call()) -> cf_api_std_return().
-spec get(api_binary(), ne_binary() | kapps_call:call()) -> cf_api_std_return().

get(Call) -> get(kapps_call:authorizing_id(Call), Call).

get('undefined', _Call) ->
    {'error', 'invalid_endpoint_id'};
get(EndpointId, AccountDb) when is_binary(AccountDb) ->
    case kz_cache:peek_local(?CACHE_NAME, {?MODULE, AccountDb, EndpointId}) of
        {'ok', Endpoint} -> {'ok', Endpoint};
        {'error', 'not_found'} ->
            maybe_fetch_endpoint(EndpointId, AccountDb)
    end;
get(EndpointId, Call) ->
    get(EndpointId, kapps_call:account_db(Call)).

-spec maybe_fetch_endpoint(ne_binary(), ne_binary()) ->
                                  {'ok', kz_json:object()} |
                                  kz_data:data_error().
maybe_fetch_endpoint(EndpointId, AccountDb) ->
    case kz_datamgr:open_cache_doc(AccountDb, EndpointId) of
        {'ok', JObj} ->
            maybe_have_endpoint(JObj, EndpointId, AccountDb);
        {'error', _R}=E ->
            lager:info("unable to fetch endpoint ~s: ~p", [EndpointId, _R]),
            E
    end.

-spec maybe_have_endpoint(kz_json:object(), ne_binary(), ne_binary()) ->
                                 {'ok', kz_json:object()} |
                                 {'error', 'not_device_nor_user'}.
maybe_have_endpoint(JObj, EndpointId, AccountDb) ->
    EndpointTypes = [<<"device">>, <<"user">>, <<"account">>],
    EndpointType = endpoint_type_as(kz_doc:type(JObj)),
    case lists:member(EndpointType, EndpointTypes) of
        'false' ->
            lager:info("endpoint module does not manage document type ~s", [EndpointType]),
            {'error', 'not_device_nor_user'};
        'true' ->
            has_endpoint(JObj, EndpointId, AccountDb, EndpointType)
    end.

-spec endpoint_type_as(api_binary()) -> api_binary().
endpoint_type_as(<<"click2call">>) -> <<"device">>;
endpoint_type_as(Type) -> Type.

-spec has_endpoint(kz_json:object(), ne_binary(), ne_binary(), ne_binary()) ->
                          {'ok', kz_json:object()}.
has_endpoint(JObj, EndpointId, AccountDb, EndpointType) ->
    Endpoint = kz_json:set_value(<<"Endpoint-ID">>, EndpointId, merge_attributes(JObj, EndpointType)),
    CacheProps = [{'origin', cache_origin(JObj, EndpointId, AccountDb)}],
    catch kz_cache:store_local(?CACHE_NAME, {?MODULE, AccountDb, EndpointId}, Endpoint, CacheProps),
    {'ok', Endpoint}.

-spec cache_origin(kz_json:object(), ne_binary(), ne_binary()) -> list().
cache_origin(JObj, EndpointId, AccountDb) ->
    Routines = [fun(P) -> [{'db', AccountDb, EndpointId} | P] end
                ,fun(P) ->
                         [{'db', AccountDb, kz_util:format_account_id(AccountDb, 'raw')}
                          | P
                         ]
                 end
                ,fun(P) -> maybe_cached_owner_id(P, JObj, AccountDb) end
                ,fun(P) -> maybe_cached_hotdesk_ids(P, JObj, AccountDb) end
               ],
    lists:foldl(fun(F, P) -> F(P) end, [], Routines).

-spec maybe_cached_owner_id(kz_proplist(), kz_json:object(), ne_binary()) -> kz_proplist().
maybe_cached_owner_id(Props, JObj, AccountDb) ->
    case kz_json:get_value(<<"owner_id">>, JObj) of
        'undefined' -> Props;
        OwnerId -> [{'db', AccountDb, OwnerId}|Props]
    end.

-spec maybe_cached_hotdesk_ids(kz_proplist(), kz_json:object(), ne_binary()) -> kz_proplist().
maybe_cached_hotdesk_ids(Props, JObj, AccountDb) ->
    case kz_json:get_keys([<<"hotdesk">>, <<"users">>], JObj) of
        [] -> Props;
        OwnerIds ->
            lists:foldl(fun(Id, P) ->
                                [{'db', AccountDb, Id}|P]
                        end, Props, OwnerIds)
    end.

-spec merge_attributes(kz_json:object(), ne_binary()) -> kz_json:object().
-spec merge_attributes(kz_json:object(), ne_binary(), ne_binaries()) -> kz_json:object().
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

merge_attributes(Owner, <<"user">>, Keys) ->
    case kz_account:fetch(kz_doc:account_id(Owner)) of
        {'ok', Account} -> merge_attributes(Keys, Account, kz_json:new(), Owner);
        {'error', _} -> merge_attributes(Keys, kz_json:new(), kz_json:new(), Owner)
    end;
merge_attributes(Device, <<"device">>, Keys) ->
    Owner = get_user(kz_doc:account_db(Device), Device),
    Endpoint = kz_json:set_value(<<"owner_id">>, kz_doc:id(Owner), Device),
    case kz_account:fetch(kz_doc:account_id(Device)) of
        {'ok', Account} -> merge_attributes(Keys, Account, Endpoint, Owner);
        {'error', _} -> merge_attributes(Keys, kz_json:new(), Endpoint, Owner)
    end;
merge_attributes(Account, <<"account">>, Keys) ->
   merge_attributes(Keys, Account, kz_json:new(), kz_json:new());
merge_attributes(Endpoint, Type, _Keys) ->
    lager:debug("unhandled endpoint type on merge attributes : ~p : ~p", [Type, Endpoint]),
    kz_json:new().

-spec merge_attributes(ne_binaries(), api_object(), api_object(), api_object()) ->
                              kz_json:object().
merge_attributes([], _AccountDoc, Endpoint, _OwnerDoc) -> Endpoint;
merge_attributes([<<"call_restriction">>|Keys], Account, Endpoint, Owner) ->
    Classifiers = kz_json:get_keys(knm_converters:available_classifiers()),
    Update = merge_call_restrictions(Classifiers, Account, Endpoint, Owner),
    merge_attributes(Keys, Account, Update, Owner);
merge_attributes([?CF_ATTR_LOWER_KEY|Keys], Account, Endpoint, Owner) ->
    FullKey = [?CF_ATTR_LOWER_KEY, ?CF_ATTR_UPPER_KEY],
    OwnerAttr = kz_json:get_integer_value(FullKey, Owner, 5),
    EndpointAttr = kz_json:get_integer_value(FullKey, Endpoint, 5),
    case EndpointAttr < OwnerAttr of
        'true' ->
            merge_attributes(Keys, Account, Endpoint, Owner);
        'false' ->
            Update = kz_json:set_value(FullKey, OwnerAttr, Endpoint),
            merge_attributes(Keys, Account, Update, Owner)
    end;
merge_attributes([<<"name">> = Key|Keys], Account, Endpoint, Owner) ->
    Name = create_endpoint_name(kz_json:get_ne_value(<<"first_name">>, Owner)
                                ,kz_json:get_ne_value(<<"last_name">>, Owner)
                                ,kz_json:get_ne_value(Key, Endpoint)
                                ,kz_json:get_ne_value(Key, Account)),
    merge_attributes(Keys, Account, kz_json:set_value(Key, Name, Endpoint), Owner);
merge_attributes([<<"call_forward">> = Key|Keys], Account, Endpoint, Owner) ->
    EndpointAttr = kz_json:get_ne_value(Key, Endpoint, kz_json:new()),
    case kz_json:is_true(<<"enabled">>, EndpointAttr) of
        'true' -> merge_attributes(Keys, Account, Endpoint, Owner);
        'false' ->
            AccountAttr = kz_json:get_ne_value(Key, Account, kz_json:new()),
            OwnerAttr = kz_json:get_ne_value(Key, Owner, kz_json:new()),
            Merged = kz_json:merge_recursive([AccountAttr, EndpointAttr, OwnerAttr]),
            merge_attributes(Keys, Account, kz_json:set_value(Key, Merged, Endpoint), Owner)
    end;
merge_attributes([<<"call_waiting">> = Key|Keys], Account, Endpoint, Owner) ->
    AccountAttr = kz_json:get_ne_value(Key, Account, kz_json:new()),
    EndpointAttr = kz_json:get_ne_value(Key, Endpoint, kz_json:new()),
    OwnerAttr = kz_json:get_ne_value(Key, Owner, kz_json:new()),
    %% allow the device to override the owner preference (and vice versa) so
    %%  endpoints such as mobile device can disable call_waiting while sip phone
    %%  might still have it enabled
    Merged = kz_json:merge_recursive([AccountAttr, OwnerAttr, EndpointAttr]),
    merge_attributes(Keys, Account, kz_json:set_value(Key, Merged, Endpoint), Owner);
merge_attributes([<<"caller_id">> = Key|Keys], Account, Endpoint, Owner) ->
    AccountAttr = kz_json:get_ne_value(Key, Account, kz_json:new()),
    EndpointAttr = kz_json:get_ne_value(Key, Endpoint, kz_json:new()),
    OwnerAttr = caller_id_owner_attr(Owner),
    Merged = merge_attribute_caller_id(Account, AccountAttr, OwnerAttr, EndpointAttr),
    case kz_json:get_ne_value([<<"emergency">>, <<"number">>], EndpointAttr) of
        'undefined' ->
            merge_attributes(Keys, Account, kz_json:set_value(Key, Merged, Endpoint), Owner);
        Number ->
            CallerId = kz_json:set_value([<<"emergency">>, <<"number">>], Number, Merged),
            merge_attributes(Keys, Account, kz_json:set_value(Key, CallerId, Endpoint), Owner)
    end;
merge_attributes([<<"do_not_disturb">> = Key|Keys], Account, Endpoint, Owner) ->
    AccountAttr = kz_json:is_true([Key, <<"enabled">>], Account, 'false'),
    EndpointAttr = kz_json:is_true([Key, <<"enabled">>], Endpoint, 'false'),
    OwnerAttr = kz_json:is_true([Key, <<"enabled">>], Owner, 'false'),
    Dnd = AccountAttr orelse OwnerAttr orelse EndpointAttr,
    merge_attributes(Keys, Account, kz_json:set_value([Key, <<"enabled">>], Dnd, Endpoint), Owner);
merge_attributes([<<"language">>|_]=Keys, Account, Endpoint, Owner) ->
    merge_value(Keys, Account, Endpoint, Owner);
merge_attributes([<<"presence_id">>|_]=Keys, Account, Endpoint, Owner) ->
    merge_value(Keys, Account, Endpoint, Owner);
merge_attributes([<<"record_call">> = Key|Keys], Account, Endpoint, Owner) ->
    EndpointAttr = get_record_call_properties(Endpoint),
    AccountAttr = get_record_call_properties(Account),
    OwnerAttr = get_record_call_properties(Owner),
    Merged = kz_json:merge_recursive([AccountAttr, OwnerAttr, EndpointAttr]),
    merge_attributes(Keys, Account, kz_json:set_value(Key, Merged, Endpoint), Owner);
merge_attributes([Key|Keys], Account, Endpoint, Owner) ->
    AccountAttr = kz_json:get_ne_value(Key, Account, kz_json:new()),
    EndpointAttr = kz_json:get_ne_value(Key, Endpoint, kz_json:new()),
    OwnerAttr = kz_json:get_ne_value(Key, Owner, kz_json:new()),
    Merged = kz_json:merge_recursive([AccountAttr, EndpointAttr, OwnerAttr]
                                     ,fun(_, V) -> not kz_util:is_empty(V) end
                                    ),
    merge_attributes(Keys, Account, kz_json:set_value(Key, Merged, Endpoint), Owner).

-spec merge_attribute_caller_id(api_object(), api_object(), api_object(), api_object()) -> api_object().
merge_attribute_caller_id(AccountJObj, AccountJAttr, UserJAttr, EndpointJAttr) ->
    Merging =
        case kz_json:is_true(<<"prefer_device_caller_id">>, AccountJObj, 'false') of
            'true' -> [AccountJAttr, UserJAttr, EndpointJAttr];
            'false' -> [AccountJAttr, EndpointJAttr, UserJAttr]
        end,
    kz_json:merge_recursive(Merging, fun(_, V) -> not kz_util:is_empty(V) end).

-spec get_record_call_properties(kz_json:object()) -> kz_json:object().
get_record_call_properties(JObj) ->
    RecordCall = kz_json:get_ne_value(<<"record_call">>, JObj),
    case kz_json:is_json_object(RecordCall) of
        'true' -> RecordCall;
        'false' ->
            case kz_util:is_true(RecordCall) of
                'false' -> kz_json:new();
                'true' ->
                    kz_json:from_list(
                      [{<<"action">>, <<"start">>}
                       ,{<<"record_call">>, 'true'}
                      ]
                     )
            end
    end.

-spec merge_value(ne_binaries(), api_object(), kz_json:object(), api_object()) ->
                              kz_json:object().
merge_value([Key|Keys], Account, Endpoint, Owner) ->
    case kz_json:find(Key, [Owner, Endpoint, Account], 'undefined') of
        'undefined' -> merge_attributes(Keys, Account, Endpoint, Owner);
        Value -> merge_attributes(Keys, Account, kz_json:set_value(Key, Value, Endpoint), Owner)
    end.

-spec caller_id_owner_attr(kz_json:object()) -> kz_json:object().
caller_id_owner_attr(Owner) ->
    OwnerAttr = kz_json:get_ne_value(<<"caller_id">>, Owner, kz_json:new()),
    case kz_json:get_value([<<"internal">>, <<"name">>], OwnerAttr) of
        'undefined' ->
            Name = create_endpoint_name(kz_json:get_ne_value(<<"first_name">>, Owner)
                                        ,kz_json:get_ne_value(<<"last_name">>, Owner)
                                        ,'undefined'
                                        ,'undefined'),
            kz_json:set_value([<<"internal">>, <<"name">>], Name, OwnerAttr);
        _Else -> OwnerAttr
    end.

-spec merge_call_restrictions(ne_binaries(), kz_json:object(), kz_json:object(), kz_json:object()) ->
                                     kz_json:object().
merge_call_restrictions([], _, Endpoint, _) -> Endpoint;
merge_call_restrictions([Key|Keys], Account, Endpoint, Owner) ->
    case kz_json:get_value([<<"call_restriction">>, Key, <<"action">>], Account) =:= <<"deny">>
        orelse kz_json:get_value([<<"call_restriction">>, Key, <<"action">>], Owner)
    of
        'true' ->
            %% denied at the account level
            Update = kz_json:set_value([<<"call_restriction">>, Key, <<"action">>], <<"deny">>, Endpoint),
            merge_call_restrictions(Keys, Account, Update, Owner);
        <<"deny">> ->
            %% denied at the user level
            Update = kz_json:set_value([<<"call_restriction">>, Key, <<"action">>], <<"deny">>, Endpoint),
            merge_call_restrictions(Keys, Account, Update, Owner);
        <<"allow">> ->
            %% allowed at the user level
            Update = kz_json:set_value([<<"call_restriction">>, Key, <<"action">>], <<"allow">>, Endpoint),
            merge_call_restrictions(Keys, Account, Update, Owner);
        _Else ->
            %% user inherit or no user, either way use the device restrictions
            merge_call_restrictions(Keys, Account, Endpoint, Owner)
    end.

-spec get_user(ne_binary(), api_binary() | kz_json:object()) -> kz_json:object().
get_user(_, 'undefined') -> kz_json:new();
get_user(AccountDb, OwnerId) when is_binary(OwnerId) ->
    case kz_datamgr:open_cache_doc(AccountDb, OwnerId) of
        {'ok', JObj} -> JObj;
        {'error', _R} ->
            lager:warning("failed to load endpoint owner ~s: ~p", [OwnerId, _R]),
            kz_json:new()
    end;
get_user(AccountDb, Endpoint) ->
    case kz_json:get_keys([<<"hotdesk">>, <<"users">>], Endpoint) of
        [] ->
            get_user(AccountDb, kz_json:get_value(<<"owner_id">>, Endpoint));
        [OwnerId] ->
            fix_user_restrictions(get_user(AccountDb, OwnerId));
        [_|_]=OwnerIds->
            J = convert_to_single_user(get_users(AccountDb, OwnerIds)),
            fix_user_restrictions(J)
    end.

-spec get_users(ne_binary(), ne_binaries()) -> kz_json:objects().
get_users(AccountDb, OwnerIds) ->
    get_users(AccountDb, OwnerIds, []).

-spec get_users(ne_binary(), ne_binaries(), kz_json:objects()) -> kz_json:objects().
get_users(_, [], Users) ->
    Users;
get_users(AccountDb, [OwnerId|OwnerIds], Users) ->
    case kz_datamgr:open_cache_doc(AccountDb, OwnerId) of
        {'ok', JObj} ->
            get_users(AccountDb, OwnerIds, [JObj|Users]);
        {'error', _R} ->
            lager:warning("failed to load endpoint owner ~s: ~p", [OwnerId, _R]),
            get_users(AccountDb, OwnerIds, Users)
    end.

-spec fix_user_restrictions(kz_json:object()) -> kz_json:object().
fix_user_restrictions(JObj) ->
    lists:foldl(fun(Key, J) ->
                        case kz_json:get_value([<<"call_restriction">>
                                                ,Key
                                                ,<<"action">>
                                               ], J)
                        of
                            <<"deny">> -> J;
                            _Else ->
                                %% this ensures we override the device
                                %% but only when there is a user associated
                                kz_json:set_value([<<"call_restriction">>
                                                   ,Key
                                                   ,<<"action">>
                                                  ], <<"allow">>, J)
                        end
                end, JObj, kz_json:get_keys(knm_converters:available_classifiers())).

-spec convert_to_single_user(kz_json:objects()) -> kz_json:object().
convert_to_single_user(JObjs) ->
    Routines = [fun singlfy_user_attr_keys/2
                ,fun singlfy_user_restrictions/2
               ],
    lists:foldl(fun(F, J) -> F(JObjs, J) end, kz_json:new(), Routines).

-spec singlfy_user_attr_keys(kz_json:objects(), kz_json:object()) -> kz_json:object().
singlfy_user_attr_keys(JObjs, JObj) ->
    Value = lists:foldl(fun(J, V1) ->
                                case kz_json:get_integer_value([?CF_ATTR_LOWER_KEY
                                                                ,?CF_ATTR_UPPER_KEY
                                                               ], J, 5)
                                of
                                    V2 when V2 < V1 -> V2;
                                    _ -> V1
                                end
                        end, 5, JObjs),
    kz_json:set_value([?CF_ATTR_LOWER_KEY, ?CF_ATTR_UPPER_KEY], Value, JObj).

-spec singlfy_user_restrictions(kz_json:objects(), kz_json:object()) -> kz_json:object().
singlfy_user_restrictions(JObjs, JObj) ->
    lists:foldl(fun(Key, J) ->
                        Fun = fun(Elem) -> do_all_restrict(Key, Elem) end,
                        case lists:all(Fun, JObjs) of
                            'false' -> J;
                            'true' ->
                                 kz_json:set_value([<<"call_restriction">>
                                                    ,Key
                                                    ,<<"action">>
                                                   ], <<"deny">>, J)
                        end
                end, JObj, kz_json:get_keys(knm_converters:available_classifiers())).

-spec do_all_restrict(ne_binary(), kz_json:object()) -> boolean().
do_all_restrict(Key, JObj) ->
    kz_json:get_value([<<"call_restriction">>
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
        kz_cache:filter_local(?CACHE_NAME, fun({?MODULE, Db, _Id}, _Value) ->
                                                   Db =:= AccountDb;
                                              (_, _) -> 'false'
                                           end),
    _ = [flush(Db, Id)|| {{?MODULE, Db, Id}, _} <- ToRemove],
    'ok'.

flush(Db, Id) ->
    kz_cache:erase_local(?CACHE_NAME, {?MODULE, Db, Id}),
    {'ok', Rev} = kz_datamgr:lookup_doc_rev(Db, Id),
    Props =
        [{<<"ID">>, Id}
         ,{<<"Database">>, Db}
         ,{<<"Rev">>, Rev}
         ,{<<"Type">>, kz_device:type()}
         | kz_api:default_headers(<<"configuration">>
                                  ,?DOC_EDITED
                                  ,?APP_NAME
                                  ,?APP_VERSION
                                 )
        ],
    Fun = fun(P) ->
                  kapi_conf:publish_doc_update('edited', Db, kz_device:type(), Id, P)
          end,
    kapps_util:amqp_pool_send(Props, Fun).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates one or more kazoo API endpoints for use in a bridge string.
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

-spec build(api_binary() | kz_json:object(), kapps_call:call()) ->
                   {'ok', kz_json:objects()} |
                   {'error', build_errors()}.
-spec build(api_binary() | kz_json:object(), api_object(), kapps_call:call()) ->
                   {'ok', kz_json:objects()} |
                   {'error', build_errors()}.

build(EndpointId, Call) ->
    build(EndpointId, kz_json:new(), Call).

build('undefined', _Properties, _Call) ->
    {'error', 'endpoint_id_undefined'};
build(EndpointId, 'undefined', Call) when is_binary(EndpointId) ->
    build(EndpointId, kz_json:new(), Call);
build(EndpointId, Properties, Call) when is_binary(EndpointId) ->
    case ?MODULE:get(EndpointId, Call) of
        {'ok', Endpoint} -> build(Endpoint, Properties, Call);
        {'error', _}=E -> E
    end;
build(Endpoint, Properties, Call) ->
    Call1 = maybe_rewrite_caller_id(Endpoint, Call),
    case should_create_endpoint(Endpoint, Properties, Call1) of
        'ok' -> create_endpoints(Endpoint, Properties, Call1);
        {'error', _}=E -> E
    end.

-spec maybe_rewrite_caller_id(kz_json:object(), kapps_call:call()) -> kapps_call:call().
maybe_rewrite_caller_id(Endpoint, Call) ->
    case kz_json:get_ne_value(<<"caller_id_options">>, Endpoint) of
        'undefined' -> Call;
        CidOptions  ->
            kapps_call:maybe_format_caller_id(Call, kz_json:get_value(<<"format">>, CidOptions))
    end.

-spec should_create_endpoint(kz_json:object(), kz_json:object(), kapps_call:call()) ->
                                          'ok' | {'error', any()}.
should_create_endpoint(Endpoint, Properties, Call) ->
    Routines = [fun maybe_missing_resource_type/3
                ,fun maybe_owner_called_self/3
                ,fun maybe_endpoint_called_self/3
                ,fun maybe_endpoint_disabled/3
                ,fun maybe_do_not_disturb/3
                ,fun maybe_exclude_from_queues/3
               ],
    should_create_endpoint(Routines, Endpoint, Properties, Call).

-type ep_routine_v() :: fun((kz_json:object(), kz_json:object(), kapps_call:call()) -> 'ok' | _).
-type ep_routines_v() :: [ep_routine_v()].
-spec should_create_endpoint(ep_routines_v(), kz_json:object(), kz_json:object(),  kapps_call:call()) ->
                                          'ok' | {'error', any()}.
should_create_endpoint([], _, _, _) -> 'ok';
should_create_endpoint([Routine|Routines], Endpoint, Properties, Call) when is_function(Routine, 3) ->
    case Routine(Endpoint, Properties, Call) of
        'ok' -> should_create_endpoint(Routines, Endpoint, Properties, Call);
        Else -> Else
    end.

-spec maybe_missing_resource_type(kz_json:object(), kz_json:object(),  kapps_call:call()) ->
                                     'ok' |
                                     {'error', 'no_resource_type'}.
-spec maybe_missing_resource_type(api_binary()) ->
                                     'ok' |
                                     {'error', 'no_resource_type'}.
maybe_missing_resource_type(_, _, Call) ->
    maybe_missing_resource_type(kapps_call:resource_type(Call)).

maybe_missing_resource_type('undefined') ->
    lager:error("kapps_call resource type is undefined"),
    kz_util:log_stacktrace(),
    {'error', 'no_resource_type'};
maybe_missing_resource_type(_) -> 'ok'.

-spec maybe_owner_called_self(kz_json:object(), kz_json:object(),  kapps_call:call()) ->
                                     'ok' |
                                     {'error', 'owner_called_self'}.
maybe_owner_called_self(Endpoint, Properties, Call) ->
    maybe_owner_called_self(Endpoint, Properties, kapps_call:resource_type(Call), Call).

-spec maybe_owner_called_self(kz_json:object(), kz_json:object(), api_binary(), kapps_call:call()) ->
                                     'ok' |
                                     {'error', 'owner_called_self'}.
maybe_owner_called_self(Endpoint, Properties, <<"audio">>, Call) ->
    CanCallSelf = kz_json:is_true(<<"can_call_self">>, Properties),
    EndpointOwnerId = kz_json:get_value(<<"owner_id">>, Endpoint),
    OwnerId = kapps_call:kvs_fetch('owner_id', Call),
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
    AccountId = kapps_call:account_id(Call),
    DefTextSelf = kapps_account_config:get_global(AccountId, ?CF_CONFIG_CAT, <<"default_can_text_self">>, 'true'),
    CanTextSelf = kz_json:is_true(<<"can_text_self">>, Properties, DefTextSelf),
    EndpointOwnerId = kz_json:get_value(<<"owner_id">>, Endpoint),
    OwnerId = kapps_call:owner_id(Call),
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

-spec maybe_endpoint_called_self(kz_json:object(), kz_json:object(),  kapps_call:call()) ->
                                        'ok' |
                                        {'error', 'endpoint_called_self'}.
maybe_endpoint_called_self(Endpoint, Properties, Call) ->
    maybe_endpoint_called_self(Endpoint, Properties, kapps_call:resource_type(Call), Call).

-spec maybe_endpoint_called_self(kz_json:object(), kz_json:object(), api_binary(), kapps_call:call()) ->
                                        'ok' |
                                        {'error', 'endpoint_called_self'}.
maybe_endpoint_called_self(Endpoint, Properties, <<"audio">>, Call) ->
    CanCallSelf = kz_json:is_true(<<"can_call_self">>, Properties),
    AuthorizingId = kapps_call:authorizing_id(Call),
    EndpointId = kz_doc:id(Endpoint),
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
    AccountId = kapps_call:account_id(Call),
    DefTextSelf = kapps_account_config:get_global(AccountId, ?CF_CONFIG_CAT, <<"default_can_text_self">>, 'true'),
    CanTextSelf = kz_json:is_true(<<"can_text_self">>, Properties, DefTextSelf),
    AuthorizingId = kapps_call:authorizing_id(Call),
    EndpointId = kz_doc:id(Endpoint),
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

-spec maybe_endpoint_disabled(kz_json:object(), kz_json:object(), kapps_call:call()) ->
                                           'ok' |
                                           {'error', 'endpoint_disabled'}.
maybe_endpoint_disabled(Endpoint, _, _) ->
    case kz_json:is_false(<<"enabled">>, Endpoint) of
        'false' -> 'ok';
        'true' ->
            lager:info("endpoint ~s is disabled", [kz_doc:id(Endpoint)]),
            {'error', 'endpoint_disabled'}
    end.

-spec maybe_do_not_disturb(kz_json:object(), kz_json:object(),  kapps_call:call()) ->
                                        'ok' |
                                        {'error', 'do_not_disturb'}.
maybe_do_not_disturb(Endpoint, _, _) ->
    DND = kz_json:get_ne_value(<<"do_not_disturb">>, Endpoint, kz_json:new()),
    case kz_json:is_true(<<"enabled">>, DND) of
        'false' -> 'ok';
        'true' ->
            lager:info("do not distrub endpoint ~s", [kz_doc:id(Endpoint)]),
            {'error', 'do_not_disturb'}
    end.

-spec maybe_exclude_from_queues(kz_json:object(), kz_json:object(), kapps_call:call()) ->
                                        'ok' |
                                        {'error', 'exclude_from_queues'}.
maybe_exclude_from_queues(Endpoint, _, Call) ->
    case is_binary(kapps_call:custom_channel_var(<<"Queue-ID">>, Call))
        andalso kz_json:is_true(<<"exclude_from_queues">>, Endpoint)
    of
        'false' -> 'ok';
        'true' -> {'error', 'exclude_from_queues'}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% creates the actual endpoint json objects for use in the kazoo
%% bridge API.
%% @end
%%--------------------------------------------------------------------
-spec create_endpoints(kz_json:object(), kz_json:object(), kapps_call:call()) ->
                              {'ok', kz_json:objects()} |
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
            maybe_start_metaflows(Call, Endpoints),
            {'ok', Endpoints}
    end.

maybe_start_metaflows(Call, Endpoints) ->
    maybe_start_metaflows(kapps_call:call_id_direct(Call), Call, Endpoints).

maybe_start_metaflows('undefined', _Call, _Endpoints) -> 'ok';
maybe_start_metaflows(_, Call, Endpoints) ->
    cf_util:maybe_start_metaflows(Call, Endpoints).

-type ep_routine() :: fun((kz_json:object(), kz_json:object(), kapps_call:call()) ->
                                 {'error', _} | kz_json:object()).
-spec try_create_endpoint(ep_routine(), kz_json:objects(), kz_json:object(), kz_json:object(), kapps_call:call()) ->
                                       kz_json:objects().
try_create_endpoint(Routine, Endpoints, Endpoint, Properties, Call) when is_function(Routine, 3) ->
    try Routine(Endpoint, Properties, Call) of
        {'error', _R} ->
            lager:warning("failed to create endpoint: ~p", [_R]),
            Endpoints;
        JObj -> [JObj|Endpoints]
    catch
        _E:_R ->
            lager:warning("unable to build endpoint(~s): ~p", [_E, _R]),
            kz_util:log_stacktrace(),
            Endpoints
    end.

-spec maybe_create_fwd_endpoint(kz_json:object(), kz_json:object(), kapps_call:call()) ->
                                       kz_json:object() |
                                       {'error', 'call_forward_not_appropriate'}.
maybe_create_fwd_endpoint(Endpoint, Properties, Call) ->
    case is_call_forward_enabled(Endpoint, Properties) of
        'false' -> {'error', 'call_forward_not_appropriate'};
        'true' ->
            lager:info("creating call forwarding endpoint"),
            create_call_fwd_endpoint(Endpoint, Properties, Call)
    end.

-spec maybe_create_endpoint(kz_json:object(), kz_json:object(), kapps_call:call()) ->
                                   kz_json:object() |
                                   {'error', 'call_forward_substitute'}.
maybe_create_endpoint(Endpoint, Properties, Call) ->
    case is_call_forward_enabled(Endpoint, Properties)
        andalso kz_json:is_true([<<"call_forward">>, <<"substitute">>], Endpoint)
    of
        'true' -> {'error', 'call_forward_substitute'};
        'false' ->
            EndpointType = get_endpoint_type(Endpoint),
            maybe_create_endpoint(EndpointType, Endpoint, Properties, Call)
    end.

-spec is_call_forward_enabled(kz_json:object(), kz_json:object()) -> boolean().
is_call_forward_enabled(Endpoint, Properties) ->
    CallForwarding = kz_json:get_ne_value(<<"call_forward">>, Endpoint, kz_json:new()),
    Source = kz_json:get_value(<<"source">>, Properties),
    Number = kz_json:get_value(<<"number">>, CallForwarding),
    kz_json:is_true(<<"enabled">>, CallForwarding)
        andalso not kz_util:is_empty(Number)
        andalso (kz_json:is_false(<<"direct_calls_only">>, CallForwarding, 'true')
                 orelse
                   (not lists:member(Source, ?NON_DIRECT_MODULES))).

-spec maybe_create_endpoint(ne_binary(), kz_json:object(), kz_json:object(), kapps_call:call()) ->
                                   kz_json:object() | {'error', ne_binary()}.
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
    {'error', <<"unknown endpoint type ", (kz_util:to_binary(UnknownType))/binary>>}.

-spec maybe_create_mobile_endpoint(kz_json:object(), kz_json:object(), kapps_call:call()) ->
                                          kz_json:object() |
                                          {'error', ne_binary()}.
maybe_create_mobile_endpoint(Endpoint, Properties, Call) ->
    case kapps_config:get_is_true(?CF_MOBILE_CONFIG_CAT, <<"create_sip_endpoint">>, 'false') of
        'true' ->
            lager:info("building mobile as SIP endpoint"),
            create_sip_endpoint(Endpoint, Properties, Call);
        'false' ->
            create_mobile_endpoint(Endpoint, Properties, Call)
    end.

-spec get_endpoint_type(kz_json:object()) -> ne_binary().
get_endpoint_type(Endpoint) ->
    Type = kz_json:get_first_defined([<<"endpoint_type">>
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

-spec maybe_guess_endpoint_type(kz_json:object()) -> ne_binary().
maybe_guess_endpoint_type(Endpoint) ->
    case kapps_config:get_is_true(?CF_CONFIG_CAT, <<"restrict_to_known_types">>, 'false') of
        'false' -> guess_endpoint_type(Endpoint);
        'true' ->
            lager:info("unknown endpoint type and callflows restrictued to known types", []),
            <<"unknown">>
    end.

-spec guess_endpoint_type(kz_json:object()) -> ne_binary().
guess_endpoint_type(Endpoint) ->
    guess_endpoint_type(Endpoint, [<<"mobile">>
                                   ,<<"sip">>
                                   ,<<"skype">>
                                  ]).
-spec guess_endpoint_type(kz_json:object(), ne_binaries()) -> ne_binary().
guess_endpoint_type(Endpoint, [Type|Types]) ->
    case kz_json:get_ne_value(Type, Endpoint) of
        'undefined' -> guess_endpoint_type(Endpoint, Types);
        _ -> Type
    end;
guess_endpoint_type(Endpoint, []) ->
    case kz_json:get_ne_value(<<"sip">>, Endpoint) of
        'undefined' -> <<"unknown">>;
        _Else -> <<"sip">>
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates the kazoo API endpoint for a bridge call command. This
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

-spec get_clid(kz_json:object(), kz_json:object(), kapps_call:call()) -> clid().
get_clid(Endpoint, Properties, Call) ->
    get_clid(Endpoint, Properties, Call, <<"internal">>).

get_clid(Endpoint, Properties, Call, Type) ->
    case kz_json:is_true(<<"suppress_clid">>, Properties) of
        'true' -> #clid{};
        'false' ->
            {Number, Name} = cf_attributes:caller_id(Type, Call),
            CallerNumber = case kapps_call:caller_id_number(Call) of
                               Number -> 'undefined';
                               _Number -> Number
                           end,
            CallerName = case kapps_call:caller_id_name(Call) of
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

-spec maybe_record_call(kz_json:object(), kapps_call:call()) -> 'ok'.
maybe_record_call(Endpoint, Call) ->
    case is_sms(Call)
        orelse kapps_call:call_id_direct(Call) =:= 'undefined'
    of
        'true' -> 'ok';
        'false' ->
            Data = kz_json:get_value(<<"record_call">>, Endpoint, kz_json:new()),
            _ = cf_util:maybe_start_call_recording(Data, Call),
            'ok'
    end.

-spec create_sip_endpoint(kz_json:object(), kz_json:object(), kapps_call:call()) ->
                                 kz_json:object().
create_sip_endpoint(Endpoint, Properties, Call) ->
    Clid = get_clid(Endpoint, Properties, Call),
    create_sip_endpoint(Endpoint, Properties, Clid, Call).

-spec create_sip_endpoint(kz_json:object(), kz_json:object(), clid(), kapps_call:call()) ->
                                 kz_json:object().
create_sip_endpoint(Endpoint, Properties, #clid{}=Clid, Call) ->
    SIPJObj = kz_json:get_value(<<"sip">>, Endpoint),
    _ = maybe_record_call(Endpoint, Call),
    kz_json:from_list(
      props:filter_empty(
        [{<<"Invite-Format">>, get_invite_format(SIPJObj)}
         ,{<<"To-User">>, get_to_user(SIPJObj, Properties)}
         ,{<<"To-Username">>, get_to_username(SIPJObj)}
         ,{<<"To-Realm">>, cf_util:get_sip_realm(Endpoint, kapps_call:account_id(Call))}
         ,{<<"To-DID">>, get_to_did(Endpoint, Call)}
         ,{<<"To-IP">>, kz_json:get_value(<<"ip">>, SIPJObj)}
         ,{<<"SIP-Transport">>, get_sip_transport(SIPJObj)}
         ,{<<"SIP-Interface">>, get_custom_sip_interface(SIPJObj)}
         ,{<<"Route">>, kz_json:get_value(<<"route">>, SIPJObj)}
         ,{<<"Proxy-IP">>, kz_json:get_value(<<"proxy">>, SIPJObj)}
         ,{<<"Forward-IP">>, kz_json:get_value(<<"forward">>, SIPJObj)}
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
         ,{<<"Endpoint-ID">>, kz_doc:id(Endpoint)}
         ,{<<"Codecs">>, get_codecs(Endpoint)}
         ,{<<"Hold-Media">>, cf_attributes:moh_attributes(Endpoint, <<"media_id">>, Call)}
         ,{<<"Presence-ID">>, cf_attributes:presence_id(Endpoint, Call)}
         ,{<<"Custom-SIP-Headers">>, generate_sip_headers(Endpoint, Call)}
         ,{<<"Custom-Channel-Vars">>, generate_ccvs(Endpoint, Call)}
         ,{<<"Flags">>, get_outbound_flags(Endpoint)}
         ,{<<"Ignore-Completed-Elsewhere">>, get_ignore_completed_elsewhere(Endpoint)}
         ,{<<"Failover">>, maybe_build_failover(Endpoint, Clid, Call)}
         ,{<<"Metaflows">>, kz_json:get_value(<<"metaflows">>, Endpoint)}
         | maybe_get_t38(Endpoint, Call)
        ])).

-spec maybe_get_t38(kz_json:object(), kapps_call:call()) -> kz_proplist().
maybe_get_t38(Endpoint, Call) ->
    Opt =
        case ?MODULE:get(Call) of
            {'ok', JObj} -> kz_json:is_true([<<"media">>, <<"fax_option">>], JObj);
            {'error', _} -> 'undefined'
        end,
    DeviceType = kz_json:get_value(<<"device_type">>, Endpoint),
    case DeviceType =:= <<"fax">> of
        'false' -> [];
        'true' ->
            kapps_call_command:get_inbound_t38_settings(
              Opt
              ,kz_json:get_value(<<"Fax-T38-Enabled">>, Endpoint)
             )
    end.

-spec maybe_build_failover(kz_json:object(), clid(), kapps_call:call()) -> api_object().
maybe_build_failover(Endpoint, Clid, Call) ->
    CallForward = kz_json:get_value(<<"call_forward">>, Endpoint),
    Number = kz_json:get_value(<<"number">>, CallForward),
    case kz_json:is_true(<<"failover">>, CallForward)
        andalso not kz_util:is_empty(Number)
    of
        'false' -> maybe_build_push_failover(Endpoint, Clid, Call);
        'true' -> create_call_fwd_endpoint(Endpoint, kz_json:new(), Call)
    end.

-spec maybe_build_push_failover(kz_json:object(), clid(), kapps_call:call()) -> api_object().
maybe_build_push_failover(Endpoint, Clid, Call) ->
    case kz_json:get_value(<<"push">>, Endpoint) of
        'undefined' -> 'undefined';
        PushJObj -> build_push_failover(Endpoint, Clid, PushJObj, Call)
    end.

-spec build_push_failover(kz_json:object(), clid(), kz_json:object(), kapps_call:call()) -> api_object().
build_push_failover(Endpoint, Clid, PushJObj, Call) ->
    lager:debug("building push failover"),
    SIPJObj = kz_json:get_value(<<"sip">>, Endpoint),
    ToUsername = get_to_username(SIPJObj),
    ToRealm = cf_util:get_sip_realm(Endpoint, kapps_call:account_id(Call)),
    ToUser = <<ToUsername/binary, "@", ToRealm/binary>>,
    Proxy = kz_json:get_value(<<"Token-Proxy">>, PushJObj),
    PushHeaders = kz_json:foldl(fun(K, V, Acc) ->
                                        kz_json:set_value(<<"X-KAZOO-PUSHER-", K/binary>>, V, Acc)
                                end, kz_json:new(), PushJObj),
    kz_json:from_list(
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
         ,{<<"Endpoint-ID">>, kz_doc:id(Endpoint)}
         ,{<<"Codecs">>, get_codecs(Endpoint)}
         ,{<<"Hold-Media">>, cf_attributes:moh_attributes(Endpoint, <<"media_id">>, Call)}
         ,{<<"Presence-ID">>, cf_attributes:presence_id(Endpoint, Call)}
         ,{<<"Custom-SIP-Headers">>, generate_sip_headers(Endpoint, PushHeaders, Call)}
         ,{<<"Custom-Channel-Vars">>, generate_ccvs(Endpoint, Call)}
         ,{<<"Flags">>, get_outbound_flags(Endpoint)}
         ,{<<"Ignore-Completed-Elsewhere">>, get_ignore_completed_elsewhere(Endpoint)}
         ,{<<"Metaflows">>, kz_json:get_value(<<"metaflows">>, Endpoint)}
        ])).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_sip_transport(kz_json:object()) -> api_binary().
get_sip_transport(SIPJObj) ->
    case validate_sip_transport(kz_json:get_value(<<"transport">>, SIPJObj)) of
        'undefined' ->
            validate_sip_transport(kapps_config:get(?CF_CONFIG_CAT, <<"sip_transport">>));
        Transport -> Transport
    end.

-spec validate_sip_transport(any()) -> api_binary().
validate_sip_transport(<<"tcp">>) -> <<"tcp">>;
validate_sip_transport(<<"udp">>) -> <<"udp">>;
validate_sip_transport(<<"tls">>) -> <<"tls">>;
validate_sip_transport(<<"sctp">>) -> <<"sctp">>;
validate_sip_transport(_) -> 'undefined'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_custom_sip_interface(kz_json:object()) -> api_binary().
get_custom_sip_interface(JObj) ->
    case kz_json:get_value(<<"custom_sip_interface">>, JObj) of
        'undefined' ->
            kapps_config:get(?CF_CONFIG_CAT, <<"custom_sip_interface">>);
        Else -> Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates the kazoo API endpoint for a bridge call command. This
%% endpoint is comprised of the endpoint definition (commonally a
%% device) and the properties of this endpoint in the callflow.
%% @end
%%--------------------------------------------------------------------
-spec create_skype_endpoint(kz_json:object(), kz_json:object(), kapps_call:call()) ->
                                         kz_json:object().
create_skype_endpoint(Endpoint, Properties, _Call) ->
    SkypeJObj = kz_json:get_value(<<"skype">>, Endpoint),
    Prop =
        [{<<"Invite-Format">>, <<"username">>}
         ,{<<"To-User">>, get_to_user(SkypeJObj, Properties)}
         ,{<<"To-Username">>, get_to_username(SkypeJObj)}
         ,{<<"Endpoint-Type">>, <<"skype">>}
         ,{<<"Endpoint-Options">>, kz_json:from_list([{<<"Skype-RR">>, <<"true">>}])}
        ],
    kz_json:from_list(props:filter_undefined(Prop)).

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
-spec create_call_fwd_endpoint(kz_json:object(), kz_json:object(), kapps_call:call()) ->
                                      kz_json:object().
create_call_fwd_endpoint(Endpoint, Properties, Call) ->
    CallForward = kz_json:get_ne_value(<<"call_forward">>, Endpoint, kz_json:new()),
    lager:info("call forwarding endpoint to ~s", [kz_json:get_value(<<"number">>, CallForward)]),
    IgnoreEarlyMedia = case kz_json:is_true(<<"require_keypress">>, CallForward)
                           orelse not kz_json:is_true(<<"substitute">>, CallForward)
                       of
                           'true' -> <<"true">>;
                           'false' -> kz_json:get_binary_boolean(<<"ignore_early_media">>, CallForward)
                       end,
    Clid = case kapps_call:inception(Call) of
               'undefined' -> get_clid(Endpoint, Properties, Call, <<"external">>);
               _Else -> #clid{}
           end,
    Prop = [{<<"Invite-Format">>, <<"route">>}
            ,{<<"To-DID">>, kz_json:get_value(<<"number">>, Endpoint, kapps_call:request_user(Call))}
            ,{<<"Route">>, <<"loopback/", (kz_json:get_value(<<"number">>, CallForward, <<"unknown">>))/binary>>}
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
    kz_json:from_list(props:filter_undefined(Prop)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec create_mobile_endpoint(kz_json:object(), kz_json:object(), kapps_call:call()) ->
                                    kz_json:object() |
                                    {'error', ne_binary()}.
create_mobile_endpoint(Endpoint, Properties, Call) ->
    case kapps_call:resource_type(Call) of
        ?RESOURCE_TYPE_SMS -> create_mobile_sms_endpoint(Endpoint, Properties, Call);
        _Other -> create_mobile_audio_endpoint(Endpoint, Properties, Call)
    end.

-spec create_mobile_audio_endpoint(kz_json:object(), kz_json:object(), kapps_call:call()) ->
                                          kz_json:object() |
                                          {'error', ne_binary()}.
create_mobile_audio_endpoint(Endpoint, Properties, Call) ->
    case maybe_build_mobile_route(Endpoint) of
        {'error', _R}=Error ->
            lager:info("unable to build mobile endpoint: ~s", [_R]),
            Error;
        Route ->
            Codecs = kapps_config:get(?CF_MOBILE_CONFIG_CAT, <<"codecs">>, ?DEFAULT_MOBILE_CODECS),
            SIPInterface = kapps_config:get_binary(?CF_MOBILE_CONFIG_CAT, <<"custom_sip_interface">>),
            Prop = [{<<"Invite-Format">>, <<"route">>}
                    ,{<<"Ignore-Early-Media">>, <<"true">>}
                    ,{<<"Route">>, Route}
                    ,{<<"Ignore-Early-Media">>, <<"true">>}
                    ,{<<"Endpoint-Timeout">>, get_timeout(Properties)}
                    ,{<<"Endpoint-Delay">>, get_delay(Properties)}
                    ,{<<"Presence-ID">>, cf_attributes:presence_id(Endpoint, Call)}
                    ,{<<"Custom-SIP-Headers">>, generate_sip_headers(Endpoint, Call)}
                    ,{<<"Codecs">>, Codecs}
                    ,{<<"Custom-Channel-Vars">>, generate_ccvs(Endpoint, Call, kz_json:new())}
                    ,{<<"SIP-Interface">>, SIPInterface}
                    ,{<<"Bypass-Media">>, get_bypass_media(Endpoint)}
                   ],
            kz_json:from_list(props:filter_undefined(Prop))
    end.

-spec maybe_build_mobile_route(kz_json:object()) ->
                                      ne_binary() |
                                      {'error', 'mdn_missing'}.
maybe_build_mobile_route(Endpoint) ->
    case kz_json:get_ne_value([<<"mobile">>, <<"mdn">>], Endpoint) of
        'undefined' ->
            lager:info("unable to build mobile endpoint, MDN missing", []),
            {'error', 'mdn_missing'};
        MDN -> build_mobile_route(MDN)
    end.

-spec build_mobile_route(ne_binary()) ->
                                ne_binary() |
                                {'error', 'invalid_mdn'}.
build_mobile_route(MDN) ->
    Regex = kapps_config:get_binary(?CF_MOBILE_CONFIG_CAT, <<"formatter">>, ?DEFAULT_MOBILE_FORMATER),
    case re:run(MDN, Regex, [{'capture', 'all', 'binary'}]) of
        'nomatch' ->
            lager:info("unable to build mobile endpoint, invalid MDN ~s", [MDN]),
            {'error', 'invalid_mdn'};
        {'match', Captures} ->
            Root = lists:last(Captures),
            Prefix = kapps_config:get_binary(?CF_MOBILE_CONFIG_CAT, <<"prefix">>, ?DEFAULT_MOBILE_PREFIX),
            Suffix = kapps_config:get_binary(?CF_MOBILE_CONFIG_CAT, <<"suffix">>, ?DEFAULT_MOBILE_SUFFIX),
            Realm = kapps_config:get_binary(?CF_MOBILE_CONFIG_CAT, <<"realm">>, ?DEFAULT_MOBILE_REALM),
            Route = <<"sip:"
                      ,Prefix/binary, Root/binary, Suffix/binary
                      ,"@", Realm/binary
                    >>,
            maybe_add_mobile_path(Route)
    end.

-spec maybe_add_mobile_path(ne_binary()) -> ne_binary().
maybe_add_mobile_path(Route) ->
    Path = kapps_config:get_binary(?CF_MOBILE_CONFIG_CAT, <<"path">>, ?DEFAULT_MOBILE_PATH),
    case kz_util:is_empty(Path) of
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
-spec generate_sip_headers(kz_json:object(), kapps_call:call()) ->
                                  kz_json:object().
generate_sip_headers(Endpoint, Call) ->
    generate_sip_headers(Endpoint, kz_json:new(), Call).

-spec generate_sip_headers(kz_json:object(), kz_json:object(), kapps_call:call()) ->
                                  kz_json:object().
generate_sip_headers(Endpoint, Acc, Call) ->
    Inception = kapps_call:inception(Call),

    HeaderFuns = [fun(J) -> maybe_add_sip_headers(J, Endpoint, Call) end
                  ,fun(J) -> maybe_add_alert_info(J, Endpoint, Inception) end
                  ,fun(J) -> maybe_add_aor(J, Endpoint, Call) end
                  ,fun(J) -> maybe_add_diversion(J, Endpoint, Inception, Call) end
                 ],
    lists:foldr(fun(F, JObj) -> F(JObj) end, Acc, HeaderFuns).

-spec maybe_add_diversion(kz_json:object(), kz_json:object(), api_binary(), kapps_call:call()) -> kz_json:object().
maybe_add_diversion(JObj, Endpoint, _Inception, Call) ->
    ShouldAddDiversion = kapps_call:authorizing_id(Call) =:= 'undefined'
                         andalso kz_json:is_true([<<"call_forward">>, <<"keep_caller_id">>], Endpoint, 'false')
                         andalso kapps_config:get_is_true(?CF_CONFIG_CAT, <<"should_add_diversion_header">>, 'false'),
    case ShouldAddDiversion of
        'true' ->
            Diversion = list_to_binary(["<sip:", kapps_call:request(Call), ">", ";reason=unconditional"]),
            lager:debug("add diversion as ~s", [Diversion]),
            Diversions = kz_json:get_list_value(<<"Diversions">>, JObj, []),
            kz_json:set_value(<<"Diversions">>, [Diversion | Diversions], JObj);
        'false' -> JObj
    end.

-spec maybe_add_sip_headers(kz_json:object(), kz_json:object(), kapps_call:call()) -> kz_json:object().
maybe_add_sip_headers(JObj, Endpoint, Call) ->
    case ?MODULE:get(Call) of
        {'ok', AuthorizingEndpoint} ->
            MergeFuns = [fun() -> kz_device:custom_sip_headers_inbound(Endpoint) end
                         ,fun() -> kz_device:custom_sip_headers_outbound(AuthorizingEndpoint) end
                        ],
            Fun = fun(Routine, Acc) ->
                      merge_custom_sip_headers(Routine(), Acc)
                  end,
            lists:foldl(Fun, JObj, MergeFuns);
        {'error', _} -> JObj
    end.

-spec merge_custom_sip_headers(kz_json:object(), kz_json:object()) -> kz_json:object().
merge_custom_sip_headers('undefined', JObj) ->
    JObj;
merge_custom_sip_headers(CustomHeaders, JObj) ->
    kz_json:merge_jobjs(CustomHeaders, JObj).

-spec maybe_add_alert_info(kz_json:object(), kz_json:object(), api_binary()) -> kz_json:object().
maybe_add_alert_info(JObj, Endpoint, 'undefined') ->
    case kz_json:get_value([<<"ringtones">>, <<"internal">>], Endpoint) of
        'undefined' -> JObj;
        Ringtone -> kz_json:set_value(<<"Alert-Info">>, Ringtone, JObj)
    end;
maybe_add_alert_info(JObj, Endpoint, _Inception) ->
    case kz_json:get_value([<<"ringtones">>, <<"external">>], Endpoint) of
        'undefined' -> JObj;
        Ringtone -> kz_json:set_value(<<"Alert-Info">>, Ringtone, JObj)
    end.

-spec maybe_add_aor(kz_json:object(), kz_json:object(), kapps_call:call()) -> kz_json:object().
-spec maybe_add_aor(kz_json:object(), kz_json:object(), api_binary(), ne_binary()) -> kz_json:object().
maybe_add_aor(JObj, Endpoint, Call) ->
    Realm = kz_device:sip_realm(Endpoint, kapps_call:account_realm(Call)),
    Username = kz_device:sip_username(Endpoint),
    maybe_add_aor(JObj, Endpoint, Username, Realm).

maybe_add_aor(JObj, _, 'undefined', _Realm) -> JObj;
maybe_add_aor(JObj, _, Username, Realm) ->
    kz_json:set_value(<<"X-KAZOO-AOR">>, <<"sip:", Username/binary, "@", Realm/binary>> , JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will return the custom channel vars that should be
%% set for this endpoint depending on its settings, and the current
%% call.
%% @end
%%--------------------------------------------------------------------
-spec generate_ccvs(kz_json:object(), kapps_call:call()) -> kz_json:object().
-spec generate_ccvs(kz_json:object(), kapps_call:call(), api_object()) -> kz_json:object().

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
               ,fun maybe_auto_answer/1
              ],
    Acc0 = {Endpoint, Call, CallFwd, kz_json:new()},
    {_Endpoint, _Call, _CallFwd, JObj} = lists:foldr(fun(F, Acc) -> F(Acc) end, Acc0, CCVFuns),
    JObj.

-type ccv_acc() :: {kz_json:object(), kapps_call:call(), api_object(), kz_json:object()}.

-spec maybe_retain_caller_id(ccv_acc()) -> ccv_acc().
maybe_retain_caller_id({_Endpoint, _Call, 'undefined', _JObj}=Acc) ->
    Acc;
maybe_retain_caller_id({Endpoint, Call, CallFwd, JObj}) ->
    {Endpoint, Call, CallFwd
    ,case kapps_call:inception(Call) =:= 'undefined'
         orelse kz_json:is_true(<<"keep_caller_id">>, CallFwd)
     of
         'true' ->
             lager:info("call forwarding will keep set retain caller id"),
             kz_json:set_value(<<"Retain-CID">>, <<"true">>, JObj);
         'false' -> JObj
     end
    }.

-spec maybe_set_endpoint_id(ccv_acc()) -> ccv_acc().
maybe_set_endpoint_id({Endpoint, Call, CallFwd, JObj}) ->
    {Endpoint, Call, CallFwd
     ,case kz_doc:id(Endpoint) of
          'undefined' -> JObj;
          EndpointId ->
              kz_json:set_value(<<"Authorizing-ID">>, EndpointId, JObj)
      end
    }.

-spec maybe_set_owner_id(ccv_acc()) -> ccv_acc().
maybe_set_owner_id({Endpoint, Call, CallFwd, JObj}) ->
    {Endpoint, Call, CallFwd
     ,case kz_json:get_value(<<"owner_id">>, Endpoint) of
          'undefined' -> JObj;
          OwnerId -> kz_json:set_value(<<"Owner-ID">>, OwnerId, JObj)
      end
    }.

-spec maybe_set_account_id(ccv_acc()) -> ccv_acc().
maybe_set_account_id({Endpoint, Call, CallFwd, JObj}) ->
    AccountId = kz_doc:account_id(Endpoint, kapps_call:account_id(Call)),
    {Endpoint, Call, CallFwd
     ,kz_json:set_value(<<"Account-ID">>, AccountId, JObj)
    }.

-spec maybe_set_call_forward(ccv_acc()) -> ccv_acc().
maybe_set_call_forward({_Endpoint, _Call, 'undefined', _JObj}=Acc) ->
    Acc;
maybe_set_call_forward({Endpoint, Call, CallFwd, JObj}) ->
    {Endpoint, Call, CallFwd
     ,kz_json:set_values([{<<"Call-Forward">>, <<"true">>}
                          ,{<<"Authorizing-Type">>, <<"device">>}
                          | bowout_settings(kapps_call:call_id_direct(Call) =:= 'undefined')
                         ], JObj)
    }.

-spec bowout_settings(boolean()) -> kz_proplist().
bowout_settings('true') ->
     [{<<"Simplify-Loopback">>, <<"true">>}
      ,{<<"Loopback-Bowout">>, <<"true">>}
     ];
bowout_settings('false') ->
     [{<<"Simplify-Loopback">>, <<"false">>}
      ,{<<"Loopback-Bowout">>, <<"true">>}
     ].

-spec maybe_auto_answer(ccv_acc()) -> ccv_acc().
maybe_auto_answer({Endpoint, Call, CallFwd, JObj}=Acc) ->
    case kapps_call:custom_channel_var(<<"Auto-Answer-Loopback">>, Call) of
        'undefined' -> Acc;
        AutoAnswer ->
            {Endpoint, Call, CallFwd, kz_json:set_value(<<"Auto-Answer">>, AutoAnswer, JObj)}
    end.

-spec maybe_set_confirm_properties(ccv_acc()) -> ccv_acc().
maybe_set_confirm_properties({Endpoint, Call, CallFwd, JObj}=Acc) ->
    case kz_json:is_true(<<"require_keypress">>, CallFwd) of
        'false' -> Acc;
        'true' ->
            lager:info("call forwarding configured to require key press"),
            Confirm = [{<<"Confirm-Key">>, <<"1">>}
                       ,{<<"Confirm-Cancel-Timeout">>, <<"2">>}
                       ,{<<"Confirm-File">>, ?CONFIRM_FILE(Call)}
                       ,{<<"Require-Ignore-Early-Media">>, <<"true">>}
                      ],
            {Endpoint, Call, CallFwd
             ,kz_json:merge_jobjs(kz_json:from_list(Confirm), JObj)
            }
    end.

-spec maybe_enable_fax(ccv_acc()) -> ccv_acc().
maybe_enable_fax({Endpoint, Call, CallFwd, JObj}=Acc) ->
    case kz_json:get_value([<<"media">>, <<"fax_option">>], Endpoint) of
        <<"auto">> ->
            {Endpoint, Call, CallFwd
             ,kz_json:set_value(<<"Fax-Enabled">>, <<"true">>, JObj)
            };
        _Else -> Acc
    end.

-spec maybe_enforce_security(ccv_acc()) -> ccv_acc().
maybe_enforce_security({Endpoint, Call, CallFwd, JObj}) ->
    EnforceSecurity = kz_json:is_true([<<"media">>, <<"encryption">>, <<"enforce_security">>], Endpoint, 'true'),
    {Endpoint, Call, CallFwd
     ,kz_json:set_value(<<"Media-Encryption-Enforce-Security">>, EnforceSecurity, JObj)
    }.

-spec maybe_set_encryption_flags(ccv_acc()) -> ccv_acc().
maybe_set_encryption_flags({Endpoint, Call, CallFwd, JObj}) ->
    {Endpoint, Call, CallFwd
     ,cf_util:encryption_method_map(JObj, Endpoint)
    }.

-spec set_sip_invite_domain(ccv_acc()) -> ccv_acc().
set_sip_invite_domain({Endpoint, Call, CallFwd, JObj}) ->
    {Endpoint, Call, CallFwd
     ,kz_json:set_value(<<"SIP-Invite-Domain">>, kapps_call:request_realm(Call), JObj)
    }.

-spec maybe_set_call_waiting(ccv_acc()) -> ccv_acc().
maybe_set_call_waiting({Endpoint, Call, CallFwd, JObj}) ->
    NewJobj = case kz_json:is_true([<<"call_waiting">>, <<"enabled">>], Endpoint, 'true') of
                  'true' -> JObj;
                  'false' -> kz_json:set_value(<<"Call-Waiting-Disabled">>, 'true', JObj)
              end,
    {Endpoint, Call, CallFwd, NewJobj}.

-spec get_invite_format(kz_json:object()) -> ne_binary().
get_invite_format(SIPJObj) ->
    kz_json:get_value(<<"invite_format">>, SIPJObj, <<"username">>).

-spec get_to_did(kz_json:object(), kapps_call:call()) -> api_binary().
get_to_did(Endpoint, Call) ->
    kz_json:get_value([<<"sip">>, <<"number">>]
                      ,Endpoint
                      ,kapps_call:request_user(Call)
                     ).

-spec get_to_user(kz_json:object(), kz_json:object()) -> api_binary().
get_to_user(SIPJObj, Properties) ->
    case kz_json:get_ne_value(<<"static_invite">>, Properties) of
        'undefined' ->
            case kz_json:get_ne_value(<<"static_invite">>, SIPJObj) of
                'undefined' -> kz_json:get_value(<<"username">>, SIPJObj);
                To -> To
            end;
        To -> To
    end.

-spec get_to_username(kz_json:object()) -> api_binary().
get_to_username(SIPJObj) ->
    kz_json:get_value(<<"username">>, SIPJObj).

-spec get_timeout(kz_json:object()) -> api_binary().
get_timeout(JObj) ->
    case kz_json:get_integer_value(<<"timeout">>, JObj, 0) of
        Timeout when Timeout > 0 -> kz_util:to_binary(Timeout);
        _Else -> 'undefined'
    end.

-spec get_delay(kz_json:object()) -> api_binary().
get_delay(JObj) ->
    case kz_json:get_integer_value(<<"delay">>, JObj, 0) of
        Delay when Delay > 0 -> kz_util:to_binary(Delay);
        _Else -> 'undefined'
    end.

-spec get_outbound_flags(kz_json:object()) -> api_binary().
get_outbound_flags(JObj) ->
    kz_json:get_ne_value(<<"outbound_flags">>, JObj).

-spec get_progress_timeout(kz_json:object()) -> api_binary().
get_progress_timeout(JObj) ->
    case kz_json:get_integer_value([<<"media">>, <<"progress_timeout">>], JObj, 0) of
        Timeout when Timeout > 0 -> kz_util:to_binary(Timeout);
        _Else -> 'undefined'
    end.

-spec get_ignore_early_media(kz_json:object()) -> api_binary().
get_ignore_early_media(JObj) ->
    case kz_json:is_true([<<"media">>, <<"ignore_early_media">>], JObj) of
        'true' -> <<"true">>;
        'false' -> 'undefined'
    end.

-spec get_bypass_media(kz_json:object()) -> api_binary().
get_bypass_media(JObj) ->
    case kz_json:is_true([<<"media">>, <<"peer_to_peer">>], JObj) of
        'true' -> <<"true">>;
        'false' -> 'undefined'
    end.

-spec get_codecs(kz_json:object()) -> 'undefined' | ne_binaries().
get_codecs(JObj) ->
    case kz_json:get_value([<<"media">>, <<"audio">>, <<"codecs">>], JObj, [])
        ++ kz_json:get_value([<<"media">>, <<"video">>, <<"codecs">>], JObj, [])
    of
        [] -> 'undefined';
        Codecs -> Codecs
    end.

-spec get_ignore_completed_elsewhere(kz_json:object()) -> boolean().
get_ignore_completed_elsewhere(JObj) ->
    case kz_json:get_first_defined([[<<"caller_id_options">>, <<"ignore_completed_elsewhere">>]
                                    ,[<<"sip">>, <<"ignore_completed_elsewhere">>]
                                    ,<<"ignore_completed_elsewhere">>
                                   ], JObj)
    of
        'undefined' -> kapps_config:get_is_true(?CF_CONFIG_CAT, <<"default_ignore_completed_elsewhere">>, 'true');
        IgnoreCompletedElsewhere -> kz_util:is_true(IgnoreCompletedElsewhere)
    end.

-spec is_sms(kapps_call:call()) -> boolean().
is_sms(Call) ->
    kapps_call:resource_type(Call) =:= <<"sms">>.

-spec create_mobile_sms_endpoint(kz_json:object(), kz_json:object(), kapps_call:call()) ->
                                        kz_json:object() |
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
                      ,{<<"Endpoint-Options">>, kz_json:from_list(Options)}
                      ,{<<"To-DID">>, get_to_did(Endpoint, Call)}
                      ,{<<"Callee-ID-Name">>, Clid#clid.callee_name}
                      ,{<<"Callee-ID-Number">>, Clid#clid.callee_number}
                      ,{<<"Presence-ID">>, cf_attributes:presence_id(Endpoint, Call)}
                      ,{<<"Custom-SIP-Headers">>, generate_sip_headers(Endpoint, Call)}
                      ,{<<"Custom-Channel-Vars">>, generate_ccvs(Endpoint, Call, kz_json:new())}
                     ]),
            EP = create_mobile_sms_endpoint_failover(Prop, Failover),
            kz_json:from_list(EP)
    end.

-spec create_mobile_sms_endpoint_failover(kz_proplist(), sms_routes()) -> kz_proplist().
create_mobile_sms_endpoint_failover(Endpoint, []) -> Endpoint;
create_mobile_sms_endpoint_failover(Endpoint, [{Route, Options} | Failover]) ->
    EP = props:set_values(
                      [{<<"Route">>, Route}
                       ,{<<"Endpoint-Options">>, kz_json:from_list(Options)}
                      ], Endpoint),
    props:set_value(<<"Failover">>, kz_json:from_list(create_mobile_sms_endpoint_failover(EP, Failover)), Endpoint).


-spec maybe_build_mobile_sms_route(kz_json:object()) ->
                                          ne_binary() |
                                          {'error', 'mdn_missing'}.
maybe_build_mobile_sms_route(Endpoint) ->
    case kz_json:get_ne_value([<<"mobile">>, <<"mdn">>], Endpoint) of
        'undefined' ->
            lager:info("unable to build mobile sms endpoint, MDN missing"),
            {'error', 'mdn_missing'};
        MDN -> build_mobile_sms_route(MDN)
    end.

-spec build_mobile_sms_route(ne_binary()) ->
                                    {ne_binary(), sms_routes()} |
                                    {'error', 'invalid_mdn'}.
build_mobile_sms_route(MDN) ->
    Type = kapps_config:get(?CF_MOBILE_CONFIG_CAT, <<"sms_interface">>, ?DEFAULT_MOBILE_SMS_INTERFACE),
    build_mobile_sms_route(Type, MDN).

-spec build_mobile_sms_route(ne_binary(), ne_binary()) ->
                                    {ne_binary(), sms_routes()} |
                                    {'error', 'invalid_mdn'}.
build_mobile_sms_route(<<"sip">>, MDN) ->
    {<<"sip">>, [{build_mobile_route(MDN), 'undefined'}]};
build_mobile_sms_route(<<"amqp">>, _MDN) ->
    Connections = kapps_config:get(?CF_MOBILE_CONFIG_CAT, [<<"sms">>, <<"connections">>], ?DEFAULT_MOBILE_AMQP_CONNECTIONS),
    {<<"amqp">>, kz_json:foldl(fun build_mobile_sms_amqp_route/3 , [], Connections)}.

-spec build_mobile_sms_amqp_route(kz_json:key(), kz_json:json_term(), kz_proplist()) -> sms_routes().
build_mobile_sms_amqp_route(K, JObj, Acc) ->
    Broker = kz_json:get_value(<<"broker">>, JObj),
    Acc ++ [{Broker, [{<<"Broker-Name">>, K} | build_mobile_sms_amqp_route_options(JObj)]}].

-spec build_mobile_sms_amqp_route_options(kz_json:object()) -> kz_proplist().
build_mobile_sms_amqp_route_options(JObj) ->
    [{<<"Route-ID">>, kz_json:get_value(<<"route">>, JObj, ?DEFAULT_MOBILE_SMS_ROUTE)}
     ,{<<"Exchange-ID">>, kz_json:get_value(<<"exchange">>, JObj, ?DEFAULT_MOBILE_SMS_EXCHANGE)}
     ,{<<"Exchange-Type">>, kz_json:get_value(<<"type">>, JObj, ?DEFAULT_MOBILE_SMS_EXCHANGE_TYPE)}
     ,{<<"Exchange-Options">>, kz_json:get_value(<<"options">>, JObj, ?DEFAULT_MOBILE_SMS_EXCHANGE_OPTIONS)}
    ].
