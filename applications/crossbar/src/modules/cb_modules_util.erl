%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%% Functions shared between crossbar modules
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_modules_util).

-export([pass_hashes/2
         ,update_mwi/2
         ,get_devices_owned_by/2
         ,maybe_originate_quickcall/1
         ,is_superduper_admin/1

         ,attachment_name/2
         ,content_type_to_extension/1
         ,parse_media_type/1

         ,bucket_name/1
         ,token_cost/1, token_cost/2
         ,reconcile_services/1
         ,bind/2

         ,range_view_options/1, range_view_options/2
        ]).

-include("../crossbar.hrl").

-define(MAX_RANGE, whapps_config:get_integer(?CONFIG_CAT
                                             ,<<"maximum_range">>
                                             ,(?SECONDS_IN_DAY * 31 + ?SECONDS_IN_HOUR)
                                            )
       ).

-spec range_view_options(cb_context:context()) ->
                                {pos_integer(), pos_integer()} |
                                cb_context:context().
-spec range_view_options(cb_context:context(), pos_integer()) ->
                                {pos_integer(), pos_integer()} |
                                cb_context:context().
range_view_options(Context) ->
    range_view_options(Context, ?MAX_RANGE).
range_view_options(Context, MaxRange) ->
    TStamp =  wh_util:current_tstamp(),
    CreatedTo = created_to(Context, TStamp),
    CreatedFrom = created_from(Context, CreatedTo, MaxRange),

    case CreatedTo - CreatedFrom of
        N when N < 0 ->
            cb_context:add_validation_error(
                <<"created_from">>
                ,<<"date_range">>
                ,wh_json:from_list([
                    {<<"message">>, <<"created_from is prior to created_to">>}
                    ,{<<"cause">>, CreatedFrom}
                 ])
                ,Context
            );
        N when N > MaxRange ->
            Message = <<"created_to is more than "
                        ,(wh_util:to_binary(MaxRange))/binary
                        ," seconds from created_from"
                      >>,
            cb_context:add_validation_error(
                <<"created_from">>
                ,<<"date_range">>
                ,wh_json:from_list([
                    {<<"message">>, Message}
                    ,{<<"cause">>, CreatedTo}
                 ])
                ,Context
            );
        _N -> {CreatedFrom, CreatedTo}
    end.

-spec created_to(cb_context:context(), pos_integer()) -> pos_integer().
created_to(Context, TStamp) ->
    case crossbar_doc:start_key(Context) of
        'undefined' ->
            lager:debug("building created_to from req value"),
            wh_util:to_integer(cb_context:req_value(Context, <<"created_to">>, TStamp));
        StartKey ->
            lager:debug("found startkey ~p as created_to", [StartKey]),
            wh_util:to_integer(StartKey)
    end.

-spec created_from(cb_context:context(), pos_integer(), pos_integer()) -> pos_integer().
created_from(Context, CreatedTo, MaxRange) ->
    lager:debug("building created_from from req value"),
    wh_util:to_integer(cb_context:req_value(Context, <<"created_from">>, CreatedTo - MaxRange)).

-spec bind(atom(), wh_proplist()) -> 'ok'.
bind(Module, Bindings) ->
    [crossbar_bindings:bind(Binding, Module, Function)
     || {Binding, Function} <- Bindings
    ],
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Bill for devices
%% @end
%%--------------------------------------------------------------------
-spec reconcile_services(cb_context:context()) -> cb_context:context().
reconcile_services(Context) ->
    case cb_context:resp_status(Context) =:= 'success'
        andalso cb_context:req_verb(Context) =/= <<"GET">>
    of
        'false' -> Context;
        'true' ->
            lager:debug("maybe reconciling services for account ~s"
                       ,[cb_context:account_id(Context)]),
            _ = wh_services:save_as_dirty(cb_context:account_id(Context))
    end.

-spec pass_hashes(ne_binary(), ne_binary()) -> {ne_binary(), ne_binary()}.
pass_hashes(Username, Password) ->
    Creds = list_to_binary([Username, ":", Password]),
    SHA1 = wh_util:to_hex_binary(crypto:hash(sha, Creds)),
    MD5 = wh_util:to_hex_binary(crypto:hash(md5, Creds)),
    {MD5, SHA1}.

-spec update_mwi(api_binary(), ne_binary()) -> pid().
update_mwi(OwnerId, AccountDb) ->
    spawn(fun() ->
                  timer:sleep(1000),
                  cf_util:unsolicited_owner_mwi_update(AccountDb, OwnerId)
          end).

-spec get_devices_owned_by(ne_binary(), ne_binary()) -> wh_json:objects().
get_devices_owned_by(OwnerID, DB) ->
    case couch_mgr:get_results(DB
                               ,<<"cf_attributes/owned">>
                               ,[{'key', [OwnerID, <<"device">>]}
                                 ,'include_docs'
                                ])
    of
        {'ok', JObjs} ->
            lager:debug("found ~b devices owned by ~s", [length(JObjs), OwnerID]),
            [wh_json:get_value(<<"doc">>, JObj) || JObj <- JObjs];
        {'error', _R} ->
            lager:debug("unable to fetch devices: ~p", [_R]),
            []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% TODO: this breaks the callflow/crossbar seperation....
%% @end
%%--------------------------------------------------------------------
-spec maybe_originate_quickcall(cb_context:context()) -> cb_context:context().
maybe_originate_quickcall(Context) ->
    Call = create_call_from_context(Context),
    case get_endpoints(Call, Context) of
        [] ->
            cb_context:add_system_error('unspecified_fault', Context);
        Endpoints ->
            originate_quickcall(Endpoints, Call, default_bleg_cid(Call, Context))
    end.

-spec create_call_from_context(cb_context:context()) -> whapps_call:call().
create_call_from_context(Context) ->
    Routines =
        props:filter_undefined(
          [{fun whapps_call:set_account_db/2, cb_context:account_db(Context)}
           ,{fun whapps_call:set_account_id/2, cb_context:account_id(Context)}
           ,{fun whapps_call:set_owner_id/2, wh_json:get_ne_value(<<"owner_id">>, cb_context:doc(Context))}
           | request_specific_extraction_funs(Context)
          ]),
    whapps_call:exec(Routines, whapps_call:new()).

-spec request_specific_extraction_funs(cb_context:context()) -> functions().
-spec request_specific_extraction_funs_from_nouns(cb_context:context(), req_nouns()) -> functions().
request_specific_extraction_funs(Context) ->
    request_specific_extraction_funs_from_nouns(Context, cb_context:req_nouns(Context)).

request_specific_extraction_funs_from_nouns(Context, ?DEVICES_QCALL_NOUNS(DeviceId, Number)) ->
    NumberURI = build_number_uri(Context, Number),
    [{fun whapps_call:set_authorizing_id/2, DeviceId}
     ,{fun whapps_call:set_authorizing_type/2, <<"device">>}
     ,{fun whapps_call:set_request/2, NumberURI}
     ,{fun whapps_call:set_to/2, NumberURI}
    ];
request_specific_extraction_funs_from_nouns(Context, ?USERS_QCALL_NOUNS(UserId, Number)) ->
    NumberURI = build_number_uri(Context, Number),
    [{fun whapps_call:set_authorizing_id/2, UserId}
     ,{fun whapps_call:set_authorizing_type/2, <<"user">>}
     ,{fun whapps_call:set_request/2, NumberURI}
     ,{fun whapps_call:set_to/2, NumberURI}
    ];
request_specific_extraction_funs_from_nouns(_Context, _ReqNouns) ->
    [].

-spec build_number_uri(cb_context:context(), ne_binary()) -> ne_binary().
build_number_uri(Context, Number) ->
    Realm = wh_util:get_account_realm(cb_context:account_id(Context)),
    <<Number/binary, "@", Realm/binary>>.

-spec get_endpoints(whapps_call:call(), cb_context:context()) -> wh_json:objects().
-spec get_endpoints(whapps_call:call(), cb_context:context(), req_nouns()) -> wh_json:objects().
get_endpoints(Call, Context) ->
    get_endpoints(Call, Context, cb_context:req_nouns(Context)).

get_endpoints(Call, Context, ?DEVICES_QCALL_NOUNS(_DeviceId, Number)) ->
    Properties = wh_json:from_list([{<<"can_call_self">>, 'true'}
                                    ,{<<"suppress_clid">>, 'true'}
                                    ,{<<"source">>, 'cb_devices'}
                                   ]),
    case cf_endpoint:build(cb_context:doc(Context), Properties, aleg_cid(Number, Call)) of
        {'error', _} -> [];
        {'ok', []} -> [];
        {'ok', Endpoints} -> Endpoints
    end;
get_endpoints(Call, _Context, ?USERS_QCALL_NOUNS(_UserId, Number)) ->
    Properties = wh_json:from_list([{<<"can_call_self">>, 'true'}
                                    ,{<<"suppress_clid">>, 'true'}
                                    ,{<<"source">>, 'cb_users'}
                                   ]),
    lists:foldr(fun(EndpointId, Acc) ->
                        case cf_endpoint:build(EndpointId, Properties, aleg_cid(Number, Call)) of
                            {'ok', Endpoint} -> Endpoint ++ Acc;
                            {'error', _E} -> Acc
                        end
                end, [], cf_attributes:owned_by(_UserId, <<"device">>, Call));
get_endpoints(_Call, _Context, _ReqNouns) ->
    [].

-spec aleg_cid(ne_binary(), whapps_call:call()) -> whapps_call:call().
aleg_cid(Number, Call) ->
    Routines = [fun(C) -> whapps_call:set_custom_channel_var(<<"Retain-CID">>, <<"true">>, C) end
                ,fun(C) -> whapps_call:set_caller_id_name(<<"QuickCall">>, C) end
                ,fun(C) -> whapps_call:set_caller_id_number(wh_util:to_binary(Number), C) end
               ],
    lists:foldl(fun(F, C) -> F(C) end, Call, Routines).

-spec default_bleg_cid(whapps_call:call(), cb_context:context()) -> cb_context:context().
default_bleg_cid(Call, Context) ->
    {CIDNumber, CIDName} = cf_attributes:caller_id(<<"external">>, Call),
    Defaults = wh_json:from_list([{<<"cid-name">>, CIDName}
                                  ,{<<"cid-number">>, CIDNumber}
                                 ]),
    cb_context:set_query_string(Context
                                ,wh_json:merge_jobjs(cb_context:query_string(Context), Defaults)
                               ).

-spec originate_quickcall(wh_json:objects(), whapps_call:call(), cb_context:context()) -> cb_context:context().
originate_quickcall(Endpoints, Call, Context) ->
    CCVs = [{<<"Account-ID">>, cb_context:account_id(Context)}
            ,{<<"Retain-CID">>, <<"true">>}
            ,{<<"Inherit-Codec">>, <<"false">>}
            ,{<<"Authorizing-Type">>, whapps_call:authorizing_type(Call)}
            ,{<<"Authorizing-ID">>, whapps_call:authorizing_id(Call)}
           ],
    MsgId = case wh_util:is_empty(cb_context:req_id(Context)) of
                'true' -> wh_util:rand_hex_binary(16);
                'false' -> cb_context:req_id(Context)
            end,
    Request = [{<<"Application-Name">>, <<"transfer">>}
               ,{<<"Application-Data">>, get_application_data(Context)}
               ,{<<"Msg-ID">>, MsgId}
               ,{<<"Endpoints">>, maybe_auto_answer(Endpoints)}
               ,{<<"Timeout">>, get_timeout(Context)}
               ,{<<"Ignore-Early-Media">>, get_ignore_early_media(Context)}
               ,{<<"Media">>, get_media(Context)}
               ,{<<"Outbound-Caller-ID-Name">>, <<"Device QuickCall">>}
               ,{<<"Outbound-Caller-ID-Number">>, whapps_call:request_user(Call)}
               ,{<<"Outbound-Callee-ID-Name">>, get_caller_id_name(Context)}
               ,{<<"Outbound-Callee-ID-Number">>, get_caller_id_number(Context)}
               ,{<<"Dial-Endpoint-Method">>, <<"simultaneous">>}
               ,{<<"Continue-On-Fail">>, 'false'}
               ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
               ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Retain-CID">>, <<"Authorizing-ID">>, <<"Authorizing-Type">>]}
               | wh_api:default_headers(<<"resource">>, <<"originate_req">>, ?APP_NAME, ?APP_VERSION)
              ],
    wapi_resource:publish_originate_req(props:filter_undefined(Request)),
    crossbar_util:response_202(<<"processing request">>, cb_context:set_resp_data(Context, Request)).

-spec maybe_auto_answer(wh_json:objects()) -> wh_json:objects().
maybe_auto_answer([Endpoint]) ->
    [wh_json:set_value([<<"Custom-Channel-Vars">>, <<"Auto-Answer">>], 'true', Endpoint)];
maybe_auto_answer(Endpoints) ->
    Endpoints.

-spec get_application_data(cb_context:context()) -> wh_json:object().
-spec get_application_data_from_nouns(req_nouns()) -> wh_json:object().
get_application_data(Context) ->
    get_application_data_from_nouns(cb_context:req_nouns(Context)).

get_application_data_from_nouns(?DEVICES_QCALL_NOUNS(_DeviceId, Number)) ->
    wh_json:from_list([{<<"Route">>, Number}]);
get_application_data_from_nouns(?USERS_QCALL_NOUNS(_UserId, Number)) ->
    wh_json:from_list([{<<"Route">>, Number}]);
get_application_data_from_nouns(_Nouns) ->
    wh_json:from_list([{<<"Route">>, <<"0">>}]).

-spec get_timeout(cb_context:context()) -> pos_integer().
get_timeout(Context) ->
    try wh_util:to_integer(cb_context:req_value(Context, <<"timeout">>, 30)) of
        Timeout when Timeout > 3 -> Timeout;
        _ -> 30
    catch
        _:_ -> 30
    end.

-spec get_ignore_early_media(cb_context:context()) -> boolean().
get_ignore_early_media(Context) ->
    wh_util:is_true(cb_context:req_value(Context, <<"ignore-early-media">>)).

-spec get_media(cb_context:context()) -> ne_binary().
get_media(Context) ->
    case cb_context:req_value(Context, <<"media">>) of
        <<"bypass">> -> <<"bypass">>;
        _Else -> <<"process">>
    end.

-spec get_caller_id_name(cb_context:context()) -> api_binary().
get_caller_id_name(Context) ->
    case cb_context:req_value(Context, <<"cid-name">>) of
        'undefined' -> 'undefined';
        CIDName -> wh_util:uri_decode(CIDName)
    end.

-spec get_caller_id_number(cb_context:context()) -> api_binary().
get_caller_id_number(Context) ->
    case cb_context:req_value(Context, <<"cid-number">>) of
        'undefined' -> 'undefined';
        CIDNumber -> wh_util:uri_decode(CIDNumber)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns true if the request contains a system admin module.
%% @end
%%--------------------------------------------------------------------
-spec is_superduper_admin(api_binary() | cb_context:context()) -> boolean().
-spec is_superduper_admin(ne_binary(), ne_binary()) -> boolean().
is_superduper_admin('undefined') -> 'false';
is_superduper_admin(<<_/binary>> = AccountId) ->
    is_superduper_admin(AccountId, wh_util:format_account_id(AccountId, 'encoded'));
is_superduper_admin(Context) ->
    is_superduper_admin(cb_context:auth_account_id(Context)).

is_superduper_admin(AccountId, AccountDb) ->
    lager:debug("checking for superduper admin: ~s (~s)", [AccountId, AccountDb]),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'ok', JObj} ->
            case wh_json:is_true(<<"pvt_superduper_admin">>, JObj) of
                'true' ->
                    lager:debug("the requestor is a superduper admin"),
                    'true';
                'false' ->
                    lager:debug("the requestor is not a superduper admin"),
                    'false'
            end;
        {'error', _E} ->
            lager:debug("not authorizing, error during lookup: ~p", [_E]),
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generate an attachment name if one is not provided and ensure
%% it has an extension (for the associated content type)
%% @end
%%--------------------------------------------------------------------
-spec attachment_name(ne_binary(), text()) -> ne_binary().
attachment_name(Filename, CT) ->
    Generators = [fun(A) ->
                          case wh_util:is_empty(A) of
                              'true' -> wh_util:to_hex_binary(crypto:rand_bytes(16));
                              'false' -> A
                          end
                  end
                  ,fun(A) ->
                           case wh_util:is_empty(filename:extension(A)) of
                               'false' -> A;
                               'true' ->
                                   <<A/binary, ".", (content_type_to_extension(CT))/binary>>
                           end
                   end
                 ],
    lists:foldl(fun(F, A) -> F(A) end, Filename, Generators).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert known media types to extensions
%% @end
%%--------------------------------------------------------------------
-spec content_type_to_extension(text()) -> ne_binary().
content_type_to_extension(L) when not is_binary(L) ->
    content_type_to_extension(wh_util:to_binary(L));
content_type_to_extension(<<"audio/wav">>) -> <<"wav">>;
content_type_to_extension(<<"audio/x-wav">>) -> <<"wav">>;
content_type_to_extension(<<"audio/mpeg">>) -> <<"mp3">>;
content_type_to_extension(<<"audio/mpeg3">>) -> <<"mp3">>;
content_type_to_extension(<<"audio/mp3">>) -> <<"mp3">>;
content_type_to_extension(<<"audio/ogg">>) -> <<"ogg">>;
content_type_to_extension(<<"application/x-pdf">>) -> <<"pdf">>;
content_type_to_extension(<<"application/pdf">>) -> <<"pdf">>;
content_type_to_extension(<<"image/jpg">>) -> <<"jpg">>;
content_type_to_extension(<<"image/jpeg">>) -> <<"jpg">>;
content_type_to_extension(<<"image/png">>) -> <<"png">>;
content_type_to_extension(<<"image/gif">>) -> <<"gif">>;
content_type_to_extension(<<"text/html">>) -> <<"html">>;
content_type_to_extension(<<"text/plain">>) -> <<"txt">>.

-spec parse_media_type(ne_binary()) ->
                              {'error', 'badarg'} |
                              media_values().
parse_media_type(MediaType) ->
    cowboy_http:nonempty_list(MediaType, fun cowboy_http:media_range/2).

-spec bucket_name(cb_context:context()) -> ne_binary().
-spec bucket_name(api_binary(), api_binary()) -> ne_binary().
bucket_name(Context) ->
    bucket_name(cb_context:client_ip(Context)
                ,cb_context:account_id(Context)
               ).

bucket_name('undefined', 'undefined') ->
    <<"no_ip/no_account">>;
bucket_name(IP, 'undefined') ->
    <<IP/binary, "/no_account">>;
bucket_name('undefined', AccountId) ->
    <<"no_ip/", AccountId/binary>>;
bucket_name(IP, AccountId) ->
    <<IP/binary, "/", AccountId/binary>>.

-spec token_cost(cb_context:context()) -> non_neg_integer().
-spec token_cost(cb_context:context(), non_neg_integer() | wh_json:key() | wh_json:keys()) -> non_neg_integer().
-spec token_cost(cb_context:context(), non_neg_integer(), wh_json:keys()) -> non_neg_integer().

token_cost(Context) ->
    token_cost(Context, 1).

token_cost(Context, <<_/binary>> = Suffix) ->
    token_cost(Context, 1, [Suffix]);
token_cost(Context, [_|_]=Suffix) ->
    token_cost(Context, 1, Suffix);
token_cost(Context, Default) ->
    token_cost(Context, Default, []).

token_cost(Context, Default, Suffix) when is_integer(Default), Default >= 0 ->
    Costs = whapps_config:get(?CONFIG_CAT, <<"token_costs">>, 1),
    find_token_cost(Costs
                    ,Default
                    ,Suffix
                    ,cb_context:req_nouns(Context)
                    ,cb_context:req_verb(Context)
                    ,cb_context:account_id(Context)
                   ).

-spec find_token_cost(wh_json:object() | non_neg_integer()
                      ,non_neg_integer()
                      ,wh_json:keys()
                      ,req_nouns()
                      ,http_method()
                      ,api_binary()
                     ) ->
                             non_neg_integer().

find_token_cost(N, _Default, _Suffix, _Nouns, _ReqVerb, _AccountId) when is_integer(N) ->
    lager:debug("flat token cost of ~p configured", [N]),
    N;
find_token_cost(JObj, Default, Suffix, [{Endpoint, _} | _], ReqVerb, 'undefined') ->
    Keys = [[Endpoint, ReqVerb | Suffix]
            ,[Endpoint | Suffix]
           ],
    get_token_cost(JObj, Default, Keys);
find_token_cost(JObj, Default, Suffix, [{Endpoint, _}|_], ReqVerb, AccountId) ->
    Keys = [[AccountId, Endpoint, ReqVerb | Suffix]
            ,[AccountId, Endpoint | Suffix]
            ,[AccountId | Suffix]
            ,[Endpoint, ReqVerb | Suffix]
            ,[Endpoint | Suffix]
           ],
    get_token_cost(JObj, Default, Keys).

-spec get_token_cost(wh_json:object(), non_neg_integer(), wh_json:keys()) ->
                            non_neg_integer().
get_token_cost(JObj, Default, Keys) ->
    case wh_json:get_first_defined(Keys, JObj) of
        'undefined' -> Default;
        V -> wh_util:to_integer(V)
    end.
