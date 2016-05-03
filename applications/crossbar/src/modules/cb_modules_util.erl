%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
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
         ,parse_media_type/1

         ,bucket_name/1
         ,token_cost/1, token_cost/2, token_cost/3
         ,bind/2

         ,range_view_options/1, range_view_options/2, range_view_options/3, range_view_options/5

         ,range_modb_view_options/1, range_modb_view_options/2, range_modb_view_options/3, range_modb_view_options/5

         ,take_sync_field/1

         ,remove_plaintext_password/1

         ,apply_assignment_updates/1
         ,log_assignment_updates/1
        ]).

-include("crossbar.hrl").

-define(QCALL_NUMBER_FILTER, [<<" ">>, <<",">>, <<".">>, <<"-">>, <<"(">>, <<")">>]).

-spec range_view_options(cb_context:context()) ->
                                {gregorian_seconds(), gregorian_seconds()} |
                                cb_context:context().
-spec range_view_options(cb_context:context(), pos_integer()) ->
                                {gregorian_seconds(), gregorian_seconds()} |
                                cb_context:context().
range_view_options(Context) ->
    range_view_options(Context, ?MAX_RANGE).
range_view_options(Context, MaxRange) ->
    range_view_options(Context, MaxRange, <<"created">>).
range_view_options(Context, MaxRange, Key) ->
    TStamp =  kz_util:current_tstamp(),
    RangeTo = range_to(Context, TStamp, Key),
    RangeFrom = range_from(Context, RangeTo, MaxRange, Key),
    range_view_options(Context, MaxRange, Key, RangeFrom, RangeTo).
range_view_options(Context, MaxRange, Key, RangeFrom, RangeTo) ->
    case RangeTo - RangeFrom of
        N when N < 0 ->
            cb_context:add_validation_error(
              <<Key/binary, "_from">>
              ,<<"date_range">>
              ,kz_json:from_list(
                 [{<<"message">>, <<Key/binary, "_from is prior to ", Key/binary, "_to">>}
                  ,{<<"cause">>, RangeFrom}
                 ])
              ,Context
             );
        N when N > MaxRange ->
            Message = <<Key/binary, "_to is more than "
                        ,(kz_util:to_binary(MaxRange))/binary
                        ," seconds from ", Key/binary, "_from"
                      >>,
            cb_context:add_validation_error(
              <<Key/binary, "_from">>
              ,<<"date_range">>
              ,kz_json:from_list(
                 [{<<"message">>, Message}
                  ,{<<"cause">>, RangeTo}
                 ])
              ,Context
             );
        _N -> {RangeFrom, RangeTo}
    end.

-spec range_modb_view_options(cb_context:context()) ->
                                     {'ok', kz_proplist()} |
                                     cb_context:context().
range_modb_view_options(Context) ->
    range_modb_view_options(Context, 'undefined', 'undefined').

-spec range_modb_view_options(cb_context:context(), api([api(binary())])) ->
                                     {'ok', kz_proplist()} |
                                     cb_context:context().
range_modb_view_options(Context, PrefixKeys) ->
    range_modb_view_options(Context, PrefixKeys, 'undefined').

-spec range_modb_view_options(cb_context:context(), api([api(binary())]), api([api(binary())])) ->
                                     {'ok', crossbar_doc:view_options()} |
                                     cb_context:context().
range_modb_view_options(Context, 'undefined', SuffixKeys) ->
    range_modb_view_options(Context, [], SuffixKeys);
range_modb_view_options(Context, PrefixKeys, 'undefined') ->
    range_modb_view_options(Context, PrefixKeys, []);
range_modb_view_options(Context, PrefixKeys, SuffixKeys) ->
    case ?MODULE:range_view_options(Context) of
        {CreatedFrom, CreatedTo} ->
            range_modb_view_options1(Context, PrefixKeys, SuffixKeys, CreatedFrom, CreatedTo);
        Context1 -> Context1
    end.

-spec range_modb_view_options(cb_context:context(), api([api(binary())]), api([api(binary())]), gregorian_seconds(), gregorian_seconds()) ->
                                     {'ok', crossbar_doc:view_options()} |
                                     cb_context:context().
range_modb_view_options(Context, PrefixKeys, SuffixKeys, CreatedFrom, CreatedTo) ->
    case ?MODULE:range_view_options(Context, ?MAX_RANGE, <<"created">>, CreatedFrom, CreatedTo) of
        {CreatedFrom, CreatedTo} ->
            range_modb_view_options1(Context, PrefixKeys, SuffixKeys, CreatedFrom, CreatedTo);
        Context1 -> Context1
    end.

-spec range_modb_view_options1(cb_context:context(), api([api(binary())]), api([api(binary())]), gregorian_seconds(), gregorian_seconds()) ->
                                     {'ok', crossbar_doc:view_options()} |
                                     cb_context:context().
range_modb_view_options1(Context, PrefixKeys, SuffixKeys, CreatedFrom, CreatedTo) ->
    AccountId = cb_context:account_id(Context),
    case PrefixKeys =:= [] andalso SuffixKeys =:= [] of
        'true' -> {'ok', [{'startkey', CreatedFrom}
                          ,{'endkey', CreatedTo}
                          ,{'databases', kazoo_modb:get_range(AccountId, CreatedFrom, CreatedTo)}
                         ]};
        'false' -> {'ok', [{'startkey', [Key || Key <- PrefixKeys ++ [CreatedFrom] ++ SuffixKeys] }
                           ,{'endkey', [Key || Key <- PrefixKeys  ++ [CreatedTo]   ++ SuffixKeys] }
                           ,{'databases', kazoo_modb:get_range(AccountId, CreatedFrom, CreatedTo)}
                          ]}
    end.

-spec range_to(cb_context:context(), pos_integer(), ne_binary()) -> pos_integer().
range_to(Context, TStamp, Key) ->
    case crossbar_doc:start_key(Context) of
        'undefined' ->
            lager:debug("building ~s_to from req value", [Key]),
            kz_util:to_integer(cb_context:req_value(Context, <<Key/binary, "_to">>, TStamp));
        StartKey ->
            lager:debug("found startkey ~p as ~s_to", [StartKey, Key]),
            kz_util:to_integer(StartKey)
    end.

-spec range_from(cb_context:context(), pos_integer(), pos_integer(), ne_binary()) -> pos_integer().
range_from(Context, CreatedTo, MaxRange, Key) ->
    lager:debug("building ~s_from from req value", [Key]),
    kz_util:to_integer(cb_context:req_value(Context, <<Key/binary, "_from">>, CreatedTo - MaxRange)).

-type binding() :: {ne_binary(), atom()}.
-type bindings() :: [binding(),...].
-spec bind(atom(), bindings()) -> 'ok'.
bind(Module, Bindings) ->
    _ = [crossbar_bindings:bind(Binding, Module, Function)
         || {Binding, Function} <- Bindings
        ],
    'ok'.

-spec pass_hashes(ne_binary(), ne_binary()) -> {ne_binary(), ne_binary()}.
pass_hashes(Username, Password) ->
    Creds = list_to_binary([Username, ":", Password]),
    SHA1 = kz_util:to_hex_binary(crypto:hash('sha', Creds)),
    MD5 = kz_util:to_hex_binary(crypto:hash('md5', Creds)),
    {MD5, SHA1}.

-spec update_mwi(api(binary()), ne_binary()) -> pid().
update_mwi(OwnerId, AccountDb) ->
    kz_util:spawn(fun() ->
                          timer:sleep(?MILLISECONDS_IN_SECOND),
                          cf_util:unsolicited_owner_mwi_update(AccountDb, OwnerId)
                  end).

-spec get_devices_owned_by(ne_binary(), ne_binary()) -> kz_json:objects().
get_devices_owned_by(OwnerID, DB) ->
    case kz_datamgr:get_results(DB
                               ,<<"cf_attributes/owned">>
                               ,[{'key', [OwnerID, <<"device">>]}
                                 ,'include_docs'
                                ])
    of
        {'ok', JObjs} ->
            lager:debug("found ~b devices owned by ~s", [length(JObjs), OwnerID]),
            [kz_json:get_value(<<"doc">>, JObj) || JObj <- JObjs];
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
            originate_quickcall(Endpoints, Call, Context)
    end.

-spec create_call_from_context(cb_context:context()) -> kapps_call:call().
create_call_from_context(Context) ->
    Routines =
        props:filter_undefined(
          [{fun kapps_call:set_account_db/2, cb_context:account_db(Context)}
           ,{fun kapps_call:set_account_id/2, cb_context:account_id(Context)}
           ,{fun kapps_call:set_resource_type/2, <<"audio">>}
           ,{fun kapps_call:set_owner_id/2, kz_json:get_ne_value(<<"owner_id">>, cb_context:doc(Context))}
           | request_specific_extraction_funs(Context)
          ]),
    kapps_call:exec(Routines, kapps_call:new()).

-spec request_specific_extraction_funs(cb_context:context()) -> kapps_call:exec_funs().
-spec request_specific_extraction_funs_from_nouns(cb_context:context(), req_nouns()) ->
                                                         kapps_call:exec_funs().
request_specific_extraction_funs(Context) ->
    request_specific_extraction_funs_from_nouns(Context, cb_context:req_nouns(Context)).

request_specific_extraction_funs_from_nouns(Context, ?DEVICES_QCALL_NOUNS(DeviceId, Number)) ->
    NumberURI = build_number_uri(Context, Number),
    [{fun kapps_call:set_authorizing_id/2, DeviceId}
     ,{fun kapps_call:set_authorizing_type/2, <<"device">>}
     ,{fun kapps_call:set_request/2, NumberURI}
     ,{fun kapps_call:set_to/2, NumberURI}
    ];
request_specific_extraction_funs_from_nouns(Context, ?USERS_QCALL_NOUNS(UserId, Number)) ->
    NumberURI = build_number_uri(Context, Number),
    [{fun kapps_call:set_authorizing_id/2, UserId}
     ,{fun kapps_call:set_authorizing_type/2, <<"user">>}
     ,{fun kapps_call:set_request/2, NumberURI}
     ,{fun kapps_call:set_to/2, NumberURI}
    ];
request_specific_extraction_funs_from_nouns(_Context, _ReqNouns) ->
    [].

-spec filter_number_regex(ne_binary(), ne_binary()) -> ne_binary().
filter_number_regex(Number, Regex) ->
    case re:run(Number, Regex, [{'capture', 'all_but_first', 'binary'}]) of
        {'match', [Match|_]} ->
            lager:info("filtered number using regex ~p, result: ~p", [Regex, Match]),
            Match;

        _NotMatching ->
            lager:warning("tried to filter number ~p with regex ~p, but no match found", [Number, Regex]),
            Number
    end.

-spec build_number_uri(cb_context:context(), ne_binary()) -> ne_binary().
build_number_uri(Context, Number) ->
    QueryStr  = cb_context:query_string(Context),
    FilterVal = kz_json:get_value(<<"number_filter">>, QueryStr, <<"true">>),

    UseNumber = case FilterVal of
        <<"false">> -> Number;
        <<"true">>  -> binary:replace(Number, ?QCALL_NUMBER_FILTER, <<>>, ['global']);
        FilterRegex -> filter_number_regex(Number, FilterRegex)
    end,

    Realm = kz_util:get_account_realm(cb_context:account_id(Context)),
    <<UseNumber/binary, "@", Realm/binary>>.

-spec get_endpoints(kapps_call:call(), cb_context:context()) -> kz_json:objects().
-spec get_endpoints(kapps_call:call(), cb_context:context(), req_nouns()) -> kz_json:objects().
get_endpoints(Call, Context) ->
    get_endpoints(Call, Context, cb_context:req_nouns(Context)).

get_endpoints(Call, Context, ?DEVICES_QCALL_NOUNS(_DeviceId, Number)) ->
    Properties = kz_json:from_list([{<<"can_call_self">>, 'true'}
                                    ,{<<"suppress_clid">>, 'true'}
                                    ,{<<"source">>, 'cb_devices'}
                                   ]),
    case cf_endpoint:build(cb_context:doc(Context), Properties, aleg_cid(Number, Call)) of
        {'error', _} -> [];
        {'ok', []} -> [];
        {'ok', Endpoints} -> Endpoints
    end;
get_endpoints(Call, _Context, ?USERS_QCALL_NOUNS(_UserId, Number)) ->
    Properties = kz_json:from_list([{<<"can_call_self">>, 'true'}
                                   ,{<<"suppress_clid">>, 'true'}
                                   ,{<<"source">>, 'cb_users'}
                                   ]),
    lists:foldr(fun(EndpointId, Acc) ->
                        case cf_endpoint:build(EndpointId, Properties, aleg_cid(Number, Call)) of
                            {'ok', Endpoint} -> Endpoint ++ Acc;
                            {'error', _E} -> Acc
                        end
                end
               ,[]
               ,cf_attributes:owned_by(_UserId, <<"device">>, Call)
               );
get_endpoints(_Call, _Context, _ReqNouns) ->
    [].

-spec aleg_cid(ne_binary(), kapps_call:call()) -> kapps_call:call().
aleg_cid(Number, Call) ->
    Routines = [{fun kapps_call:set_custom_channel_var/3, <<"Retain-CID">>, <<"true">>}
               ,{fun kapps_call:set_caller_id_name/2, <<"QuickCall">>}
               ,{fun kapps_call:set_caller_id_number/2, kz_util:to_binary(Number)}
               ],
    kapps_call:exec(Routines, Call).

-spec originate_quickcall(kz_json:objects(), kapps_call:call(), cb_context:context()) ->
                                 cb_context:context().
originate_quickcall(Endpoints, Call, Context) ->
    AutoAnswer = kz_json:is_true(<<"auto_answer">>, cb_context:query_string(Context), 'true'),
    CCVs = [{<<"Account-ID">>, cb_context:account_id(Context)}
            ,{<<"Retain-CID">>, <<"true">>}
            ,{<<"Inherit-Codec">>, <<"false">>}
            ,{<<"Authorizing-Type">>, kapps_call:authorizing_type(Call)}
            ,{<<"Authorizing-ID">>, kapps_call:authorizing_id(Call)}
           ],
    MsgId = case kz_util:is_empty(cb_context:req_id(Context)) of
                'true' -> kz_util:rand_hex_binary(16);
                'false' -> cb_context:req_id(Context)
            end,

    {DefaultCIDNumber, DefaultCIDName} = cf_attributes:caller_id(<<"external">>, Call),

    Request =
        kz_json:from_list(
          [{<<"Application-Name">>, <<"transfer">>}
          ,{<<"Application-Data">>, get_application_data(Context)}
          ,{<<"Msg-ID">>, MsgId}
          ,{<<"Endpoints">>, update_quickcall_endpoints(AutoAnswer, Endpoints)}
          ,{<<"Timeout">>, get_timeout(Context)}
          ,{<<"Ignore-Early-Media">>, get_ignore_early_media(Context)}
          ,{<<"Media">>, get_media(Context)}
          ,{<<"Outbound-Caller-ID-Name">>, <<"Device QuickCall">>}
          ,{<<"Outbound-Caller-ID-Number">>, kapps_call:request_user(Call)}
          ,{<<"Outbound-Callee-ID-Name">>, get_cid_name(Context, DefaultCIDName)}
          ,{<<"Outbound-Callee-ID-Number">>, get_cid_number(Context, DefaultCIDNumber)}
          ,{<<"Dial-Endpoint-Method">>, <<"simultaneous">>}
          ,{<<"Continue-On-Fail">>, 'false'}
          ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
          ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Retain-CID">>, <<"Authorizing-ID">>, <<"Authorizing-Type">>]}
           | kz_api:default_headers(<<"resource">>, <<"originate_req">>, ?APP_NAME, ?APP_VERSION)
          ]),
    kz_amqp_worker:cast(Request, fun kapi_resource:publish_originate_req/1),
    JObj = kz_json:normalize(kz_api:remove_defaults(Request)),
    crossbar_util:response_202(<<"quickcall initiated">>, JObj, cb_context:set_resp_data(Context, Request)).

-spec update_quickcall_endpoints(boolean(), kz_json:objects()) -> kz_json:objects().
update_quickcall_endpoints(AutoAnswer, [Endpoint]) ->
    WithAA = kz_json:set_value([<<"Custom-Channel-Vars">>, <<"Auto-Answer">>], AutoAnswer, Endpoint),
    [set_quickcall_outbound_call_id(WithAA)];
update_quickcall_endpoints(_AutoAnswer, Endpoints) ->
    [set_quickcall_outbound_call_id(Endpoint) || Endpoint <- Endpoints].

-spec set_quickcall_outbound_call_id(kz_json:object()) -> kz_json:object().
set_quickcall_outbound_call_id(Endpoint) ->
    CallId = <<(kz_util:rand_hex_binary(18))/binary, "-quickcall">>,
    kz_json:set_value(<<"Outbound-Call-ID">>, CallId, Endpoint).

-spec get_application_data(cb_context:context()) -> kz_json:object().
-spec get_application_data_from_nouns(req_nouns()) -> kz_json:object().
get_application_data(Context) ->
    get_application_data_from_nouns(cb_context:req_nouns(Context)).

get_application_data_from_nouns(?DEVICES_QCALL_NOUNS(_DeviceId, Number)) ->
    kz_json:from_list([{<<"Route">>, Number}]);
get_application_data_from_nouns(?USERS_QCALL_NOUNS(_UserId, Number)) ->
    kz_json:from_list([{<<"Route">>, Number}]);
get_application_data_from_nouns(_Nouns) ->
    kz_json:from_list([{<<"Route">>, <<"0">>}]).

-define(DEFAULT_TIMEOUT_S, 30).
-spec get_timeout(cb_context:context()) -> pos_integer().
get_timeout(Context) ->
    try kz_util:to_integer(cb_context:req_value(Context, <<"timeout">>, ?DEFAULT_TIMEOUT_S)) of
        Timeout when is_integer(Timeout), Timeout > 3 -> Timeout;
        _ -> ?DEFAULT_TIMEOUT_S
    catch
        _:_ -> ?DEFAULT_TIMEOUT_S
    end.

-spec get_ignore_early_media(cb_context:context()) -> boolean().
get_ignore_early_media(Context) ->
    kz_util:is_true(cb_context:req_value(Context, <<"ignore-early-media">>, 'true')).

-spec get_media(cb_context:context()) -> ne_binary().
get_media(Context) ->
    case cb_context:req_value(Context, <<"media">>) of
        <<"bypass">> -> <<"bypass">>;
        _Else -> <<"process">>
    end.

-spec get_cid_name(cb_context:context(), api(binary())) -> api(binary()).
get_cid_name(Context, Default) ->
    case cb_context:req_value(Context, <<"cid-name">>, Default) of
        'undefined' -> 'undefined';
        CIDName -> kz_util:uri_decode(CIDName)
    end.

-spec get_cid_number(cb_context:context(), api(binary())) -> api(binary()).
get_cid_number(Context, Default) ->
    case cb_context:req_value(Context, <<"cid-number">>, Default) of
        'undefined' -> 'undefined';
        CIDNumber -> kz_util:uri_decode(CIDNumber)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns true if the request contains a system admin module.
%% @end
%%--------------------------------------------------------------------
-spec is_superduper_admin(api(binary()) | cb_context:context()) -> boolean().
is_superduper_admin('undefined') -> 'false';
is_superduper_admin(<<_/binary>> = AccountId) ->
    lager:debug("checking for superduper admin: ~s", [AccountId]),
    case kz_account:fetch(AccountId) of
        {'ok', JObj} ->
            case kz_account:is_superduper_admin(JObj) of
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
    end;
is_superduper_admin(Context) ->
    is_superduper_admin(cb_context:auth_account_id(Context)).

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
                          case kz_util:is_empty(A) of
                              'true' -> kz_util:to_hex_binary(crypto:rand_bytes(16));
                              'false' -> A
                          end
                  end
                  ,fun(A) ->
                           case kz_util:is_empty(filename:extension(A)) of
                               'false' -> A;
                               'true' ->
                                   <<A/binary, ".", (kz_mime:to_extension(CT))/binary>>
                           end
                   end
                 ],
    lists:foldl(fun(F, A) -> F(A) end, Filename, Generators).

-spec parse_media_type(ne_binary()) ->
                              {'error', 'badarg'} |
                              media_values().
parse_media_type(MediaType) ->
    cowboy_http:nonempty_list(MediaType, fun cowboy_http:media_range/2).

-spec bucket_name(cb_context:context()) -> ne_binary().
-spec bucket_name(api(binary()), api(binary())) -> ne_binary().
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
-spec token_cost(cb_context:context(), non_neg_integer() | kz_json:key() | kz_json:keys()) -> non_neg_integer().
-spec token_cost(cb_context:context(), non_neg_integer(), kz_json:keys()) -> non_neg_integer().

token_cost(Context) ->
    token_cost(Context, 1).

token_cost(Context, <<_/binary>> = Suffix) ->
    token_cost(Context, 1, [Suffix]);
token_cost(Context, [_|_]=Suffix) ->
    token_cost(Context, 1, Suffix);
token_cost(Context, Default) ->
    token_cost(Context, Default, []).

token_cost(Context, Default, Suffix) when is_integer(Default), Default >= 0 ->
    Costs = kapps_config:get(?CONFIG_CAT, <<"token_costs">>, 1),
    find_token_cost(Costs
                    ,Default
                    ,Suffix
                    ,cb_context:req_nouns(Context)
                    ,cb_context:req_verb(Context)
                    ,cb_context:account_id(Context)
                   ).

-spec find_token_cost(kz_json:object() | non_neg_integer()
                      ,non_neg_integer()
                      ,kz_json:keys()
                      ,req_nouns()
                      ,http_method()
                      ,api(binary())
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

-spec get_token_cost(kz_json:object(), non_neg_integer(), kz_json:keys()) ->
                            non_neg_integer().
get_token_cost(JObj, Default, Keys) ->
    case kz_json:get_first_defined(Keys, JObj) of
        'undefined' -> Default;
        V -> kz_util:to_integer(V)
    end.

%% @public
-spec take_sync_field(cb_context:context()) -> cb_context:context().
take_sync_field(Context) ->
    Doc = cb_context:doc(Context),
    ShouldSync = kz_json:is_true(<<"sync">>, Doc, 'false'),
    CleansedDoc = kz_json:delete_key(<<"sync">>, Doc),
    cb_context:setters(Context, [{fun cb_context:store/3, 'sync', ShouldSync}
                                 ,{fun cb_context:set_doc/2, CleansedDoc}
                                ]).

%% @public
-spec remove_plaintext_password(cb_context:context()) -> cb_context:context().
remove_plaintext_password(Context) ->
    Doc = kz_json:delete_keys(
            [<<"password">>,
             <<"confirm_password">>
            ], cb_context:doc(Context)
           ),
    cb_context:set_doc(Context, Doc).

-type assignment_updates() :: [{ne_binary(), knm_number:knm_number_return()}].

-spec apply_assignment_updates([{ne_binary(), api(binary())}]) ->
                                      assignment_updates().
apply_assignment_updates(Updates) ->
    [{DID, knm_number:assign_to_app(DID, Assign)}
     || {DID, Assign} <- Updates
    ].

-spec log_assignment_updates(assignment_updates()) -> 'ok'.
log_assignment_updates(Updates) ->
    _ = [log_assignment_update(Update) || Update <- Updates],
    'ok'.

-spec log_assignment_update({ne_binary(), knm_number:knm_number_return()}) -> 'ok'.
log_assignment_update({DID, {'ok', _Number}}) ->
    lager:debug("successfully updated ~s", [DID]);
log_assignment_update({DID, {'error', E}}) ->
    lager:debug("failed to update ~s: ~p", [DID, E]).
