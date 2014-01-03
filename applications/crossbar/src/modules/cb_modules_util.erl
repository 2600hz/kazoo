%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
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
        ]).

-include("../crossbar.hrl").

-spec pass_hashes(ne_binary(), ne_binary()) -> {ne_binary(), ne_binary()}.
pass_hashes(Username, Password) ->
    Creds = list_to_binary([Username, ":", Password]),
    SHA1 = wh_util:to_hex_binary(crypto:sha(Creds)),
    MD5 = wh_util:to_hex_binary(erlang:md5(Creds)),
    {MD5, SHA1}.

-spec update_mwi('undefined' | ne_binary(), ne_binary()) -> pid().
update_mwi(OwnerId, AccountDb) ->
    spawn(fun() ->
                  timer:sleep(1000),
                  cf_util:unsolicited_owner_mwi_update(AccountDb, OwnerId)
          end).

-spec get_devices_owned_by(ne_binary(), ne_binary()) -> wh_json:objects().
get_devices_owned_by(OwnerID, DB) ->
    case couch_mgr:get_results(DB, <<"cf_attributes/owned">>, [{'key', [OwnerID, <<"device">>]}
                                                               ,'include_docs'
                                                              ]) of
        {'ok', JObjs} ->
            lager:debug("Found ~b devices owned by ~s", [length(JObjs), OwnerID]),
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
    Routines = [fun(C) -> whapps_call:set_account_db(cb_context:account_db(Context), C) end
                ,fun(C) -> whapps_call:set_account_id(cb_context:account_id(Context), C) end
                ,fun(C) -> whapps_call:set_inception(<<"on-net">>, C) end
                ,fun(C) ->
                         case wh_json:get_ne_value(<<"owner_id">>, cb_context:doc(Context)) of
                             'undefined' -> C;
                             OwnerId -> whapps_call:set_owner_id(OwnerId, C)
                         end
                 end
                | request_specific_extraction_funs(Context)
               ],
    lists:foldl(fun(F, C) -> F(C) end, whapps_call:new(), Routines).

-spec request_specific_extraction_funs(cb_context:context()) -> [function(),...] | [].
-spec request_specific_extraction_funs_from_nouns(req_nouns()) -> [function(),...] | [].
request_specific_extraction_funs(Context) ->
    request_specific_extraction_funs_from_nouns(cb_context:req_nouns(Context)).

request_specific_extraction_funs_from_nouns(?DEVICES_QCALL_NOUNS) ->
    [fun(C) -> whapps_call:set_authorizing_id(_DeviceId, C) end
     ,fun(C) -> whapps_call:set_authorizing_type(<<"device">>, C) end
     ,fun(C) -> whapps_call:set_request(<<_Number/binary, "@devicequickcall">>, C) end
     ,fun(C) -> whapps_call:set_to(<<_Number/binary, "@devicequickcall">>, C) end
    ];
request_specific_extraction_funs_from_nouns(?USERS_QCALL_NOUNS) ->
    [fun(C) -> whapps_call:set_authorizing_id(_UserId, C) end
     ,fun(C) -> whapps_call:set_authorizing_type(<<"user">>, C) end
     ,fun(C) -> whapps_call:set_request(<<_Number/binary, "@userquickcall">>, C) end
     ,fun(C) -> whapps_call:set_to(<<_Number/binary, "@userquickcall">>, C) end
    ];
request_specific_extraction_funs_from_nouns(_ReqNouns) ->
    [].

-spec get_endpoints(whapps_call:call(), cb_context:context()) -> wh_json:objects().
-spec get_endpoints(whapps_call:call(), cb_context:context(), req_nouns()) -> wh_json:objects().
get_endpoints(Call, Context) ->
    get_endpoints(Call, Context, cb_context:req_nouns(Context)).

get_endpoints(Call, Context, ?DEVICES_QCALL_NOUNS) ->
    Properties = wh_json:from_list([{<<"can_call_self">>, 'true'}
                                    ,{<<"suppress_clid">>, 'true'}
                                    ,{<<"source">>, 'cb_devices'}
                                   ]),
    case cf_endpoint:build(cb_context:doc(Context), Properties, aleg_cid(_Number, Call)) of
        {'error', _} -> [];
        {'ok', []} -> [];
        {'ok', Endpoints} -> Endpoints
    end;
get_endpoints(Call, _Context, ?USERS_QCALL_NOUNS) ->
    Properties = wh_json:from_list([{<<"can_call_self">>, 'true'}
                                    ,{<<"suppress_clid">>, 'true'}
                                    ,{<<"source">>, 'cb_users'}
                                   ]),
    lists:foldr(fun(EndpointId, Acc) ->
                        case cf_endpoint:build(EndpointId, Properties, aleg_cid(_Number, Call)) of
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
            ,{<<"Inception">>, <<"on-net">>}
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
               | wh_api:default_headers(<<>>, <<"resource">>, <<"originate_req">>, ?APP_NAME, ?APP_VERSION)
              ],
    wapi_resource:publish_originate_req(props:filter_undefined(Request)),
    crossbar_util:response_202(<<"processing request">>, cb_context:set_resp_data(Context, Request)).

-spec maybe_auto_answer(wh_json:objects()) -> wh_json:objects().
maybe_auto_answer([Endpoint]) ->
    [wh_json:set_value([<<"Custom-Channel-Vars">>, <<"Auto-Answer">>], <<"true">>, Endpoint)];
maybe_auto_answer(Endpoints) ->
    Endpoints.

-spec get_application_data(cb_context:context()) -> wh_json:object().
-spec get_application_data_from_nouns(req_nouns()) -> wh_json:object().
get_application_data(Context) ->
    get_application_data_from_nouns(cb_context:req_nouns(Context)).

get_application_data_from_nouns(?DEVICES_QCALL_NOUNS) ->
    wh_json:from_list([{<<"Route">>, _Number}]);
get_application_data_from_nouns(?USERS_QCALL_NOUNS) ->
    wh_json:from_list([{<<"Route">>, _Number}]);
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
is_superduper_admin(AccountId) when is_binary(AccountId) ->
    is_superduper_admin(AccountId, wh_util:format_account_id(AccountId, 'encoded'));
is_superduper_admin(Context) ->
    is_superduper_admin(cb_context:account_id(Context), cb_context:account_db(Context)).

is_superduper_admin(AccountId, AccountDb) ->
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
        {'error', _} ->
            lager:debug("not authorizing, error during lookup"),
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
content_type_to_extension(<<"application/pdf">>) -> <<"pdf">>.
