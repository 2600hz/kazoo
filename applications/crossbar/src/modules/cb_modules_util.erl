%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
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
maybe_originate_quickcall(#cb_context{}=Context) ->
    Call = create_call_from_context(Context),
    case get_endpoints(Call, Context) of
        [] ->
            cb_context:add_system_error('unspecified_fault', Context);
        Endpoints ->
            originate_quickcall(Endpoints, Call, default_bleg_cid(Call, Context))
    end.

create_call_from_context(#cb_context{account_id=AccountId
                                     ,db_name=AccountDb
                                     ,doc=JObj
                                    }=Context) ->
    Routines = [fun(C) -> whapps_call:set_account_db(AccountDb, C) end
                ,fun(C) -> whapps_call:set_account_id(AccountId, C) end
                ,fun(C) -> whapps_call:set_inception(<<"on-net">>, C) end
                ,fun(C) ->
                         case wh_json:get_ne_value(<<"owner_id">>, JObj) of
                             'undefined' -> C;
                             OwnerId -> whapps_call:set_owner_id(OwnerId, C)
                         end
                 end
                | request_specific_extraction_funs(Context)
               ],
    lists:foldl(fun(F, C) -> F(C) end, whapps_call:new(), Routines).

request_specific_extraction_funs(#cb_context{req_nouns=?DEVICES_QCALL_NOUNS}) ->
    [fun(C) -> whapps_call:set_authorizing_id(_DeviceId, C) end
     ,fun(C) -> whapps_call:set_authorizing_type(<<"device">>, C) end
     ,fun(C) -> whapps_call:set_request(<<_Number/binary, "@devicequickcall">>, C) end
     ,fun(C) -> whapps_call:set_to(<<_Number/binary, "@devicequickcall">>, C) end
    ];
request_specific_extraction_funs(#cb_context{req_nouns=?USERS_QCALL_NOUNS}) ->
    [fun(C) -> whapps_call:set_authorizing_id(_UserId, C) end
     ,fun(C) -> whapps_call:set_authorizing_type(<<"user">>, C) end
     ,fun(C) -> whapps_call:set_request(<<_Number/binary, "@userquickcall">>, C) end
     ,fun(C) -> whapps_call:set_to(<<_Number/binary, "@userquickcall">>, C) end
    ];
request_specific_extraction_funs(_) ->
    [].

get_endpoints(Call, #cb_context{doc=JObj, req_nouns=?DEVICES_QCALL_NOUNS}) ->
    Properties = wh_json:from_list([{<<"can_call_self">>, 'true'}
                                    ,{<<"suppress_clid">>, 'true'}
                                    ,{<<"source">>, 'cb_devices'}
                                   ]),
    case cf_endpoint:build(JObj, Properties, aleg_cid(_Number, Call)) of
        {'error', _} -> [];
        {'ok', []} -> [];
        {'ok', Endpoints} -> Endpoints
    end;
get_endpoints(Call, #cb_context{req_nouns=?USERS_QCALL_NOUNS}) ->
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
get_endpoints(_, _) ->
    [].

aleg_cid(Number, Call) ->
    Routines = [fun(C) -> whapps_call:set_custom_channel_var(<<"Retain-CID">>, <<"true">>, C) end
                ,fun(C) -> whapps_call:set_caller_id_name(<<"QuickCall">>, C) end
                ,fun(C) -> whapps_call:set_caller_id_number(wh_util:to_binary(Number), C) end
               ],
    lists:foldl(fun(F, C) -> F(C) end, Call, Routines).

default_bleg_cid(Call, #cb_context{query_json=JObj}=Context) ->
    {CIDNumber, CIDName} = cf_attributes:caller_id(<<"external">>, Call),
    Defaults = wh_json:from_list([{<<"cid-name">>, CIDName}
                                  ,{<<"cid-number">>, CIDNumber}
                                 ]),
    Context#cb_context{query_json=wh_json:merge_jobjs(JObj, Defaults)}.

originate_quickcall(Endpoints, Call, #cb_context{account_id=AccountId
                                                 ,req_id=RequestId
                                                }=Context) ->
    CCVs = [{<<"Account-ID">>, AccountId}
            ,{<<"Retain-CID">>, <<"true">>}
            ,{<<"Inherit-Codec">>, <<"false">>}
            ,{<<"Inception">>, <<"on-net">>}
            ,{<<"Authorizing-Type">>, whapps_call:authorizing_type(Call)}
            ,{<<"Authorizing-ID">>, whapps_call:authorizing_id(Call)}
           ],
    MsgId = case wh_util:is_empty(RequestId) of
                'true' -> wh_util:rand_hex_binary(16);
                'false' -> wh_util:to_binary(RequestId)
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
    crossbar_util:response_202(<<"processing request">>, Context#cb_context{resp_data=Request}).

maybe_auto_answer([Endpoint]) ->
    [wh_json:set_value([<<"Custom-Channel-Vars">>, <<"Auto-Answer">>], <<"true">>, Endpoint)];
maybe_auto_answer(Endpoints) ->
    Endpoints.

get_application_data(#cb_context{req_nouns=?DEVICES_QCALL_NOUNS}) ->
    wh_json:from_list([{<<"Route">>, _Number}]);
get_application_data(#cb_context{req_nouns=?USERS_QCALL_NOUNS}) ->
    wh_json:from_list([{<<"Route">>, _Number}]);
get_application_data(_) ->
    wh_json:from_list([{<<"Route">>, <<"0">>}]).

get_timeout(#cb_context{query_json=JObj}) ->
    try wh_json:get_integer_value(<<"timeout">>, JObj, 30) of
        Timeout when Timeout > 3 -> wh_util:to_binary(Timeout);
        _ -> <<"30">>
    catch
        _:_ -> <<"30">>
    end.

get_ignore_early_media(#cb_context{query_json=JObj}) ->
    wh_util:to_binary(wh_json:is_true(<<"ignore-early-media">>, JObj)).

get_media(#cb_context{query_json=JObj}) ->
    case wh_json:get_value(<<"media">>, JObj) of
        <<"bypass">> -> <<"bypass">>;
        _Else -> <<"process">>
    end.

get_caller_id_name(#cb_context{query_json=JObj}) ->
    case wh_json:get_binary_value(<<"cid-name">>, JObj) of
        'undefined' -> 'undefined';
        CIDName -> wh_util:uri_decode(CIDName)
    end.

get_caller_id_number(#cb_context{query_json=JObj}) ->
    case wh_json:get_binary_value(<<"cid-number">>, JObj) of
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
is_superduper_admin('undefined') -> 'false';
is_superduper_admin(#cb_context{auth_account_id=AccountId}) ->
    is_superduper_admin(AccountId);
is_superduper_admin(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'ok', JObj} ->
            %% more logging was called for
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
