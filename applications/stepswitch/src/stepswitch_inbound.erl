%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz
%%% @doc
%%% Handle route requests from carrier resources
%%% @end
%%%-------------------------------------------------------------------
-module(stepswitch_inbound).

-export([handle_req/2]).

-include("stepswitch.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    _ = kz_util:put_callid(JObj),
    'true' = kapi_route:req_v(JObj),
    case kz_json:get_ne_value(?CCV(<<"Account-ID">>), JObj) of
        'undefined' -> maybe_relay_request(JObj);
        AccountId ->
            lager:debug("fetch-id ~s already has account-id ~s, skipping."
                       ,[kapi_route:fetch_id(JObj), AccountId]
                       )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% handle a request inbound from offnet
%% @end
%%--------------------------------------------------------------------
-spec maybe_relay_request(kz_json:object()) -> 'ok'.
maybe_relay_request(JObj) ->
    Number = stepswitch_util:get_inbound_destination(JObj),
    case knm_number:lookup_account(Number) of
        {'error', _R} ->
            lager:info("unable to determine account for fetch-id ~s, ~s: ~p"
                      ,[kapi_route:fetch_id(JObj), Number, _R]);
        {'ok', _, NumberProps} ->
            lager:debug("running routines for number ~s, fetch-id : ~s"
                       ,[Number, kapi_route:fetch_id(JObj)]),
            Routines = [fun set_account_id/2
                       ,fun set_ignore_display_updates/2
                       ,fun set_inception/2
                       ,fun maybe_find_resource/2
                       ,fun maybe_format_destination/2
                       ,fun maybe_set_ringback/2
                       ,fun maybe_set_transfer_media/2
                       ,fun maybe_lookup_cnam/2
                       ,fun maybe_add_prepend/2
                       ,fun maybe_block_call/2
                       ,fun maybe_transition_port_in/2
                       ],
            _ = lists:foldl(fun(F, J) -> F(NumberProps, J) end
                           ,JObj
                           ,Routines
                           ),
            'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determine the e164 format of the inbound number
%% @end
%%--------------------------------------------------------------------
-spec set_account_id(knm_number_options:extra_options(), kz_json:object()) ->
                            kz_json:object().
set_account_id(NumberProps, JObj) ->
    AccountId = knm_number_options:account_id(NumberProps),
    kz_json:set_value(?CCV(<<"Account-ID">>), AccountId, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_ignore_display_updates(knm_number_options:extra_options(), kz_json:object()) ->
                                        kz_json:object().
set_ignore_display_updates(_, JObj) ->
    kz_json:set_value(?CCV(<<"Ignore-Display-Updates">>), <<"true">>, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_inception(knm_number_options:extra_options(), kz_json:object()) ->
                           kz_json:object().
set_inception(_, JObj) ->
    Request = kz_json:get_value(<<"Request">>, JObj),
    kz_json:set_value(?CCV(<<"Inception">>), Request, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_find_resource(knm_number_options:extra_options(), kz_json:object()) ->
                                 kz_json:object().
maybe_find_resource(_, JObj) ->
    case stepswitch_resources:reverse_lookup(JObj) of
        {'error', 'not_found'} -> JObj;
        {'ok', ResourceProps} ->
            Routines = [fun add_resource_id/2
                       ,fun maybe_add_t38_settings/2
                       ],
            lists:foldl(fun(F, J) ->  F(J, ResourceProps) end
                       ,JObj
                       ,Routines
                       )
    end.

-spec add_resource_id(kz_json:object(), kz_proplist()) -> kz_json:object().
add_resource_id(JObj, ResourceProps) ->
    ResourceId = props:get_value('resource_id', ResourceProps),
    kz_json:set_values([{?CCV(<<"Resource-ID">>), ResourceId}
                       ,{?CCV(<<"Global-Resource">>), props:get_is_true('global', ResourceProps)}
                       ,{?CCV(<<"Authorizing-Type">>), <<"resource">>}
                        %% TODO
                        %% we need to make sure that Authorizing-ID is used
                        %% with Authorizing-Type in ALL kapps
                        %% when this is done remove the comment below
                        %% ,{?CCV(<<"Authorizing-ID">>), ResourceId}
                       ]
                      ,JObj
                      ).

-spec maybe_add_t38_settings(kz_json:object(), kz_proplist()) -> kz_json:object().
maybe_add_t38_settings(JObj, ResourceProps) ->
    case props:get_value('fax_option', ResourceProps) of
        'true' ->
            kz_json:set_value(?CCV(<<"Resource-Fax-Option">>)
                             ,props:get_value('fax_option', ResourceProps)
                             ,JObj
                             );
        <<"auto">> ->
            kz_json:set_value(?CCV(<<"Resource-Fax-Option">>)
                             ,props:get_value('fax_option', ResourceProps)
                             ,JObj
                             );
        _ -> JObj
    end.

-spec maybe_format_destination(knm_number_options:extra_options(), kz_json:object()) ->
                                      kz_json:object().
maybe_format_destination(_, JObj) ->
    case kz_json:get_value(?CCV(<<"Resource-ID">>), JObj) of
        'undefined' -> JObj;
        ResourceId ->
            case stepswitch_resources:get_props(ResourceId) of
                'undefined' -> JObj;
                Resource ->
                    Formatters = props:get_value(<<"Formatters">>, Resource, kz_json:new()),
                    kz_formatters:apply(JObj, Formatters, 'inbound')
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_set_ringback(knm_number_options:extra_options(), kz_json:object()) ->
                                kz_json:object().
maybe_set_ringback(NumberProps, JObj) ->
    case knm_number_options:ringback_media_id(NumberProps) of
        'undefined' -> JObj;
        MediaId ->
            kz_json:set_value(?CCV(<<"Ringback-Media">>), MediaId, JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determine the e164 format of the inbound number
%% @end
%%--------------------------------------------------------------------
-spec maybe_set_transfer_media(knm_number_options:extra_options(), kz_json:object()) ->
                                      kz_json:object().
maybe_set_transfer_media(NumberProps, JObj) ->
    case knm_number_options:transfer_media_id(NumberProps) of
        'undefined' -> JObj;
        MediaId ->
            kz_json:set_value(?CCV(<<"Transfer-Media">>), MediaId, JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% build the JSON to set the custom channel vars with the calls
%% account and authorizing  ID
%% @end
%%--------------------------------------------------------------------
-spec maybe_lookup_cnam(knm_number_options:extra_options(), kz_json:object()) ->
                               kz_json:object().
maybe_lookup_cnam(NumberProps, JObj) ->
    case knm_number_options:inbound_cnam_enabled(NumberProps) of
        'false' -> JObj;
        'true' -> stepswitch_cnam:lookup(JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_add_prepend(knm_number_options:extra_options(), kz_json:object()) ->
                               kz_json:object().
maybe_add_prepend(NumberProps, JObj) ->
    case knm_number_options:prepend(NumberProps) of
        'undefined' -> JObj;
        Prepend -> kz_json:set_value(<<"Prepend-CID-Name">>, Prepend, JObj)
    end.

-spec maybe_block_call(knm_number_options:extra_options(), kz_json:object()) -> kz_json:object().
maybe_block_call(_, JObj) ->
    case is_blacklisted(JObj)
        orelse kz_privacy:should_block_anonymous(JObj)
    of
        true -> JObj;
        false -> relay_request(JObj)
    end,
    JObj.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% relay a route request once populated with the new properties
%% @end
%%--------------------------------------------------------------------
-spec relay_request(kz_json:object()) -> kz_json:object().
relay_request(JObj) ->
    kapi_route:publish_req(JObj),
    lager:debug("relaying route request ~s", [kapi_route:fetch_id(JObj)]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_transition_port_in(knm_number_options:extra_options(), kz_json:object()) -> any().
maybe_transition_port_in(NumberProps, JObj) ->
    case knm_number_options:has_pending_port(NumberProps) of
        'false' -> 'ok';
        'true' -> transition_port_in(knm_number_options:number(NumberProps), JObj)
    end.

-spec transition_port_in(ne_binary(), api_object()) -> any().
transition_port_in(Number, JObj) ->
    {ok, MasterAccountId} = kapps_util:get_master_account_id(),
    Comment = <<(?APP_NAME)/binary, "-", (?APP_VERSION)/binary, " automagic">>,
    Metadata = knm_port_request:transition_metadata(MasterAccountId, undefined, Comment),
    case knm_port_request:get(Number) of
        {'ok', PortReq} -> knm_port_request:transition_to_complete(PortReq, Metadata);
        _ ->
            Num = stepswitch_util:get_inbound_destination(JObj),
            {'ok', PortReq} = knm_port_request:get(Num),
            knm_port_request:transition_to_complete(PortReq, Metadata)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_blacklisted(kz_json:object()) -> boolean().
is_blacklisted(JObj) ->
    AccountId = kz_json:get_ne_value(?CCV(<<"Account-ID">>), JObj),
    case get_blacklists(AccountId) of
        {'error', _R} ->
            lager:debug("not blacklisted ~p", [_R]),
            'false';
        {'ok', Blacklists} ->
            Blacklist = get_blacklist(AccountId, Blacklists),
            is_number_blacklisted(Blacklist, JObj)
    end.

-spec is_number_blacklisted(kz_json:object(), kz_json:object()) -> boolean().
is_number_blacklisted(Blacklist, JObj) ->
    Number = kz_json:get_value(<<"Caller-ID-Number">>, JObj),
    Normalized = knm_converters:normalize(Number),
    case kz_json:get_value(Normalized, Blacklist) =/= 'undefined'
        orelse (kz_privacy:is_anonymous(JObj)
                andalso kz_json:is_true(<<"should_block_anonymous">>, Blacklist)
               )
    of
        'false' -> false;
        'true' ->
            lager:info("~s(~s) is blacklisted", [Number, Normalized]),
            'true'
    end.

-spec get_blacklists(ne_binary()) ->
                            {'ok', ne_binaries()} |
                            {'error', any()}.
get_blacklists(AccountId) ->
    case kz_account:fetch(AccountId) of
        {'error', _R}=E ->
            lager:error("could not open account doc ~s : ~p", [AccountId, _R]),
            E;
        {'ok', Doc} ->
            case kz_json:get_value(<<"blacklists">>, Doc, []) of
                [] -> {'error', 'undefined'};
                [_|_]=Blacklists-> {'ok', Blacklists};
                _ -> {'error', 'miss_configured'}
            end
    end.

-spec get_blacklist(ne_binary(), ne_binaries()) -> kz_json:object().
get_blacklist(AccountId, Blacklists) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    lists:foldl(
      fun(BlacklistId, Acc) ->
              case kz_datamgr:open_cache_doc(AccountDb, BlacklistId) of
                  {'error', _R} ->
                      lager:error("could not open ~s in ~s: ~p", [BlacklistId, AccountDb, _R]),
                      Acc;
                  {'ok', Doc} ->
                      Numbers = kz_json:get_value(<<"numbers">>, Doc, kz_json:new()),
                      BlackList = maybe_set_block_anonymous(Numbers, kz_json:is_true(<<"should_block_anonymous">>, Doc)),
                      kz_json:merge_jobjs(Acc, BlackList)
              end
      end
               ,kz_json:new()
               ,Blacklists
     ).

-spec maybe_set_block_anonymous(kz_json:object(), boolean()) -> kz_json:object().
maybe_set_block_anonymous(JObj, 'false') -> JObj;
maybe_set_block_anonymous(JObj, 'true') ->
    kz_json:set_value(<<"should_block_anonymous">>, 'true', JObj).
