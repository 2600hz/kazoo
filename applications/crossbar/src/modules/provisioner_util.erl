%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc Common functions for the provisioner modules
%%% @author Karl Anderson
%%% @end
%%%-----------------------------------------------------------------------------
-module(provisioner_util).

-include("crossbar.hrl").

-export([maybe_provision/1]).
-export([maybe_delete_provision/1]).
-export([maybe_update_account/1]).
-export([maybe_delete_account/1]).
-export([maybe_send_contact_list/1]).
-export([get_provision_defaults/1]).
-export([is_mac_address_in_use/2]).
-export([maybe_sync_sip_data/2]).
-export([cleanse_mac_address/1]).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".devices">>).
-define(PROVISIONER_CONFIG, <<"provisioner">>).
-define(TEMPLATE_ATTCH, <<"template">>).

-define(BASE_HEADERS
       ,props:filter_undefined(
          [{"host", kapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioning_host">>)}
          ,{"referer", kapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioning_referer">>)}
          ,{"user-agent", kz_term:to_list(erlang:node())}
          ])).
-define(JSON_HEADERS, [{"content-type", "application/json"} | ?BASE_HEADERS]).
-define(FORM_HEADERS, [{"content-type", "application/x-www-form-urlencoded"} | ?BASE_HEADERS]).

-spec get_mac_address(cb_context:context()) -> kz_term:api_binary().
get_mac_address(Context) ->
    MACAddress = kz_json:get_ne_binary_value(<<"mac_address">>, cb_context:doc(Context)),
    cleanse_mac_address(MACAddress).

-spec get_old_mac_address(cb_context:context()) -> kz_term:api_binary().
get_old_mac_address(Context) ->
    MACAddress =
        case cb_context:fetch(Context, 'db_doc') of
            'undefined' -> 'undefined';
            JObj ->
                kz_json:get_ne_value(<<"mac_address">>, JObj)
        end,
    cleanse_mac_address(MACAddress).

-spec cleanse_mac_address(kz_term:api_ne_binary()) -> kz_term:api_ne_binary().
cleanse_mac_address('undefined') -> 'undefined';
cleanse_mac_address(MACAddress) ->
    ReOptions = ['global'
                ,{'return','binary'}
                ],
    MAC = kz_term:to_lower_binary(
            re:replace(MACAddress, <<"[^0-9a-fA-F]">>, <<>>, ReOptions)),
    case kz_term:is_empty(MAC) of
        false -> MAC;
        true -> undefined
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec maybe_provision(cb_context:context()) -> boolean().
maybe_provision(Context) ->
    maybe_provision(Context, cb_context:resp_status(Context)).

-spec maybe_provision(cb_context:context(), crossbar_status()) -> boolean().
maybe_provision(Context, 'success') ->
    MACAddress = get_mac_address(Context),
    case MACAddress =/= 'undefined'
        andalso get_provisioning_type()
    of
        <<"super_awesome_provisioner">> ->
            _ = do_full_provisioner_provider(Context),
            _ = do_full_provision(MACAddress, Context),
            'true';
        <<"awesome_provisioner">> ->
            _ = do_awesome_provision(Context),
            'true';
        <<"simple_provisioner">>  ->
            _ = do_simple_provision(MACAddress, Context),
            'true';
        <<"provisioner_v5">>  ->
            _ = maybe_provision_v5(Context, cb_context:req_verb(Context)),
            'true';
        _ -> 'false'
    end;
maybe_provision(_Context, _Status) -> 'false'.

-spec maybe_provision_v5(cb_context:context(), kz_term:ne_binary()) -> 'ok'.
maybe_provision_v5(Context, ?HTTP_PUT) ->
    AuthToken = cb_context:auth_token(Context),
    NewDevice = cb_context:doc(Context),
    _ = provisioner_v5:update_device(NewDevice, AuthToken),
    'ok';

maybe_provision_v5(Context, ?HTTP_POST) ->
    AuthToken = cb_context:auth_token(Context),
    NewDevice = cb_context:doc(Context),
    NewAddress = cb_context:req_value(Context, <<"mac_address">>),
    OldDevice = cb_context:fetch(Context, 'db_doc'),
    OldAddress = kz_json:get_ne_value(<<"mac_address">>, OldDevice),
    case NewAddress =:= OldAddress of
        'true' ->
            _ = provisioner_v5:update_device(NewDevice, AuthToken);
        'false' ->
            NewDevice1 = kz_json:set_value(<<"mac_address">>, OldAddress, NewDevice),
            _ = provisioner_v5:delete_device(NewDevice1, AuthToken),
            _ = provisioner_v5:update_device(NewDevice, AuthToken)
    end,
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec maybe_delete_provision(cb_context:context()) -> boolean().
maybe_delete_provision(Context) ->
    maybe_delete_provision(Context, cb_context:resp_status(Context)).

-spec maybe_delete_provision(cb_context:context(), crossbar_status()) -> boolean().
maybe_delete_provision(Context, 'success') ->
    MACAddress = get_mac_address(Context),
    case MACAddress =/= 'undefined'
        andalso get_provisioning_type()
    of
        <<"super_awesome_provisioner">> ->
            _ = delete_full_provision(MACAddress, Context),
            'true';
        <<"provisioner_v5">>  ->
            _ = provisioner_v5:delete_device(cb_context:doc(Context)
                                            ,cb_context:auth_token(Context)
                                            ),
            'true';
        _ ->
            'false'
    end;
maybe_delete_provision(_Context, _Status) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update_account(cb_context:context()) -> boolean().
maybe_update_account(Context) ->
    case cb_context:is_context(Context)
        andalso get_provisioning_type()
    of
        'false' -> 'false';
        <<"provisioner_v5">> ->
            _ = provisioner_v5:update_account(cb_context:account_id(Context)
                                             ,cb_context:doc(Context)
                                             ,cb_context:auth_token(Context)
                                             ),
            'true';
        _ -> 'false'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_delete_account(cb_context:context()) -> boolean().
maybe_delete_account(Context) ->
    case cb_context:is_context(Context)
        andalso get_provisioning_type()
    of
        'false' -> 'false';
        <<"super_awesome_provisioner">> ->
            _ = delete_account(Context),
            'true';
        <<"provisioner_v5">> ->
            _ = provisioner_v5:delete_account(cb_context:account_id(Context)
                                             ,cb_context:auth_token(Context)
                                             ),
            'true';
        _ -> 'false'
    end.

-spec maybe_send_contact_list(cb_context:context()) -> 'ok'.
maybe_send_contact_list(Context) ->
    maybe_send_contact_list(Context, cb_context:resp_status(Context)).

-spec maybe_send_contact_list(cb_context:context(), crossbar_status()) -> 'ok'.
maybe_send_contact_list(Context, 'success') ->
    case cb_context:is_context(Context)
        andalso get_provisioning_type()
    of
        <<"super_awesome_provisioner">> ->
            _ = do_full_provision_contact_list(Context),
            'ok';
        <<"provisioner_v5">> ->
            _ = provisioner_v5:update_user(cb_context:account_id(Context)
                                          ,cb_context:doc(Context)
                                          ,cb_context:auth_token(Context)
                                          ),
            'ok';
        _ -> 'ok'
    end;
maybe_send_contact_list(_Context, _Status) -> 'ok'.

-spec do_full_provisioner_provider(cb_context:context()) -> boolean().
do_full_provisioner_provider(Context) ->
    do_full_provision_contact_list(cb_context:account_id(Context)).

-spec do_full_provision_contact_list(kz_term:ne_binary() | cb_context:context()) -> boolean().
do_full_provision_contact_list(AccountId) when is_binary(AccountId) ->
    case kzd_accounts:fetch(AccountId) of
        {'ok', JObj} ->
            Routines = [fun kz_doc:public_fields/1
                       ,fun(J) ->
                                ResellerId = kz_services_reseller:get_id(AccountId),
                                kz_json:set_value(<<"provider_id">>, ResellerId, J)
                        end
                       ,fun(J) -> kz_json:delete_key(<<"available_apps">>, J) end
                       ,fun(J) ->
                                AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
                                ContactList = provisioner_contact_list:build(AccountDb),
                                kz_json:set_value(<<"directory">>, ContactList, J)
                        end
                       ],
            Provider = lists:foldl(fun(F, J) -> F(J) end, JObj, Routines),
            PartialURL = <<AccountId/binary, "/">>,
            maybe_send_to_full_provisioner(PartialURL, Provider);
        {'error', _R} ->
            lager:warning("failed to get account definition for ~s: ~p", [AccountId, _R]),
            'false'
    end;
do_full_provision_contact_list(Context) ->
    case should_build_contact_list(Context) of
        'false' -> 'false';
        'true' -> do_full_provision_contact_list(cb_context:account_id(Context))
    end.

-spec should_build_contact_list(cb_context:context()) -> boolean().
should_build_contact_list(Context) ->
    OriginalJObj = cb_context:fetch(Context, 'db_doc'),
    JObj = cb_context:doc(Context),
    case kz_json:is_json_object(OriginalJObj) of
        'false' ->
            kz_doc:type(JObj) =:= <<"callflow">>;
        'true' ->
            kz_doc:type(JObj) =:= <<"callflow">>
                orelse kz_json:get_value(<<"name">>, JObj) =/=  kz_json:get_value(<<"name">>, OriginalJObj)
                orelse kz_json:get_value(<<"first_name">>, JObj) =/=  kz_json:get_value(<<"first_name">>, OriginalJObj)
                orelse kz_json:get_value(<<"last_name">>, JObj) =/=  kz_json:get_value(<<"last_name">>, OriginalJObj)
                orelse kz_json:get_value([<<"contact_list">>, <<"exclude">>], JObj) =/=
                kz_json:get_value([<<"contact_list">>, <<"exclude">>], OriginalJObj)
    end.

%%------------------------------------------------------------------------------
%% @doc This doesn't belong here, needs to be in an external library. Make request to
%% get provisioning defaults
%% @end
%%------------------------------------------------------------------------------
-spec get_provision_defaults(cb_context:context()) -> cb_context:context().
get_provision_defaults(Context) ->
    JObj = cb_context:doc(Context),

    Brand   = kz_http_util:urlencode(kz_json:get_string_value([<<"properties">>, <<"brand">>], JObj)),
    Model   = kz_http_util:urlencode(kz_json:get_string_value([<<"properties">>, <<"model">>], JObj)),
    Product = kz_http_util:urlencode(kz_json:get_string_value([<<"properties">>, <<"product">>], JObj)),

    Url = [kapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioning_url">>)
          ,"?request=data"
          ,"&brand=", kz_term:to_list(Brand)
          ,"&model=", kz_term:to_list(Model)
          ,"&product=", kz_term:to_list(Product)
          ],
    UrlString = lists:flatten(Url),
    lager:debug("attempting to pull provisioning configs from ~s", [UrlString]),
    case kz_http:get(UrlString, ?BASE_HEADERS) of
        {'ok', 200, _, Response} ->
            lager:debug("great success, acquired provisioning template"),
            JResp = kz_json:decode(Response),
            cb_context:setters(Context
                              ,[{fun cb_context:set_doc/2, kz_json:set_value(<<"template">>, JResp, JObj)}
                               ,{fun cb_context:set_resp_status/2, 'success'}
                               ]);
        {'ok', Status, _, _} ->
            lager:debug("could not get provisioning template defaults: ~p", [Status]),
            crossbar_util:response('error', <<"Error retrieving content from external site">>, 500, Context);
        {'error', _R} ->
            lager:debug("could not get provisioning template defaults: ~p", [_R]),
            crossbar_util:response('error', <<"Error retrieving content from external site">>, 500, Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_mac_address_in_use(cb_context:context(), kz_term:ne_binary()) -> boolean().
is_mac_address_in_use(Context, MacAddress) ->
    case cb_context:is_context(Context)
        andalso get_provisioning_type()
    of
        <<"provisioner_v5">> ->
            AuthToken = cb_context:auth_token(Context),
            %% Note: following call will take a "long" time
            'false' =/= provisioner_v5:check_MAC(MacAddress, AuthToken);
        _ -> 'false'
    end.

%%------------------------------------------------------------------------------
%% @doc post data to a provisioning server
%% @end
%%------------------------------------------------------------------------------
-spec do_simple_provision(kz_term:ne_binary(), cb_context:context()) -> boolean().
do_simple_provision(MACAddress, Context) ->
    JObj = cb_context:doc(Context),
    case kapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioning_url">>) of
        'undefined' -> 'false';
        Url ->
            AccountRealm = kzd_accounts:fetch_realm(cb_context:account_id(Context)),
            Body =
                kz_json:from_list(
                  [{<<"device[mac]">>, MACAddress}
                  ,{<<"device[label]">>, kz_json:get_ne_binary_value(<<"name">>, JObj)}
                  ,{<<"sip[realm]">>, kzd_devices:sip_realm(JObj, AccountRealm)}
                  ,{<<"sip[username]">>, kzd_devices:sip_username(JObj)}
                  ,{<<"sip[password]">>, kzd_devices:sip_password(JObj)}
                  ,{<<"submit">>, <<"true">>}
                  ]),
            Encoded = kz_http_util:json_to_querystring(Body),
            lager:debug("posting to ~s with: ~-300s", [Url, Encoded]),
            Res = kz_http:post(Url, ?FORM_HEADERS, Encoded),
            lager:debug("response from server: ~p", [Res]),
            'true'
    end.

%%------------------------------------------------------------------------------
%% @doc post data to a provisioning server
%% @end
%%------------------------------------------------------------------------------
-spec delete_account(kz_term:ne_binary() | cb_context:context()) -> boolean().
delete_account(<<_/binary>> = AccountId) ->
    maybe_send_to_full_provisioner(AccountId);
delete_account(Context) ->
    delete_account(cb_context:account_id(Context)).


-spec delete_full_provision(kz_term:ne_binary(), cb_context:context()) -> boolean().
delete_full_provision(MACAddress, Context) ->
    {'ok', Context1} = get_merged_device(MACAddress, Context),
    AccountId = kz_json:get_binary_value(<<"account_id">>, cb_context:doc(Context1)),
    PartialURL = <<AccountId/binary, "/", MACAddress/binary>>,
    maybe_send_to_full_provisioner(PartialURL).

-spec do_full_provision(kz_term:ne_binary(), cb_context:context()) -> boolean().
do_full_provision(MACAddress, Context) ->
    {'ok', Context1} = get_merged_device(MACAddress, Context),
    OldMACAddress = get_old_mac_address(Context),
    _ = case OldMACAddress =/= MACAddress
            andalso OldMACAddress =/= 'undefined'
        of
            'true' -> delete_full_provision(OldMACAddress, Context);
            _ -> 'ok'
        end,
    JObj = cb_context:doc(Context1),
    AccountId = kz_json:get_binary_value(<<"account_id">>, JObj),
    PartialURL = <<AccountId/binary, "/", MACAddress/binary>>,
    maybe_send_to_full_provisioner(PartialURL, JObj).

-spec maybe_send_to_full_provisioner(kz_term:ne_binary()) -> boolean().
maybe_send_to_full_provisioner(PartialURL) ->
    case kapps_config:get_binary(?MOD_CONFIG_CAT, <<"provisioning_url">>) of
        'undefined' -> 'false';
        Url ->
            FullUrl = kz_term:to_lower_string(<<Url/binary, "/", PartialURL/binary>>),
            send_to_full_provisioner(FullUrl)
    end.

-spec maybe_send_to_full_provisioner(kz_term:ne_binary(), kz_json:object()) -> boolean().
maybe_send_to_full_provisioner(PartialURL, JObj) ->
    case kapps_config:get_binary(?MOD_CONFIG_CAT, <<"provisioning_url">>) of
        'undefined' -> 'false';
        Url ->
            FullUrl = kz_term:to_lower_string(<<Url/binary, "/", PartialURL/binary>>),
            {'ok', _, _, RawJObj} = kz_http:get(FullUrl, ?JSON_HEADERS, [{'timeout', 10 * ?MILLISECONDS_IN_SECOND}]),
            case kz_json:get_integer_value([<<"error">>, <<"code">>], kz_json:decode(RawJObj)) of
                'undefined' -> send_to_full_provisioner('post', FullUrl, JObj);
                404 -> send_to_full_provisioner('put', FullUrl, JObj);
                _ -> 'false'
            end
    end.

-spec send_to_full_provisioner(string()) -> boolean().
send_to_full_provisioner(FullUrl) ->
    lager:debug("making ~s request to ~s", ['delete', FullUrl]),
    Res = kz_http:delete(FullUrl, ?JSON_HEADERS, [], [{'timeout', 10 * ?MILLISECONDS_IN_SECOND}]),
    lager:debug("response from server: ~p", [Res]),
    'true'.

-spec send_to_full_provisioner('put' | 'post', string(), kz_json:object()) -> boolean().
send_to_full_provisioner('put', FullUrl, JObj) ->
    Body = kz_term:to_list(kz_json:encode(JObj)),
    lager:debug("making put request to ~s with: ~-300p", [FullUrl, Body]),
    Res = kz_http:put(FullUrl, ?JSON_HEADERS, Body, [{'timeout', 10 * ?MILLISECONDS_IN_SECOND}]),
    lager:debug("response from server: ~p", [Res]),
    'true';
send_to_full_provisioner('post', FullUrl, JObj) ->
    J = kz_json:from_list(
          [{<<"provider_id">>, kz_json:get_value(<<"provider_id">>, JObj)}
          ,{<<"name">>, kz_json:get_value(<<"name">>, JObj)}
          ,{<<"settings">>, JObj}
          ]),
    Body = kz_term:to_list(kz_json:encode(J)),
    lager:debug("making post request to ~s with: ~-300p", [FullUrl, Body]),
    Res = kz_http:post(FullUrl, ?JSON_HEADERS, Body, [{'timeout', 10 * ?MILLISECONDS_IN_SECOND}]),
    lager:debug("response from server: ~p", [Res]),
    'true'.

-spec do_awesome_provision(cb_context:context()) -> boolean().
do_awesome_provision(Context) ->
    case get_template(Context) of
        {'error', _} -> 'false';
        {'ok', JObj} ->
            send_provisioning_template(JObj, Context),
            'true'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_merged_device(kz_term:ne_binary(), cb_context:context()) ->
                               {'ok', cb_context:context()}.
get_merged_device(MACAddress, Context) ->
    {'ok', Data} = merge_device(MACAddress, Context),
    {'ok', cb_context:set_doc(Context, Data)}.

-spec merge_device(kz_term:ne_binary(), cb_context:context()) ->
                          {'ok', kz_json:object()}.
merge_device(MACAddress, Context) ->
    JObj = cb_context:doc(Context),
    AccountId = cb_context:account_id(Context),

    Routines = [fun(J) -> kz_json:set_value(<<"mac_address">>, MACAddress, J) end
               ,fun(J) ->
                        OwnerId = kz_json:get_ne_value(<<"owner_id">>, JObj),
                        Owner = get_owner(OwnerId, AccountId),
                        kz_json:merge(J, Owner)
                end
               ,fun(J) -> kz_json:delete_key(<<"apps">>, J) end
               ,fun(J) -> kz_json:set_value(<<"account_id">>, AccountId, J) end
               ],
    MergedDevice = lists:foldl(fun(F, J) -> F(J) end, JObj, Routines),
    {'ok', kz_doc:public_fields(MergedDevice)}.

-spec get_owner(kz_term:api_binary(), kz_term:ne_binary()) -> kz_json:object().
get_owner('undefined', _) -> kz_json:new();
get_owner(OwnerId, AccountId) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:open_cache_doc(AccountDb, OwnerId) of
        {'ok', Owner} -> Owner;
        {'error', _R} ->
            lager:debug("unable to open user definition ~s/~s: ~p", [AccountDb, OwnerId, _R]),
            kz_json:new()
    end.

%%------------------------------------------------------------------------------
%% @doc Do awesome provisioning
%% @end
%%------------------------------------------------------------------------------
send_provisioning_template(JObj, Context) ->
    %% TODO: theoretically this is the start of multiple line support....
    Line = <<"lineloop|line_1">>,
    MAC = re:replace(kz_json:get_string_value(<<"mac_address">>, cb_context:doc(Context), "")
                    ,"[^0-9a-fA-F]", "", [{'return', 'list'}, 'global']
                    ),
    LineGenerators = [fun set_device_line_defaults/1
                     ,fun set_account_line_defaults/1
                     ],
    TmplGenerators = [fun set_account_id/1
                     ,fun set_account_overrides/1
                     ,fun set_user_overrides/1
                     ,fun set_device_overrides/1
                     ,fun set_global_overrides/1
                     ],
    LineUpdaters = lists:foldr(fun(F, U) -> F(Context) ++ U end, [], LineGenerators),
    TmplUpdaters = lists:foldr(fun(F, U) -> F(Context) ++ U end, [], TmplGenerators),
    DefaultTemplate = lists:foldr(fun(F, J) -> F(J) end, JObj, TmplUpdaters),
    LineLoop = kz_json:get_value([<<"data">>, <<"globals">>, <<"globals">>, Line], DefaultTemplate),
    LineTemplate = lists:foldr(fun(F, J) -> F(J) end, LineLoop, LineUpdaters),

    Template = kz_json:set_value([<<"data">>, <<"globals">>, <<"globals">>, Line], LineTemplate, DefaultTemplate),
    send_provisioning_request(Template, MAC).

%%------------------------------------------------------------------------------
%% @doc If the device specifies a local template id then return that
%% template
%% @end
%%------------------------------------------------------------------------------
-spec get_template(cb_context:context()) ->
                          {'ok', kz_json:object()} |
                          {'error', any()}.
get_template(Context) ->
    DocId = kz_json:get_value([<<"provision">>, <<"id">>], cb_context:doc(Context)),
    case is_binary(DocId)
        andalso kz_datamgr:fetch_attachment(cb_context:account_db(Context), DocId, ?TEMPLATE_ATTCH)
    of
        'false' ->
            lager:debug("unknown template id ~s", [DocId]),
            {'error', 'not_found'};
        {'error', _R}=E ->
            lager:debug("could not fetch template doc ~s: ~p", [DocId, _R]),
            E;
        {'ok', Attachment} ->
            {'ok', kz_json:decode(Attachment)}
    end.

%%------------------------------------------------------------------------------
%% @doc add the account_id to the root of the provisioning json
%% @end
%%------------------------------------------------------------------------------
-spec set_account_id(cb_context:context()) ->
                            [fun((kz_json:object()) -> kz_json:object()),...].
set_account_id(Context) ->
    AccountId = cb_context:auth_account_id(Context),
    [fun(J) -> kz_json:set_value(<<"account_id">>, AccountId, J) end].

%%------------------------------------------------------------------------------
%% @doc get the settings from the account doc that should be used in the
%% base properties for the line
%% @end
%%------------------------------------------------------------------------------
-spec set_account_line_defaults(cb_context:context()) ->
                                       [fun((kz_json:object()) -> kz_json:object()),...].
set_account_line_defaults(Context) ->
    Account = case kzd_accounts:fetch(cb_context:account_id(Context)) of
                  {'ok', JObj} -> JObj;
                  {'error', _} -> kz_json:new()
              end,
    [fun(J) ->
             case kz_json:get_ne_value(<<"realm">>, Account) of
                 'undefined' -> J;
                 Value -> kz_json:set_value([<<"server_host">>, <<"value">>], Value, J)
             end
     end
    ,fun(J) ->
             case kz_json:get_ne_value(<<"name">>, Account) of
                 'undefined' -> J;
                 Value -> kz_json:set_value([<<"displayname">>, <<"value">>], Value, J)
             end
     end
    ].

%%------------------------------------------------------------------------------
%% @doc get the settings from the device doc that should be used in the
%% base properties for the line
%% @end
%%------------------------------------------------------------------------------
-spec set_device_line_defaults(cb_context:context()) ->
                                      [fun((kz_json:object()) -> kz_json:object()),...].
set_device_line_defaults(Context) ->
    Device = cb_context:doc(Context),
    [fun(J) ->
             case kzd_devices:sip_username(Device) of
                 'undefined' -> J;
                 Value -> kz_json:set_value([<<"authname">>, <<"value">>], Value, J)
             end
     end
    ,fun(J) ->
             case kzd_devices:sip_username(Device) of
                 'undefined' -> J;
                 Value -> kz_json:set_value([<<"username">>, <<"value">>], Value, J)
             end
     end
    ,fun(J) ->
             case kzd_devices:sip_password(Device) of
                 'undefined' -> J;
                 Value -> kz_json:set_value([<<"secret">>, <<"value">>], Value, J)
             end
     end
    ,fun(J) ->
             case kzd_devices:sip_realm(Device) of
                 'undefined' -> J;
                 Value -> kz_json:set_value([<<"server_host">>, <<"value">>], Value, J)
             end
     end
    ,fun(J) ->
             case kzd_devices:name(Device) of
                 'undefined' -> J;
                 Value -> kz_json:set_value([<<"displayname">>, <<"value">>], Value, J)
             end
     end
    ].

%%------------------------------------------------------------------------------
%% @doc merge in any overrides from the global provisioning db
%% @end
%%------------------------------------------------------------------------------
-spec set_global_overrides(cb_context:context()) ->
                                  [fun((kz_json:object()) -> kz_json:object()),...].
set_global_overrides(_) ->
    GlobalDefaults = case kz_datamgr:open_cache_doc(?KZ_PROVISIONER_DB, <<"base_properties">>) of
                         {'ok', JObj} -> JObj;
                         {'error', _} -> kz_json:new()
                     end,
    [fun(J) ->
             case kz_json:get_value(<<"defaults">>, GlobalDefaults) of
                 'undefined' -> J;
                 Overrides -> kz_json:merge(J, Overrides)
             end
     end
    ].

%%------------------------------------------------------------------------------
%% @doc merge in any overrides from the account doc
%% @end
%%------------------------------------------------------------------------------
-spec set_account_overrides(cb_context:context()) ->
                                   [fun((kz_json:object()) -> kz_json:object()),...].
set_account_overrides(Context) ->
    Account = case kzd_accounts:fetch(cb_context:account_id(Context)) of
                  {'ok', JObj} -> JObj;
                  {'error', _} -> kz_json:new()
              end,
    [fun(J) ->
             case kz_json:get_value([<<"provision">>, <<"overrides">>], Account) of
                 'undefined' -> J;
                 Overrides -> kz_json:merge(J, Overrides)
             end
     end
    ].

%%------------------------------------------------------------------------------
%% @doc merge in any overrides from the user doc
%% @end
%%------------------------------------------------------------------------------
-spec set_user_overrides(cb_context:context()) ->
                                [fun((kz_json:object()) -> kz_json:object()),...].
set_user_overrides(Context) ->
    OwnerId = kz_json:get_ne_value(<<"owner_id">>, cb_context:doc(Context)),
    User = case is_binary(OwnerId)
               andalso kz_datamgr:open_doc(cb_context:account_db(Context), OwnerId)
           of
               {'ok', JObj} -> JObj;
               _Else -> kz_json:new()
           end,
    [fun(J) ->
             case kz_json:get_value([<<"provision">>, <<"overrides">>], User) of
                 'undefined' -> J;
                 Overrides -> kz_json:merge(J, Overrides)
             end
     end
    ].

%%------------------------------------------------------------------------------
%% @doc merge in any overrides from the device doc
%% @end
%%------------------------------------------------------------------------------
-spec set_device_overrides(cb_context:context()) ->
                                  [fun((kz_json:object()) -> kz_json:object()),...].
set_device_overrides(Context) ->
    Device = cb_context:doc(Context),
    [fun(J) ->
             case kz_json:get_value([<<"provision">>, <<"overrides">>], Device) of
                 'undefined' -> J;
                 Overrides ->
                     kz_json:merge(J, Overrides)
             end
     end
    ].

%%------------------------------------------------------------------------------
%% @doc Send awesome provisioning request
%% @end
%%------------------------------------------------------------------------------
-spec send_provisioning_request(kz_json:object(), kz_term:ne_binary()) -> 'ok'.
send_provisioning_request(Template, MACAddress) ->
    ProvisionRequest = kz_json:encode(Template),
    UrlTmpl = kapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioning_url">>),
    UrlString = re:replace(UrlTmpl, "{{mac_address}}", MACAddress, ['global', {'return', 'list'}]),
    lager:debug("provisioning via ~s", [UrlString]),
    case kz_http:post(UrlString, ?JSON_HEADERS, ProvisionRequest) of
        {'ok', 200, _, Response} ->
            lager:debug("SUCCESS! BOOM! ~s", [Response]);
        {'ok', Code, _, Response} ->
            lager:debug("ERROR! OH NO! ~p. ~s", [Code, Response]);
        {'error', R} ->
            lager:debug("ERROR! OH NO! ~p", [R])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_provisioning_type() -> kz_term:api_ne_binary().
get_provisioning_type() ->
    case kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"provisioning_type">>) of
        'undefined' ->
            lager:debug("using ~p for provisioner_type", [?PROVISIONER_CONFIG]),
            kapps_config:get_ne_binary(?PROVISIONER_CONFIG, <<"provisioning_type">>);
        Result ->
            lager:debug("using ~p for provisioner_type", [?MOD_CONFIG_CAT]),
            Result
    end.


-spec maybe_sync_sip_data(cb_context:context(), 'user' | 'device') -> 'ok'.
maybe_sync_sip_data(Context, Type) ->
    ShouldSync = cb_context:fetch(Context, 'sync'),
    maybe_sync_sip_data(Context, Type, ShouldSync).

-spec maybe_sync_sip_data(cb_context:context(), 'user' | 'device', boolean() | 'force') -> 'ok'.
maybe_sync_sip_data(_Context, _Type, 'false') ->
    lager:debug("sync not configured in context for ~s", [_Type]);
maybe_sync_sip_data(Context, 'device', 'true') ->
    NewDevice = cb_context:doc(Context),
    OldDevice = cb_context:fetch(Context, 'db_doc'),
    OldUsername = kzd_devices:sip_username(OldDevice),
    case kzd_devices:sip_username(NewDevice) =/= OldUsername
        orelse kzd_devices:sip_password(NewDevice) =/= kzd_devices:sip_password(OldDevice)
    of
        'false' ->
            lager:debug("nothing has changed on device; no check-sync needed");
        'true' ->
            Realm = kzd_accounts:fetch_realm(cb_context:account_id(Context)),
            send_check_sync(OldUsername, Realm, cb_context:req_id(Context))
    end;
maybe_sync_sip_data(Context, 'device', 'force') ->
    Username = kzd_devices:sip_username(cb_context:doc(Context)),
    Realm = kzd_accounts:fetch_realm(cb_context:account_id(Context)),
    send_check_sync(Username, Realm, cb_context:req_id(Context));
maybe_sync_sip_data(Context, 'user', 'true') ->
    Realm = kzd_accounts:fetch_realm(cb_context:account_id(Context)),
    Req = [{<<"Realm">>, Realm}
          ,{<<"Fields">>, [<<"Username">>]}
          ],
    ReqResp = kz_amqp_worker:call(Req
                                 ,fun kapi_registration:publish_query_req/1
                                 ,fun kapi_registration:query_resp_v/1
                                 ),
    case ReqResp of
        {'error', _E} -> lager:debug("no devices to send check sync to for realm ~s", [Realm]);
        {'timeout', _} -> lager:debug("timed out query for fetching devices for ~s", [Realm]);
        {'ok', JObj} ->
            lists:foreach(fun(J) ->
                                  Username = kz_json:get_value(<<"Username">>, J),
                                  send_check_sync(Username, Realm, 'undefined')
                          end
                         ,kz_json:get_value(<<"Fields">>, JObj)
                         )
    end;
maybe_sync_sip_data(Context, 'user', 'force') ->
    case cb_users_v2:user_devices(Context) of
        {'error', _E} ->
            lager:debug("unable to sync user devices: ~p", [_E]);
        {'ok', []} ->
            lager:debug("no user devices to sync");
        {'ok', DeviceDocs} ->
            Realm = kzd_accounts:fetch_realm(cb_context:account_id(Context)),
            _ = [send_check_sync(kzd_devices:presence_id(DeviceDoc), Realm, cb_context:req_id(Context))
                 || DeviceDoc <- DeviceDocs
                ],
            'ok'
    end.

-spec send_check_sync(kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary()) -> 'ok'.
send_check_sync('undefined', _Realm, _MsgId) ->
    lager:warning("did not send check sync: username is undefined");
send_check_sync(_Username, 'undefined', _MsgId) ->
    lager:warning("did not send check sync: realm is undefined");
send_check_sync(Username, Realm, MsgId) ->
    lager:debug("sending check sync for ~s @ ~s", [Username, Realm]),
    publish_check_sync(MsgId, [{<<"Event">>, <<"check-sync">>}
                              ,{<<"Realm">>, Realm}
                              ,{<<"Username">>, Username}
                               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                              ]).

-spec publish_check_sync(kz_term:api_binary(), kz_term:proplist()) -> 'ok'.
publish_check_sync('undefined', Req) ->
    publish_check_sync(Req);
publish_check_sync(MsgId, Req) ->
    publish_check_sync([{<<"Msg-ID">>, MsgId} | Req]).

-spec publish_check_sync(kz_term:proplist()) -> 'ok'.
publish_check_sync(Req) ->
    kz_amqp_worker:cast(Req, fun kapi_switch:publish_notify/1).
