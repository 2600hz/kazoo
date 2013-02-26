%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% Common functions for the provisioner modules
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(provisioner_util).

-include("src/crossbar.hrl").

-export([get_mac_address/1]).
-export([maybe_provision/1]).
-export([maybe_send_contact_list/1]).
-export([get_provision_defaults/1]).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".devices">>).
-define(TEMPLATE_ATTCH, <<"template">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_mac_address/1 :: (cb_context:context()) -> 'undefined' | string().
get_mac_address(#cb_context{doc=JObj}) ->
    case wh_json:get_ne_value(<<"mac_address">>, JObj) of
        undefined -> undefined;
        MACAddress ->
            re:replace(wh_util:to_list(MACAddress)
                       ,"[^0-9a-fA-F]"
                       ,""
                       ,[{return, list}, global])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_provision/1 :: (cb_context:context()) -> boolean().
maybe_provision(#cb_context{resp_status=success}=Context) ->
    MACAddress = get_mac_address(Context),
    case MACAddress =/= undefined
        andalso whapps_config:get_binary(?MOD_CONFIG_CAT, <<"provisioning_type">>) 
    of
        <<"super_awesome_provisioner">> ->
            spawn(fun() ->
                          do_full_provisioner_provider(MACAddress, Context),
                          do_full_provision(MACAddress, Context)
                  end),
            true;
        <<"awesome_provisioner">> ->
            spawn(fun() -> do_awesome_provision(MACAddress, Context) end),
            true;
        <<"simple_provisioner">>  ->
            spawn(fun() -> do_simple_provision(MACAddress, Context) end),
            true;
        _ -> false
    end;
maybe_provision(_) -> false.

-spec maybe_send_contact_list/1 :: (cb_context:context()) -> cb_context:context().
maybe_send_contact_list(#cb_context{resp_status=success}=Context) ->
    _ = case whapps_config:get_binary(?MOD_CONFIG_CAT, <<"provisioning_type">>) of
            <<"super_awesome_provisioner">> ->
                spawn(fun() -> do_full_provision_contact_list(Context) end);
            _ -> ok
        end,
    Context;
maybe_send_contact_list(Context) ->
    Context.

-spec do_full_provisioner_provider/2 :: (ne_binary(), #cb_context{}) -> boolean().
do_full_provisioner_provider(_, #cb_context{db_name=AccountDb, account_id=AccountId}) ->
    do_full_provision_contact_list(AccountId, AccountDb).

-spec do_full_provision_contact_list/1 :: (#cb_context{}) -> boolean().
-spec do_full_provision_contact_list/2 :: (ne_binary(), ne_binary()) -> boolean().

do_full_provision_contact_list(#cb_context{db_name=AccountDb, account_id=AccountId}=Context) ->
    case should_build_contact_list(Context) of
        false -> false;
        true ->
            do_full_provision_contact_list(AccountId, AccountDb)
    end.

do_full_provision_contact_list(AccountId, AccountDb) ->
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {ok, JObj} ->
            Routines = [fun(J) -> wh_json:public_fields(J) end 
                        ,fun(J) -> 
                                 ResellerId = wh_services:find_reseller_id(AccountId),
                                 wh_json:set_value(<<"provider_id">>, ResellerId, J)
                         end
                        ,fun(J) -> wh_json:delete_key(<<"available_apps">>, J) end
                        ,fun(J) ->
                                 ContactList = provisioner_contact_list:build(AccountDb),
                                 wh_json:set_value(<<"directory">>, ContactList, J)
                         end
                       ],
            Provider = lists:foldl(fun(F, J) -> F(J) end, JObj, Routines),
            PartialURL = <<AccountId/binary, "/">>,
            send_to_full_provisioner(Provider, PartialURL);
        {error, _R} ->
            lager:warning("failed to get account definition for ~s: ~p", [AccountId, _R]),
            false
    end.

should_build_contact_list(#cb_context{doc=JObj}=Context) ->
    OriginalJObj = cb_context:fetch(db_doc, Context), 
    case wh_json:is_json_object(OriginalJObj) of
        false ->
            wh_json:get_value(<<"pvt_type">>, JObj) =:= <<"callflow">>;
        true ->
            wh_json:get_value(<<"pvt_type">>, JObj) =:= <<"callflow">>
                orelse wh_json:get_value(<<"name">>, JObj) =/=  wh_json:get_value(<<"name">>, OriginalJObj)
                orelse wh_json:get_value(<<"first_name">>, JObj) =/=  wh_json:get_value(<<"first_name">>, OriginalJObj)
                orelse wh_json:get_value(<<"last_name">>, JObj) =/=  wh_json:get_value(<<"last_name">>, OriginalJObj)
                orelse wh_json:get_value([<<"contact_list">>, <<"exclude">>], JObj) 
                =/=  wh_json:get_value([<<"contact_list">>, <<"exclude">>], OriginalJObj)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This doesn't belong here, needs to be in an external library. Make request to
%% get provisioning defaults
%% @end
%%--------------------------------------------------------------------
-spec get_provision_defaults/1 :: (cb_context:context()) -> cb_context:context().
get_provision_defaults(#cb_context{doc=JObj}=Context) ->
    Url = [whapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioning_url">>)
           ,"?request=data"
           ,"&brand=", mochiweb_util:quote_plus(wh_json:get_string_value([<<"properties">>, <<"brand">>], JObj))
           ,"&model=", mochiweb_util:quote_plus(wh_json:get_string_value([<<"properties">>, <<"model">>], JObj))
           ,"&product=", mochiweb_util:quote_plus(wh_json:get_string_value([<<"properties">>, <<"product">>], JObj))
          ],
    UrlString = lists:flatten(Url),
    Headers = [KV || {_, V}=KV <- [{"Host", whapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioner_host">>)}
                                   ,{"Referer", whapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioner_referer">>)}
                                   ,{"User-Agent", wh_util:to_list(erlang:node())}
                                  ],
                     V =/= undefined
              ],
    Body = [],
    HTTPOptions = [],
    lager:debug("attempting to pull provisioning configs from ~s", [UrlString]),
    case ibrowse:send_req(UrlString, Headers, get, Body, HTTPOptions) of
        {ok, "200", _, Response} ->
            lager:debug("great success, accquired provisioning template"),
            JResp = wh_json:decode(Response),
            Context#cb_context{
                doc = wh_json:set_value(<<"template">>, JResp, JObj)
                ,resp_status = success
            };
        {ok, Status, _, _} ->
            lager:debug("could not get provisioning template defaults: ~s", [Status]),
            crossbar_util:response(error, <<"Error retrieving content from external site">>, 500, Context);
        {error, _R} ->
            lager:debug("could not get provisioning template defaults: ~p", [_R]),
            crossbar_util:response(error, <<"Error retrieving content from external site">>, 500, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% post data to a provisiong server
%% @end
%%--------------------------------------------------------------------
-spec do_simple_provision/2 :: (string(), cb_context:context()) -> boolean().
do_simple_provision(MACAddress, #cb_context{doc=JObj}=Context) ->
    case whapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioning_url">>) of
        undefined -> false;
        Url ->
            AccountRealm = crossbar_util:get_account_realm(Context),
            Headers = [{K, V}
                       || {K, V} <- [{"Host", whapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioning_host">>)}
                                     ,{"Referer", whapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioning_referer">>)}
                                     ,{"User-Agent", wh_util:to_list(erlang:node())}
                                     ,{"Content-Type", "application/x-www-form-urlencoded"}
                                    ]
                              ,V =/= undefined
                      ],
            HTTPOptions = [],
            Body = [{"device[mac]", MACAddress}
                    ,{"device[label]", wh_json:get_string_value(<<"name">>, JObj)}
                    ,{"sip[realm]", wh_json:get_string_value([<<"sip">>, <<"realm">>], JObj, AccountRealm)}
                    ,{"sip[username]", wh_json:get_string_value([<<"sip">>, <<"username">>], JObj)}
                    ,{"sip[password]", wh_json:get_string_value([<<"sip">>, <<"password">>], JObj)}
                    ,{"submit", "true"}
                   ],
            Encoded = mochiweb_util:urlencode(Body),
            lager:debug("posting to ~s with: ~-300p", [Url, Encoded]),
            Res = ibrowse:send_req(Url, Headers, post, Encoded, HTTPOptions),            
            lager:debug("response from server: ~p", [Res]),
            true
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% post data to a provisiong server
%% @end
%%--------------------------------------------------------------------
-spec do_full_provision/2 :: (string(), #cb_context{}) -> boolean().
do_full_provision(MACAddress, #cb_context{}=Context) ->
    case get_merged_device(MACAddress, Context) of
        {error, _} -> false;
        {ok, #cb_context{doc=JObj}} ->
            do_full_provision(MACAddress, JObj)
    end;
do_full_provision(MACAddress, JObj) ->
    PartialURL = <<(wh_json:get_binary_value(<<"account_id">>, JObj))/binary
                   ,"/", (wh_util:to_binary(MACAddress))/binary>>,
    send_to_full_provisioner(JObj, PartialURL).

-spec send_to_full_provisioner/2 :: (wh_json:object(), ne_binary()) -> boolean().
send_to_full_provisioner(JObj, PartialURL) ->
    case whapps_config:get_binary(?MOD_CONFIG_CAT, <<"provisioning_url">>) of
        undefined -> false;
        Url ->
            Headers = [{"Content-Type", "application/json"}],
            FullUrl = wh_util:to_lower_string(<<Url/binary, "/", PartialURL/binary>>),
            {ok, _, _, RawJObj} = ibrowse:send_req(FullUrl, Headers, get, "", [{inactivity_timeout, 10000}]),
            {Verb, Body} = case wh_json:get_integer_value([<<"error">>, <<"code">>], wh_json:decode(RawJObj)) of
                               undefined -> 
                                   Props = [{<<"provider_id">>, wh_json:get_value(<<"provider_id">>, JObj)}
                                            ,{<<"name">>, wh_json:get_value(<<"name">>, JObj)}
                                            ,{<<"settings">>, JObj}
                                           ],
                                   J =  wh_json:from_list(props:filter_undefined(Props)),
                                   {post,  wh_util:to_list(wh_json:encode(J))};
                               404 -> 
                                   {put, wh_util:to_list(wh_json:encode(JObj))} 
                           end,
            lager:debug("making ~s request to ~s with: ~-300p", [Verb, FullUrl, Body]),  
            Res = ibrowse:send_req(FullUrl, Headers, Verb, Body, [{inactivity_timeout, 10000}]),
            lager:debug("response from server: ~p", [Res]),
            true
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec do_awesome_provision/2 :: (string(), cb_context:context()) -> boolean().
do_awesome_provision(_MACAddress, Context) ->
    case get_template(Context) of
        {error, _} -> false;
        {ok, JObj} ->
            send_provisioning_template(JObj, Context),
            true
    end.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_merged_device(ne_binary(), cb_context:context()) -> {ok, cb_context:context()} | {error, binary()}.
get_merged_device(MACAddress, Context) ->
    case merge_device(MACAddress, Context) of
        {error, _}=E -> E;
        {ok, Data} ->
            {ok, Context#cb_context{doc=Data}}
    end.

-spec merge_device(ne_binary(), cb_context:context()) -> {ok, wh_json:json_object()} | {error, binary()}.
merge_device(MACAddress, #cb_context{doc=JObj, account_id=AccountId}) ->
    Routines = [fun(J) -> wh_json:set_value(<<"mac_address">>, MACAddress, J) end
                ,fun(J) -> 
                        OwnerId = wh_json:get_ne_value(<<"owner_id">>, JObj),
                        Owner = get_owner(OwnerId, AccountId),
                        wh_json:merge_recursive(J, Owner) 
                 end
                ,fun(J) -> wh_json:delete_key(<<"apps">>, J) end
                ,fun(J) -> wh_json:set_value(<<"account_id">>, AccountId, J) end
               ],
    MergedDevice = lists:foldl(fun(F, J) -> F(J) end, JObj, Routines),
    {ok, wh_json:public_fields(MergedDevice)}.

-spec get_owner/2 :: (api_binary(), ne_binary()) -> wh_json:json_object().
get_owner(undefined, _) ->
    wh_json:new();
get_owner(OwnerId, AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:open_cache_doc(AccountDb, OwnerId) of
        {ok, Owner} ->
            Owner;
        {error, _R} ->
            lager:debug("unable to open user definition ~s/~s: ~p", [AccountDb, OwnerId, _R]),
            wh_json:new()
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do awesome provisioning
%% @end
%%--------------------------------------------------------------------
send_provisioning_template(JObj, #cb_context{doc=Device}=Context) ->
    %% TODO: theoretically this is the start of multiple line support....
    Line = <<"lineloop|line_1">>,
    MAC = re:replace(wh_json:get_string_value(<<"mac_address">>, Device, "")
                     ,"[^0-9a-fA-F]", "", [{return, list}, global]),
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
    LineLoop = wh_json:get_value([<<"data">>, <<"globals">>, <<"globals">>, Line], DefaultTemplate),
    LineTemplate = lists:foldr(fun(F, J) -> F(J) end, LineLoop, LineUpdaters),

    Template = wh_json:set_value([<<"data">>, <<"globals">>, <<"globals">>, Line], LineTemplate, DefaultTemplate),
    send_provisioning_request(Template, MAC).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If the device specifies a local template id then return that 
%% template
%% @end
%%--------------------------------------------------------------------
-spec get_template/1 :: (#cb_context{}) -> {ok, wh_json:json_object()} |
                                           {error, term()}.
get_template(#cb_context{doc=Device, db_name=Db}) ->
    DocId = wh_json:get_value([<<"provision">>, <<"id">>], Device),
    case is_binary(DocId) andalso couch_mgr:fetch_attachment(Db, DocId, ?TEMPLATE_ATTCH) of 
        false ->
            lager:debug("unknown template id ~s", [DocId]),
            {error, not_found};
        {error, _R}=E ->
            lager:debug("could not fetch template doc ~s: ~p", [DocId, _R]),
            E;
        {ok, Attachment} ->
            {ok, wh_json:decode(Attachment)}
    end.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% add the account_id to the root of the provisioning json
%% @end
%%--------------------------------------------------------------------
-spec set_account_id(cb_context:context()) -> [fun((wh_json:json_object()) -> wh_json:json_object()),...].
set_account_id(#cb_context{auth_account_id=AccountId}) ->
    [fun(J) -> wh_json:set_value(<<"account_id">>, AccountId, J) end].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% get the settings from the account doc that should be used in the 
%% base properties for the line
%% @end
%%--------------------------------------------------------------------
-spec set_account_line_defaults(cb_context:context()) -> [fun((wh_json:json_object()) -> wh_json:json_object()),...].
set_account_line_defaults(#cb_context{account_id=AccountId}) ->
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    Account = case couch_mgr:open_cache_doc(AccountDb, AccountId) of
                  {ok, JObj} -> JObj;
                  {error, _} -> wh_json:new()
              end,
    [fun(J) ->
              case wh_json:get_ne_value(<<"realm">>, Account) of
                  undefined -> J;
                  Value -> wh_json:set_value([<<"server_host">>, <<"value">>], Value, J)
              end
     end
     ,fun(J) ->
              case wh_json:get_ne_value(<<"name">>, Account) of
                  undefined -> J;
                  Value -> wh_json:set_value([<<"displayname">>, <<"value">>], Value, J)
              end              
      end
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% get the settings from the device doc that should be used in the 
%% base properties for the line
%% @end
%%--------------------------------------------------------------------
-spec set_device_line_defaults(cb_context:context()) -> [fun((wh_json:json_object()) -> wh_json:json_object()),...].
set_device_line_defaults(#cb_context{doc=Device}) ->
    [fun(J) -> 
             case wh_json:get_ne_value([<<"sip">>, <<"username">>], Device) of
                 undefined -> J;
                 Value -> wh_json:set_value([<<"authname">>, <<"value">>], Value, J)
             end
     end
     ,fun(J) -> 
              case wh_json:get_ne_value([<<"sip">>, <<"username">>], Device) of
                  undefined -> J;
                  Value -> wh_json:set_value([<<"username">>, <<"value">>], Value, J)
              end
      end
     ,fun(J) -> 
              case wh_json:get_ne_value([<<"sip">>, <<"password">>], Device) of
                  undefined -> J;
                  Value -> wh_json:set_value([<<"secret">>, <<"value">>], Value, J)
              end
      end
     ,fun(J) -> 
              case wh_json:get_ne_value([<<"sip">>, <<"realm">>], Device) of
                  undefined -> J;
                  Value -> wh_json:set_value([<<"server_host">>, <<"value">>], Value, J)
              end
      end
     ,fun(J) ->
              case wh_json:get_ne_value(<<"name">>, Device) of
                  undefined -> J;
                  Value -> wh_json:set_value([<<"displayname">>, <<"value">>], Value, J)
              end              
      end
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% merge in any overrides from the global provisioning db
%% @end
%%--------------------------------------------------------------------
-spec set_global_overrides(cb_context:context()) -> [fun((wh_json:json_object()) -> wh_json:json_object()),...].
set_global_overrides(_) ->
    GlobalDefaults = case couch_mgr:open_cache_doc(?WH_PROVISIONER_DB, <<"base_properties">>) of
                         {ok, JObj} -> JObj;
                         {error, _} -> wh_json:new()
                     end,
    [fun(J) ->
             case wh_json:get_value(<<"defaults">>, GlobalDefaults) of
                 undefined -> J;
                 Overrides ->
                     wh_json:merge_recursive(J, Overrides)
             end
     end
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% merge in any overrides from the account doc
%% @end
%%--------------------------------------------------------------------
-spec set_account_overrides(cb_context:context()) -> [fun((wh_json:json_object()) -> wh_json:json_object()),...].
set_account_overrides(#cb_context{account_id=AccountId}) ->
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    Account = case couch_mgr:open_cache_doc(AccountDb, AccountId) of
                  {ok, JObj} -> JObj;
                  {error, _} -> wh_json:new()
              end,
    [fun(J) ->
             case wh_json:get_value([<<"provision">>, <<"overrides">>], Account) of
                 undefined -> J;
                 Overrides -> 
                     wh_json:merge_recursive(J, Overrides)
             end
     end
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% merge in any overrides from the user doc
%% @end
%%--------------------------------------------------------------------
-spec set_user_overrides(cb_context:context()) -> [fun((wh_json:json_object()) -> wh_json:json_object()),...].
set_user_overrides(#cb_context{doc=Device, account_id=AccountId}) ->
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    OwnerId = wh_json:get_ne_value(<<"owner_id">>, Device),
    User = case is_binary(OwnerId) andalso couch_mgr:open_doc(AccountDb, OwnerId) of
               {ok, JObj} -> JObj;
               _Else -> wh_json:new()
           end,
    [fun(J) ->
             case wh_json:get_value([<<"provision">>, <<"overrides">>], User) of
                 undefined -> J;
                 Overrides -> 
                     wh_json:merge_recursive(J, Overrides)
             end
     end
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% merge in any overrides from the device doc
%% @end
%%--------------------------------------------------------------------
-spec set_device_overrides(cb_context:context()) -> [fun((wh_json:json_object()) -> wh_json:json_object()),...].
set_device_overrides(#cb_context{doc=Device}) ->
    [fun(J) ->
             case wh_json:get_value([<<"provision">>, <<"overrides">>], Device) of
                 undefined -> J;
                 Overrides -> 
                     wh_json:merge_recursive(J, Overrides)
             end
     end
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Send awesome provisioning request
%% @end
%%--------------------------------------------------------------------
-spec send_provisioning_request(wh_json:json_object(), ne_binary()) -> 'ok'.
send_provisioning_request(Template, MACAddress) ->
    ProvisionRequest = wh_json:encode(Template),
    UrlTmpl = whapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioning_url">>),
    UrlString = re:replace(UrlTmpl, "{{mac_address}}", MACAddress, [global, {return, list}]),
    Headers = [KV || {_, V}=KV <- [{"Content-Type", "application/json"}
                                   ,{"Host", whapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioner_host">>)}
                                   ,{"Referer", whapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioner_referer">>)}
                                   ,{"User-Agent", wh_util:to_list(erlang:node())}
                                  ],
                     V =/= undefined
              ],
    HTTPOptions = [],
    lager:debug("provisioning via ~s", [UrlString]),
    case ibrowse:send_req(UrlString, Headers, post, ProvisionRequest, HTTPOptions) of
        {ok, "200", _, Response} ->
            lager:debug("SUCCESS! BOOM! ~s", [Response]);
        {ok, Code, _, Response} ->
            lager:debug("ERROR! OH NO! ~s. ~s", [Code, Response])
    end.
