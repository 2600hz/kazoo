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

-include("include/crossbar.hrl").

-export([get_provision_defaults/1]).
-export([send_provisioning_template/2]).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".provisioner_templates">>).

%%--------------------------------------------------------------------
%% @public
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
%% add the account_id to the root of the provisioning json
%% @end
%%--------------------------------------------------------------------
-spec set_account_id/1 :: (cb_context:context()) -> [fun((wh_json:json_object()) -> wh_json:json_object()),...].
set_account_id(#cb_context{auth_account_id=AccountId}) ->
    [fun(J) -> wh_json:set_value(<<"account_id">>, AccountId, J) end].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% get the settings from the account doc that should be used in the 
%% base properties for the line
%% @end
%%--------------------------------------------------------------------
-spec set_account_line_defaults/1 :: (cb_context:context()) -> [fun((wh_json:json_object()) -> wh_json:json_object()),...].
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
-spec set_device_line_defaults/1 :: (cb_context:context()) -> [fun((wh_json:json_object()) -> wh_json:json_object()),...].
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
-spec set_global_overrides/1 :: (cb_context:context()) -> [fun((wh_json:json_object()) -> wh_json:json_object()),...].
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
-spec set_account_overrides/1 :: (cb_context:context()) -> [fun((wh_json:json_object()) -> wh_json:json_object()),...].
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
-spec set_user_overrides/1 :: (cb_context:context()) -> [fun((wh_json:json_object()) -> wh_json:json_object()),...].
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
-spec set_device_overrides/1 :: (cb_context:context()) -> [fun((wh_json:json_object()) -> wh_json:json_object()),...].
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
-spec send_provisioning_request/2 :: (wh_json:json_object(), ne_binary()) -> 'ok'.
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This doesn't belong here, needs to be in an external library. Make request to
%% get provisioning defaults
%% @end
%%--------------------------------------------------------------------
-spec get_provision_defaults/1 :: (cb_context:context()) -> cb_context:context().
get_provision_defaults(#cb_context{doc=JObj}=Context) ->
    Url = [whapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioner_template_url">>)
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
