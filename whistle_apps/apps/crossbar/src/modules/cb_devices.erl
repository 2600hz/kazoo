%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Devices module
%%%
%%% Handle client requests for device documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_devices).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,billing/1
         ,validate/1, validate/2
         ,put/1
         ,post/2
         ,delete/2
         ,reconcile_services/1
        ]).

-include("include/crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".devices">>).

-define(CB_LIST, <<"devices/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.devices">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.devices">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.billing">>, ?MODULE, billing),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.devices">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.devices">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.devices">>, ?MODULE, post),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.delete.devices">>, ?MODULE, delete),
    crossbar_bindings:bind(<<"v1_resource.finish_request.*.devices">>, ?MODULE, reconcile_services).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods().
-spec allowed_methods/1 :: (path_token()) -> http_methods().
allowed_methods() ->
    ['GET', 'PUT'].
allowed_methods(<<"status">>) ->
    ['GET'];
allowed_methods(_) ->
    ['GET', 'POST', 'DELETE'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
-spec resource_exists/1 :: (path_token()) -> 'true'.
resource_exists() -> true.
resource_exists(_) -> true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Ensure we will be able to bill for devices
%% @end
%%--------------------------------------------------------------------
billing(#cb_context{req_nouns=[{<<"devices">>, _}|_], req_verb = <<"get">>}=Context) ->
    Context;
billing(#cb_context{req_nouns=[{<<"devices">>, _}|_], account_id=AccountId}=Context) ->
    try wh_services:allow_updates(AccountId) of
        true -> Context
    catch
        throw:{Error, Reason} ->
            crossbar_util:response(error, wh_util:to_binary(Error), 500, Reason, Context)
    end;
billing(Context) -> Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Bill for devices
%% @end
%%--------------------------------------------------------------------
-spec reconcile_services/1 :: (#cb_context{}) -> #cb_context{}.
reconcile_services(#cb_context{req_verb = <<"get">>}=Context) ->
    Context;
reconcile_services(#cb_context{account_id=AccountId}=Context) ->
    _ = wh_services:reconcile(AccountId, <<"devices">>),
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    load_device_summary(Context);
validate(#cb_context{req_verb = <<"put">>}=Context) ->
    create_device(Context).

validate(#cb_context{req_verb = <<"get">>}=Context, <<"status">>) ->
    load_device_status(Context);
validate(#cb_context{req_verb = <<"get">>}=Context, DocId) ->
    load_device(DocId, Context);
validate(#cb_context{req_verb = <<"post">>}=Context, DocId) ->
    update_device(DocId, Context);
validate(#cb_context{req_verb = <<"delete">>}=Context, DocId) ->
    load_device(DocId, Context).

-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
post(#cb_context{}=Context, _DocId) ->
    _ = cb_context:put_reqid(Context),
    case crossbar_doc:save(Context) of
        #cb_context{resp_status=success, doc=Doc1, account_id=AcctId}=Context1 ->
            _ = case wh_json:get_ne_value([<<"sip">>, <<"realm">>], Doc1) of
                    undefined -> ok;
                    _Else ->
                        lager:debug("adding device to the sip auth aggregate"),
                        couch_mgr:ensure_saved(?WH_SIP_DB, wh_json:delete_key(<<"_rev">>, Doc1))
                end,
            
            case wh_json:get_ne_value([<<"sip">>, <<"ip">>], Doc1) of
                undefined -> ok;
                DeviceIP ->
                    lager:debug("adding device to the sip auth aggregate"),
                    {ok, D} = couch_mgr:ensure_saved(?WH_SIP_DB, wh_json:delete_key(<<"_rev">>, Doc1)),
                    maybe_update_acls(DeviceIP, AcctId, wh_json:get_value(<<"_id">>, D))
            end,

            spawn(fun() -> do_simple_provision(Context1) end),
            Context1;
        Else ->
            Else
    end.

-spec put/1 :: (#cb_context{}) -> #cb_context{}.
put(#cb_context{}=Context) ->
    _ = cb_context:put_reqid(Context),
    case crossbar_doc:save(Context) of
        #cb_context{resp_status=success, doc=Doc1, account_id=AcctId}=Context1 ->
            _ = case wh_json:get_ne_value([<<"sip">>, <<"realm">>], Doc1) of
                    undefined -> ok;
                    _Else ->
                        lager:debug("adding device to the sip auth aggregate"),
                        couch_mgr:ensure_saved(?WH_SIP_DB, wh_json:delete_key(<<"_rev">>, Doc1))
                end,
            
            case wh_json:get_ne_value([<<"sip">>, <<"ip">>], Doc1) of
                undefined -> ok;
                DeviceIP ->
                    lager:debug("adding device to the sip auth aggregate"),
                    {ok, D} = couch_mgr:ensure_saved(?WH_SIP_DB, wh_json:delete_key(<<"_rev">>, Doc1)),
                    maybe_update_acls(DeviceIP, AcctId, wh_json:get_value(<<"_id">>, D))
            end,

            _ = spawn(fun() -> do_simple_provision(Context1) end),
            Context1;
        Else ->
            Else
    end.

-spec delete/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
delete(#cb_context{doc=Doc}=Context, _DocId) ->
    _ = cb_context:put_reqid(Context),

    case crossbar_doc:delete(Context) of
        #cb_context{resp_status=success}=Context1 ->
            DeviceId = wh_json:get_value(<<"_id">>, Doc),
            _ = case couch_mgr:lookup_doc_rev(?WH_SIP_DB, DeviceId) of
                    {ok, Rev} ->
                        couch_mgr:del_doc(?WH_SIP_DB, wh_json:set_value(<<"_rev">>, Rev, Doc));
                    {error, not_found} ->
                        ok
                end,
            wapi_switch:publish_reloadacl(),
            Context1;
        Else ->
            Else
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_device_summary/1 :: (#cb_context{}) -> #cb_context{}.
load_device_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new device document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_device/1 :: (#cb_context{}) -> #cb_context{}.
create_device(#cb_context{req_data=Req, db_name=Db}=Context) ->
    SIPRealm = wh_json:get_ne_value([<<"sip">>, <<"realm">>], Req, <<>>),
    AccountRealm = crossbar_util:get_account_realm(Context),
    Data = case AccountRealm =:= SIPRealm of
               true ->
                   wh_json:delete_key([<<"sip">>, <<"realm">>], Req);
               false ->
                   Req
           end,
    Username = wh_json:get_ne_value([<<"sip">>, <<"username">>], Data),
    Realm = wh_json:get_ne_value([<<"sip">>, <<"realm">>], Data),
    IsCredsUnique = is_sip_creds_unique(Db, Realm, Username),

    Data1 = set_outbound_flags(Data, wh_json:get_value(<<"outbound_flags">>, Data)),

    case wh_json_validator:is_valid(Data1, <<"devices">>) of
        {fail, Errors} when not IsCredsUnique ->
            E = wh_json:set_value([<<"sip">>, <<"username">>, <<"unique">>]
                                  ,<<"SIP credentials are already in use">>
                                  ,Errors),
            crossbar_util:response_invalid_data(E, Context);
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, _} when not IsCredsUnique ->
            E = wh_json:set_value([<<"sip">>, <<"username">>, <<"unique">>]
                                  ,<<"SIP credentials are already in use">>
                                  ,wh_json:new()),
            crossbar_util:response_invalid_data(E, Context);
        {pass, JObj} ->
            Context#cb_context{resp_status=success
                               ,doc=wh_json:set_value(<<"pvt_type">>, <<"device">>, JObj)
                              }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a device document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_device/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_device(DocId, Context) ->
    crossbar_doc:load(DocId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing device document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_device/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
update_device(DocId, #cb_context{req_data=Req, db_name=Db}=Context) ->
    SIPRealm = wh_json:get_ne_value([<<"sip">>, <<"realm">>], Req, <<>>),
    AccountRealm = crossbar_util:get_account_realm(Context),
    Data = case AccountRealm =:= SIPRealm of
               true ->
                   wh_json:delete_key([<<"sip">>, <<"realm">>], Req);
               false ->
                   Req
           end,
    Username = wh_json:get_ne_value([<<"sip">>, <<"username">>], Data),
    Realm = wh_json:get_ne_value([<<"sip">>, <<"realm">>], Data),
    IsCredsUnique = is_sip_creds_unique(Db, Realm, Username, DocId),

    Data1 = set_outbound_flags(Data, wh_json:get_value(<<"outbound_flags">>, Data)),

    case wh_json_validator:is_valid(Data1, <<"devices">>) of
        {fail, Errors} when not IsCredsUnique ->
            E = wh_json:set_value([<<"sip">>, <<"username">>, <<"unique">>]
                                  ,<<"SIP credentials are already in use">>
                                  ,Errors),
            crossbar_util:response_invalid_data(E, Context);
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, _} when not IsCredsUnique ->
            E = wh_json:set_value([<<"sip">>, <<"username">>, <<"unique">>]
                                  ,<<"SIP credentials are already in use">>
                                  ,wh_json:new()),
            crossbar_util:response_invalid_data(E, Context);
        {pass, JObj} ->
            crossbar_doc:load_merge(DocId, JObj, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieve the status of the devices linked to the account
%% Reads registered devices in registrations, then map to devices of the account
%% @end
%%--------------------------------------------------------------------
-spec load_device_status/1 :: (#cb_context{}) -> #cb_context{}.
load_device_status(Context) ->
    AccountRealm = crossbar_util:get_account_realm(Context),
    crossbar_util:response(lookup_regs(AccountRealm), Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the complete list of registrations in a the first registrar
%% to respond for a given account realm.  This is not 100% accurate
%% as an endpoint might be stored in another registrar, but it is
%% accurate enough for the status icons.
%% @end
%%--------------------------------------------------------------------
-spec lookup_regs/1 :: (ne_binary()) -> wh_json:json_objects().
lookup_regs(AccountRealm) ->
    Q = amqp_util:new_queue(),
    ok = amqp_util:bind_q_to_targeted(Q),
    ok = amqp_util:basic_consume(Q),
    Req = [{<<"Realm">>, AccountRealm}
           ,{<<"Fields">>, [<<"Authorizing-ID">>]}
           | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
          ],
    wapi_registration:publish_query_req(Req),
    receive
        {_, #amqp_msg{payload = Payload}} ->
            JObj = wh_json:decode(Payload),
            true = wapi_registration:query_resp_v(JObj),
            case wh_json:get_value(<<"Fields">>, JObj) of
                undefined -> [];
                Regs ->
                    [wh_json:from_list([{<<"device_id">>, AuthorizingId}
                                        ,{<<"registered">>, true}
                                       ])
                     || Reg <- Regs
                            ,(AuthorizingId = wh_json:get_value(<<"Authorizing-ID">>, Reg)) =/= undefined
                    ]
            end
    after
        1000 -> []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if the device sip creds are unique
%% @end
%%--------------------------------------------------------------------
-spec is_sip_creds_unique/3 :: (undefined | ne_binary(), undefined | ne_binary(), undefined | ne_binary() ) -> boolean().
-spec is_sip_creds_unique/4 :: (undefined | ne_binary(), undefined | ne_binary(), undefined | ne_binary(), undefined | ne_binary()) 
                               -> boolean().

is_sip_creds_unique(AccountDb, Realm, Username) ->
    is_sip_creds_unique(AccountDb, Realm, Username, undefined).

%% no account id and no doc id (ie initial create with no account)
is_sip_creds_unique(undefined, _, _, undefined) ->
    true;
is_sip_creds_unique(_, _, undefined, undefined) ->
    true;
is_sip_creds_unique(AccountDb, undefined, Username, DocId) ->
    case couch_mgr:get_results(AccountDb, <<"devices/sip_credentials">>, [{<<"key">>, Username}]) of
        {ok, []} -> true;
        {ok, [JObj]} -> 
            wh_json:get_value(<<"id">>, JObj) =:= DocId;
        {error, not_found} -> true;
        _ -> false
    end;
is_sip_creds_unique(_, Realm, Username, DocId) ->
    ViewOptions = [{<<"key">>, [Realm, Username]}],
    case couch_mgr:get_results(?WH_SIP_DB, <<"credentials/lookup">>, ViewOptions) of
        {ok, []} -> true;
        {ok, [JObj]} -> wh_json:get_value(<<"id">>, JObj) =:= DocId;
        {error, not_found} -> true;
        _ -> false
    end.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% post data to a provisiong server
%% @end
%%--------------------------------------------------------------------
-spec do_simple_provision/1 :: (#cb_context{}) -> 'ok'.
do_simple_provision(#cb_context{doc=JObj}=Context) ->
    case whapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioning_url">>) of
        undefined -> ok;
        Url ->
            AccountRealm = crossbar_util:get_account_realm(Context),
            Headers = [{K, V}
                       || {K, V} <- [{"Host", whapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioning_host">>)}
                                     ,{"Referer", whapps_config:get_string(?MOD_CONFIG_CAT, <<"provisioning_referer">>)}
                                     ,{"User-Agent", wh_util:to_list(erlang:node())}
                                     ,{"Content-Type", "application/x-www-form-urlencoded"}]
                              ,V =/= undefined],
            HTTPOptions = [],
            Body = [{"device[mac]", re:replace(wh_json:get_string_value(<<"mac_address">>, JObj, ""), "[^0-9a-fA-F]", "", [{return, list}, global])}
                    ,{"device[label]", wh_json:get_string_value(<<"name">>, JObj)}
                    ,{"sip[realm]", wh_json:get_string_value([<<"sip">>, <<"realm">>], JObj, AccountRealm)}
                    ,{"sip[username]", wh_json:get_string_value([<<"sip">>, <<"username">>], JObj)}
                    ,{"sip[password]", wh_json:get_string_value([<<"sip">>, <<"password">>], JObj)}
                    ,{"submit", "true"}],
            Encoded = mochiweb_util:urlencode(Body),
            lager:debug("posting to ~s with ~s", [Url, Encoded]),
            ibrowse:send_req(Url, Headers, post, Encoded, HTTPOptions)
    end.

-spec maybe_update_acls/3 :: (ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
maybe_update_acls(DeviceIP, AcctId, DeviceId) ->
    CIDR = <<DeviceIP/binary, "/32">>,
    Acls = wh_json:set_value(CIDR
                             ,wh_json:from_list([{<<"network-list-name">>, <<"trusted">>}
                                                 ,{<<"type">>, <<"allow">>}
                                                 ,{<<"cidr">>, CIDR}
                                                 ,{<<"account_id">>, AcctId}
                                                 ,{<<"device_id">>, DeviceId}
                                                ])
                             ,get_acl_json()
                            ),
    lager:debug("setting ~s into system acls", [CIDR]),
    _ = whapps_config:set_default(<<"ecallmgr">>, <<"acls">>, Acls),
    wapi_switch:publish_reloadacl().

-spec get_acl_json/0 :: () -> wh_json:json_object().
get_acl_json() ->
    ACLs = whapps_config:get(<<"ecallmgr">>, <<"acls">>, wh_json:new()),
    case wh_json:is_json_object(ACLs) of
        true -> ACLs;
        false ->
            lager:debug("acls were not a json document: ~p", [ACLs]),
            wh_json:new()
    end.

set_outbound_flags(JObj, undefined) ->
    wh_json:set_value(<<"outbound_flags">>, [], JObj);
set_outbound_flags(JObj, []) ->
    JObj;
set_outbound_flags(JObj, <<>>) ->
    wh_json:set_value(<<"outbound_flags">>, [], JObj);
set_outbound_flags(JObj, L) when is_list(L) ->
    lager:debug("flags: ~p", [[wh_util:strip_binary(B) || B <- L]]),
    wh_json:set_value(<<"outbound_flags">>, [wh_util:strip_binary(B) || B <- L], JObj);
set_outbound_flags(JObj, _) -> JObj.
