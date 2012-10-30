%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
%%% @doc
%%% Various utilities specific to ecallmgr. More general utilities go
%%% in whistle_util.erl
%%% @end
%%%
%%% @contributors
%%%   James Aimonetti <james@2600hz.org>
%%%   Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_util).

-export([send_cmd/4]).
-export([get_expires/1]).
-export([get_interface_properties/1, get_interface_properties/2]).
-export([get_sip_to/1, get_sip_from/1, get_sip_request/1, get_orig_ip/1, custom_channel_vars/1]).
-export([eventstr_to_proplist/1, varstr_to_proplist/1, get_setting/1, get_setting/2]).
-export([is_node_up/1, is_node_up/2]).
-export([put_callid/1]).
-export([build_bridge_string/1, build_bridge_string/2]).
-export([build_channel/1]).
-export([create_masquerade_event/2, create_masquerade_event/3]).
-export([media_path/3, media_path/4, media_path/5]).
-export([unserialize_fs_array/1]).
-export([convert_fs_evt_name/1, convert_whistle_app_name/1]).
-export([fax_filename/1
         ,recording_filename/1
        ]).

-export([lookup_media/3, lookup_media/4, lookup_media/5]).

-include("ecallmgr.hrl").

-type send_cmd_ret() :: fs_sendmsg_ret() | fs_api_ret().
-export_type([send_cmd_ret/0]).

-record(bridge_endpoint, {invite_format = <<"username">> :: ne_binary()
                          ,endpoint_type = <<"sip">> :: ne_binary()
                          ,ip_address = undefined :: 'undefined' | ne_binary()
                          ,username = undefined :: 'undefined' | ne_binary()
                          ,realm = undefined :: 'undefined' | ne_binary()
                          ,number = undefined :: 'undefined' | ne_binary()
                          ,route = undefined :: 'undefined' | ne_binary()
                          ,proxy_address = undefined :: 'undefined' | ne_binary()
                          ,forward_address = undefined :: 'undefined' | ne_binary()
                          ,transport = undefined :: 'undefined' | ne_binary()
                          ,span = <<"1">> :: ne_binary()
                          ,channel_selection = <<"a">> :: ne_binary()
                          ,channel_vars = ["[",[],"]"] :: iolist()
                         }).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% send the SendMsg proplist to the freeswitch node
%% @end
%%--------------------------------------------------------------------
-spec send_cmd/4 :: (atom(), ne_binary(), ne_binary() | string(), ne_binary() | string()) -> send_cmd_ret().
send_cmd(Node, UUID, App, Args) when not is_list(App) ->
    send_cmd(Node, UUID, wh_util:to_list(App), Args);
send_cmd(Node, UUID, "xferext", Dialplan) ->
    XferExt = [begin
                   lager:debug("building xferext on node ~s: ~s", [Node, V]),
                   {wh_util:to_list(K), wh_util:to_list(V)}
               end || {K, V} <- Dialplan],
    ok = freeswitch:sendmsg(Node, UUID, [{"call-command", "xferext"} | XferExt]);
send_cmd(Node, UUID, App, Args) when not is_list(Args) ->
    send_cmd(Node, UUID, App, wh_util:to_list(Args));
send_cmd(Node, UUID, "record_call", Args) ->
    lager:debug("execute on node ~s: uuid_record(~s)", [Node, Args]),
    case freeswitch:api(Node, uuid_record, Args) of
        {ok, _Msg}=Ret ->
            lager:debug("executing uuid_record returned: ~s", [_Msg]),
            Ret;
        {error, <<"-ERR ", E/binary>>} ->
            lager:debug("error executing uuid_record: ~s", [E]),
            Evt = list_to_binary([ecallmgr_util:create_masquerade_event(<<"record_call">>, <<"RECORD_STOP">>)
                                  ,",whistle_application_response="
                                  ,"'",binary:replace(E, <<"\n">>, <<>>),"'"
                                 ]),
            lager:debug("publishing event: ~s", [Evt]),
            _ = send_cmd(Node, UUID, "application", Evt),
            {error, E};
        timeout ->
            lager:debug("timeout executing uuid_record"),
            Evt = list_to_binary([ecallmgr_util:create_masquerade_event(<<"record_call">>, <<"RECORD_STOP">>)
                                  ,",whistle_application_response=timeout"
                                 ]),
            lager:debug("publishing event: ~s", [Evt]),
            _ = send_cmd(Node, UUID, "application", Evt),
            {error, timeout}
    end;
send_cmd(Node, UUID, "playstop", _Args) ->
    lager:debug("execute on node ~s: uuid_break(~s all)", [Node, UUID]),
    freeswitch:api(Node, uuid_break, wh_util:to_list(<<UUID/binary, " all">>));
send_cmd(Node, UUID, "unbridge", _) ->
    lager:debug("execute on node ~s: uuid_park(~s)", [Node, UUID]),
    freeswitch:api(Node, uuid_park, wh_util:to_list(UUID));
send_cmd(Node, _UUID, "broadcast", Args) ->
    lager:debug("execute on node ~s: uuid_broadcast(~s)", [Node, Args]),
    Resp = freeswitch:api(Node, uuid_broadcast, wh_util:to_list(iolist_to_binary(Args))),
    lager:debug("broadcast resulted in: ~p", [Resp]),
    Resp;
send_cmd(Node, _UUID, "call_pickup", Args) ->
    lager:debug("execute on node ~s: uuid_bridge(~s)", [Node, Args]),
    freeswitch:api(Node, uuid_bridge, wh_util:to_list(Args));
send_cmd(Node, UUID, "hangup", _) ->
    lager:debug("terminate call on node ~s", [Node]),
    freeswitch:api(Node, uuid_kill, wh_util:to_list(UUID));
send_cmd(Node, UUID, "set", "ecallmgr_Account-ID=" ++ Value) ->
    send_cmd(Node, UUID, "export", "ecallmgr_Account-ID=" ++ Value);
send_cmd(Node, UUID, AppName, Args) ->
    lager:debug("execute on node ~s: ~s(~s)", [Node, AppName, Args]),
    case AppName of
        "set" -> maybe_update_channel_cache(Args, Node, UUID);
        "export" -> maybe_update_channel_cache(Args, Node, UUID);
        _Else -> ok
    end,
    freeswitch:sendmsg(Node, UUID, [{"call-command", "execute"}
                                    ,{"execute-app-name", AppName}
                                    ,{"execute-app-arg", wh_util:to_list(Args)}
                                   ]).

-spec maybe_update_channel_cache/3 :: (string(), atom(), ne_binary()) -> 'ok'.
maybe_update_channel_cache("ecallmgr_Account-ID=" ++ Value, Node, UUID) ->
    ecallmgr_fs_nodes:channel_set_account_id(Node, UUID, Value);
maybe_update_channel_cache("ecallmgr_Billing-ID=" ++ Value, Node, UUID) ->
    ecallmgr_fs_nodes:channel_set_billing_id(Node, UUID, Value);
maybe_update_channel_cache("ecallmgr_Account-Billing=" ++ Value, Node, UUID) ->
    ecallmgr_fs_nodes:channel_set_account_billing(Node, UUID, Value);
maybe_update_channel_cache("ecallmgr_Reseller-ID=" ++ Value, Node, UUID) ->
    ecallmgr_fs_nodes:channel_set_reseller_id(Node, UUID, Value);
maybe_update_channel_cache("ecallmgr_Reseller-Billing=" ++ Value, Node, UUID) ->
    ecallmgr_fs_nodes:channel_set_reseller_billing(Node, UUID, Value);
maybe_update_channel_cache("ecallmgr_Authorizing-ID=" ++ Value, Node, UUID) ->
    ecallmgr_fs_nodes:channel_set_authorizing_id(Node, UUID, Value);
maybe_update_channel_cache("ecallmgr_Resource-ID=" ++ Value, Node, UUID) ->
    ecallmgr_fs_nodes:channel_set_resource_id(Node, UUID, Value);
maybe_update_channel_cache("ecallmgr_Authorizing-Type=" ++ Value, Node, UUID) ->
    ecallmgr_fs_nodes:channel_set_authorizing_type(Node, UUID, Value);
maybe_update_channel_cache("ecallmgr_Owner-ID=" ++ Value, Node, UUID) ->
    ecallmgr_fs_nodes:channel_set_owner_id(Node, UUID, Value);
maybe_update_channel_cache("ecallmgr_Presence-ID=" ++ Value, Node, UUID) ->
    ecallmgr_fs_nodes:channel_set_presence_id(Node, UUID, Value);
maybe_update_channel_cache(_, _, _) ->
    ok.

-spec get_expires/1 :: (wh_proplist()) -> integer().
get_expires(Props) ->
    Expiry = wh_util:to_integer(props:get_value(<<"Expires">>, Props
                                                ,props:get_value(<<"expires">>, Props, 300))),
    round(Expiry * 1.25).

-spec get_interface_properties/1 :: (atom()) -> wh_proplist().
-spec get_interface_properties/2 :: (atom(), string() | ne_binary()) -> wh_proplist().

get_interface_properties(Node) ->
    get_interface_properties(Node, ?DEFAULT_FS_PROFILE).

get_interface_properties(Node, Interface) ->
    case freeswitch:api(Node, 'sofia', wh_util:to_list(list_to_binary(["status profile ", Interface]))) of
        {ok, Response} ->
            R = binary:replace(Response, <<" ">>, <<>>, [global]),
            [KV || Line <- binary:split(R, <<"\n">>, [global]),
                   (KV = case binary:split(Line, <<"\t">>) of
                             [K, V] -> {K, V};
                             _ -> false
                         end) =/= false
            ];
        _Else -> []
    end.

%% retrieves the sip address for the 'to' field
-spec get_sip_to/1 :: (wh_proplist()) -> ne_binary().
get_sip_to(Prop) ->
    list_to_binary([props:get_value(<<"sip_to_user">>, Prop
                                    ,props:get_value(<<"variable_sip_to_user">>, Prop, "nouser"))
                    ,"@"
                    ,props:get_value(<<"sip_to_host">>, Prop
                                      ,props:get_value(<<"variable_sip_to_host">>, Prop, ?DEFAULT_DOMAIN))
                   ]).

%% retrieves the sip address for the 'from' field
-spec get_sip_from/1 :: (wh_proplist()) -> ne_binary().
get_sip_from(Prop) ->
    list_to_binary([props:get_value(<<"sip_from_user">>, Prop
                                    ,props:get_value(<<"variable_sip_from_user">>, Prop, "nouser"))
                    ,"@"
                    ,props:get_value(<<"sip_from_host">>, Prop
                                      ,props:get_value(<<"variable_sip_from_host">>, Prop, ?DEFAULT_DOMAIN))
                   ]).

%% retrieves the sip address for the 'request' field
-spec get_sip_request/1 :: (wh_proplist()) -> ne_binary().
get_sip_request(Prop) ->
    list_to_binary([props:get_value(<<"Hunt-Destination-Number">>, Prop
                                    ,props:get_value(<<"Caller-Destination-Number">>, Prop, "nouser"))
                    ,"@"
                    ,props:get_value(list_to_binary(["variable_", ?CHANNEL_VAR_PREFIX, "Realm"]), Prop
                                                 ,props:get_value(<<"variable_sip_auth_realm">>, Prop, ?DEFAULT_DOMAIN))
                   ]).

-spec get_orig_ip/1 :: (wh_proplist()) -> ne_binary().
get_orig_ip(Prop) ->
    props:get_value(<<"X-AUTH-IP">>, Prop, props:get_value(<<"ip">>, Prop)).

%% Extract custom channel variables to include in the event
-spec custom_channel_vars/1 :: (wh_proplist()) -> wh_proplist().
custom_channel_vars(Prop) ->
    lists:foldl(fun({<<"variable_", ?CHANNEL_VAR_PREFIX, Key/binary>>, V}, Acc) -> [{Key, V} | Acc];
                   ({<<?CHANNEL_VAR_PREFIX, Key/binary>>, V}, Acc) -> [{Key, V} | Acc];
                   ({<<"variable_sip_h_Referred-By">>, V}, Acc) -> [{<<"Referred-By">>, wh_util:to_binary(mochiweb_util:unquote(V))} | Acc];
                   ({<<"variable_sip_refer_to">>, V}, Acc) -> [{<<"Referred-To">>, wh_util:to_binary(mochiweb_util:unquote(V))} | Acc];
                   (_, Acc) -> Acc
                end, [], Prop).

%% convert a raw FS string of headers to a proplist
%% "Event-Name: NAME\nEvent-Timestamp: 1234\n" -> [{<<"Event-Name">>, <<"NAME">>}, {<<"Event-Timestamp">>, <<"1234">>}]
-spec eventstr_to_proplist/1 :: (ne_binary() | nonempty_string()) -> wh_proplist().
eventstr_to_proplist(EvtStr) ->
    [to_kv(X, ": ") || X <- string:tokens(wh_util:to_list(EvtStr), "\n")].

-spec to_kv/2 :: (nonempty_string(), nonempty_string()) -> {ne_binary(), ne_binary()}.
to_kv(X, Separator) ->
    [K, V] = string:tokens(X, Separator),
    [{V1,[]}] = mochiweb_util:parse_qs(V),
    {wh_util:to_binary(K), wh_util:to_binary(fix_value(K, V1))}.

fix_value("Event-Date-Timestamp", TStamp) ->
    wh_util:microseconds_to_seconds(wh_util:to_integer(TStamp));
fix_value(_K, V) -> V.

-spec unserialize_fs_array/1 :: (api_binary()) -> [ne_binary(),...] | [].
unserialize_fs_array(undefined) -> [];
unserialize_fs_array(<<"ARRAY::", Serialized/binary>>) ->
    binary:split(Serialized, <<"|:">>, [global]);
unserialize_fs_array(_) -> [].

%% convert a raw FS list of vars  to a proplist
%% "Event-Name=NAME,Event-Timestamp=1234" -> [{<<"Event-Name">>, <<"NAME">>}, {<<"Event-Timestamp">>, <<"1234">>}]
-spec varstr_to_proplist/1 :: (nonempty_string()) -> wh_proplist().
varstr_to_proplist(VarStr) ->
    [to_kv(X, "=") || X <- string:tokens(wh_util:to_list(VarStr), ",")].

-spec get_setting/1 :: (wh_json:json_string()) -> {'ok', term()}.
-spec get_setting/2 :: (wh_json:json_string(), Default) -> {'ok', term() | Default}.
get_setting(Setting) ->
    get_setting(Setting, null).
get_setting(Setting, Default) ->
    {ok, ecallmgr_config:get(Setting, Default)}.

-spec is_node_up/1 :: (atom()) -> boolean().
is_node_up(Node) ->
    ecallmgr_fs_nodes:is_node_up(Node).

-spec is_node_up/2 :: (atom(), ne_binary()) -> boolean().
is_node_up(Node, UUID) ->
    ecallmgr_fs_nodes:is_node_up(Node) andalso ecallmgr_fs_nodes:channel_exists(UUID).

-spec put_callid/1 :: (wh_json:json_object()) -> 'undefined' | term().
put_callid(JObj) ->
    wh_util:put_callid(JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% takes endpoints (/sofia/foo/bar), and optionally a caller id name/num
%% and create the dial string ([origination_caller_id_name=Name
%%                              ,origination_caller_id_number=Num]Endpoint)
%% joined by the optional seperator.  Saves time by not spawning
%% endpoints with the invite format of "route" (about 100ms per endpoint)
%% @end
%%--------------------------------------------------------------------
-spec build_bridge_string/1 :: (wh_json:json_objects()) -> ne_binary().
-spec build_bridge_string/2 :: (wh_json:json_objects(), ne_binary()) -> ne_binary().

build_bridge_string(Endpoints) ->
    build_bridge_string(Endpoints, <<"|">>).

build_bridge_string(Endpoints, Seperator) ->
    KeyedEPs = [endpoint_jobj_to_record(Endpoint)
                || Endpoint <- Endpoints
                       ,wapi_dialplan:bridge_endpoint_v(Endpoint)
               ],
    %% De-dup the bridge strings by matching those with the same
    %%  Invite-Format, To-IP, To-User, To-realm, To-DID, and Route
    BridgeStrings = build_bridge_channels(KeyedEPs, []),
    %% NOTE: dont use binary_join here as it will crash on an empty list...
    wh_util:join_binary(lists:reverse(BridgeStrings), Seperator).

-spec endpoint_jobj_to_record/1 :: (wh_json:json_object()) -> #bridge_endpoint{}.
endpoint_jobj_to_record(Endpoint) -> 
    #bridge_endpoint{invite_format = wh_json:get_ne_value(<<"Invite-Format">>, Endpoint, <<"username">>)
                     ,endpoint_type = wh_json:get_ne_value(<<"Endpoint-Type">>, Endpoint, <<"sip">>)
                     ,ip_address = wh_json:get_ne_value(<<"To-IP">>, Endpoint)
                     ,username = wh_json:get_ne_value(<<"To-User">>, Endpoint)
                     ,realm = wh_json:get_ne_value(<<"To-Realm">>, Endpoint)
                     ,number = wh_json:get_ne_value(<<"To-DID">>, Endpoint)
                     ,route = wh_json:get_ne_value(<<"Route">>, Endpoint)
                     ,proxy_address = wh_json:get_ne_value(<<"Proxy-IP">>, Endpoint)
                     ,forward_address = wh_json:get_ne_value(<<"Forward-IP">>, Endpoint)
                     ,transport = wh_json:get_ne_value(<<"SIP-Transport">>, Endpoint)
                     ,span = get_endpoint_span(Endpoint)
                     ,channel_selection = get_endpoint_channel_selection(Endpoint)
                     ,channel_vars = ecallmgr_fs_xml:get_leg_vars(Endpoint)
                    }.

-spec get_endpoint_span/1 :: (wh_json:json_object()) -> ne_binary().
get_endpoint_span(Endpoint) ->
    wh_json:get_binary_value([<<"Endpoint-Options">>, <<"Span">>], Endpoint, <<"1">>).

-spec get_endpoint_channel_selection/1 :: (wh_json:json_object()) -> ne_binary().
get_endpoint_channel_selection(Endpoint) ->
    case wh_json:get_binary_value([<<"Endpoint-Options">>, <<"Span">>], Endpoint) of
        <<"descending">> -> <<"A">>;
        _Else -> <<"a">>
    end.

-type bridge_channel() :: ne_binary().
-type bridge_channels() :: [] | [bridge_channel(),...].
-type build_return() :: bridge_channel() | {'worker', pid()}.
-type bridge_endpoints() :: [#bridge_endpoint{},...] | [].
-spec build_bridge_channels/2 :: (bridge_endpoints(), [build_return(),...] | []) -> bridge_channels().

%% If the Invite-Format is "route" then we have been handed a sip route, do that now
build_bridge_channels([#bridge_endpoint{invite_format = <<"route">>}=Endpoint|Endpoints], Channels) ->
    case build_channel(Endpoint) of
        {error, _} -> build_bridge_channels(Endpoints, Channels);
        {ok, Channel} -> build_bridge_channels(Endpoints, [Channel|Channels])
    end;
%% If this does not have an explicted sip route and we have no ip address, lookup the registration
build_bridge_channels([#bridge_endpoint{ip_address = undefined}=Endpoint|Endpoints], Channels) ->
    S = self(),
    Pid = spawn(fun() -> S ! {self(), build_channel(Endpoint)} end),
    build_bridge_channels(Endpoints, [{worker, Pid}|Channels]);
%% If we have been given a IP to route to then do that now
build_bridge_channels([Endpoint|Endpoints], Channels) ->
    case build_channel(Endpoint) of
        {error, _} -> build_bridge_channels(Endpoints, Channels);
        {ok, Channel} -> build_bridge_channels(Endpoints, [Channel|Channels])
    end;
%% wait for any registration lookups to complete
build_bridge_channels([], IntermediateResults) ->
    lists:foldr(fun({worker, Pid}, Channels) ->
                        maybe_collect_worker_channel(Pid, Channels);
                 (Channel, Channels) -> 
                        [Channel|Channels]
                end, [], IntermediateResults).

-spec maybe_collect_worker_channel/2 :: (pid(), bridge_channels()) -> bridge_channels().
maybe_collect_worker_channel(Pid, Channels) ->
    receive
        {Pid, {error, _}} -> 
            Channels;
        {Pid, {ok, Channel}} ->
            [Channel|Channels]
    after
        2000 -> Channels
    end.

build_channel(#bridge_endpoint{endpoint_type = <<"freetdm">>}=Endpoint) ->
    build_freetdm_channel(Endpoint);
build_channel(#bridge_endpoint{endpoint_type = <<"sip">>}=Endpoint) ->
    build_sip_channel(Endpoint);
build_channel(EndpointJObj) ->
    build_channel(endpoint_jobj_to_record(EndpointJObj)).

-spec build_freetdm_channel/1 :: (#bridge_endpoint{}) -> {'ok', bridge_channel()} | {'error', _}.
build_freetdm_channel(#bridge_endpoint{number=undefined}) ->
    {error, number_not_provided};
build_freetdm_channel(#bridge_endpoint{invite_format = <<"e164">>, number=Number
                                                ,span=Span, channel_selection=ChannelSelection}) ->
    {ok, <<"freetdm/", Span/binary, "/", ChannelSelection/binary, "/", (wnm_util:to_e164(Number))/binary>>};
build_freetdm_channel(#bridge_endpoint{invite_format = <<"npan">>, number=Number
                                                ,span=Span, channel_selection=ChannelSelection}) ->
    {ok, <<"freetdm/", Span/binary, "/", ChannelSelection/binary, "/", (wnm_util:to_npan(Number))/binary>>};
build_freetdm_channel(#bridge_endpoint{invite_format = <<"1npan">>, number=Number
                                                ,span=Span, channel_selection=ChannelSelection}) ->
    {ok, <<"freetdm/", Span/binary, "/", ChannelSelection/binary, "/", (wnm_util:to_1npan(Number))/binary>>};
build_freetdm_channel(#bridge_endpoint{number=Number, span=Span, channel_selection=ChannelSelection}) ->
    {ok, <<"freetdm/", Span/binary, "/", ChannelSelection/binary, "/", Number/binary>>}.

-spec build_sip_channel/1 :: (#bridge_endpoint{}) -> {'ok', bridge_channel()} | {'error', _}.
build_sip_channel(Endpoint) ->
    Routines = [fun(C) -> maybe_clean_contact(C, Endpoint) end
                ,fun(C) -> ensure_username_present(C, Endpoint) end
                ,fun(C) -> maybe_replace_fs_path(C, Endpoint) end
                ,fun(C) -> maybe_replace_transport(C, Endpoint) end
                ,fun(C) -> maybe_format_user(C, Endpoint) end
                ,fun(C) -> maybe_set_interface(C, Endpoint) end
                ,fun(C) -> append_channel_vars(C, Endpoint) end
               ],
    try lists:foldl(fun(F, C) -> F(C) end, get_sip_contact(Endpoint), Routines) of
        Channel -> {ok, Channel}
    catch
        _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:warning("Failed to build sip channel (~s): ~p", [_E, _R]),
            lager:warning("stacktrace:"),
            _ = [lager:warning("~p", [Line]) || Line <- ST],
            {error, invalid}
    end.

-spec get_sip_contact/1 :: (#bridge_endpoint{}) -> ne_binary().
get_sip_contact(#bridge_endpoint{invite_format = <<"route">>, route=Route}) ->
    Route;
get_sip_contact(#bridge_endpoint{ip_address=undefined, realm=Realm, username=Username}) ->
    {ok, Contact} = ecallmgr_registrar:lookup_contact(Realm, Username),
    binary:replace(Contact, <<">">>, <<>>);
get_sip_contact(#bridge_endpoint{ip_address=IPAddress}) ->
    IPAddress.

-spec maybe_clean_contact/2 :: (ne_binary(), #bridge_endpoint{}) -> ne_binary().
maybe_clean_contact(<<"sip:", Contact/binary>>, Endpoint) ->
    maybe_clean_contact(Contact, Endpoint);
maybe_clean_contact(Contact, #bridge_endpoint{invite_format = <<"route">>}) ->
    Contact;
maybe_clean_contact(Contact, _) ->
    re:replace(Contact, <<"^.*?[^=]sip:">>, <<>>, [{return, binary}]).

-spec ensure_username_present/2 :: (ne_binary(), #bridge_endpoint{}) -> ne_binary().
ensure_username_present(Contact, Endpoint) ->
    case binary:split(Contact, <<"@">>) of
        [_, _] -> Contact;
        _ ->
            <<(guess_username(Endpoint))/binary, "@", Contact/binary>>
    end.    

-spec guess_username/1 :: (#bridge_endpoint{}) -> ne_binary().
guess_username(#bridge_endpoint{number=Number}) when is_binary(Number) ->
    Number;
guess_username(#bridge_endpoint{username=Username}) when is_binary(Username) ->
    Username;
guess_username(_) ->
    <<"kazoo">>.

-spec maybe_replace_fs_path/2 :: (ne_binary(), #bridge_endpoint{}) -> ne_binary().
maybe_replace_fs_path(Contact, #bridge_endpoint{proxy_address=undefined}) ->
    Contact;
maybe_replace_fs_path(Contact, #bridge_endpoint{proxy_address = <<"sip:", _/binary>> = Proxy}) ->
    case re:replace(Contact, <<";fs_path=[^;?]*">>, <<";fs_path=", Proxy/binary>>, [{return, binary}]) of
        Contact -> 
            %% NOTE: this will be invalid if the channel has headers, see rfc3261 19.1.1
            <<Contact/binary, ";fs_path=", Proxy/binary>>;
        Updated -> Updated
    end;
maybe_replace_fs_path(Contact, #bridge_endpoint{proxy_address=Proxy}=Endpoint) ->
    maybe_replace_fs_path(Contact, Endpoint#bridge_endpoint{proxy_address = <<"sip:", Proxy/binary>>}).

-spec maybe_replace_transport/2 :: (ne_binary(), #bridge_endpoint{}) -> ne_binary().
maybe_replace_transport(Contact, #bridge_endpoint{transport=undefined}) ->
    Contact;
maybe_replace_transport(Contact, #bridge_endpoint{transport=Transport}) ->
    case re:replace(Contact, <<";transport=[^;?]*">>, <<";transport=", Transport/binary>>, [{return, binary}]) of
        Contact -> 
            %% NOTE: this will be invalid if the channel has headers, see rfc3261 19.1.1
            <<Contact/binary, ";transport=", Transport/binary>>;
        Updated -> Updated
    end.

-spec maybe_format_user/2 :: (ne_binary(), #bridge_endpoint{}) -> ne_binary().
maybe_format_user(Contact, #bridge_endpoint{invite_format = <<"username">>, username=undefined}) ->
    Contact;
maybe_format_user(Contact, #bridge_endpoint{invite_format = <<"username">>, username=Username}) ->
    re:replace(Contact, "^[^\@]+", Username, [{return, binary}]);
maybe_format_user(Contact, #bridge_endpoint{number=undefined}) ->
    Contact;
maybe_format_user(Contact, #bridge_endpoint{invite_format = <<"e164">>, number=Number}) ->
    re:replace(Contact, "^[^\@]+", wnm_util:to_e164(Number), [{return, binary}]);
maybe_format_user(Contact, #bridge_endpoint{invite_format = <<"npan">>, number=Number}) ->
    re:replace(Contact, "^[^\@]+", wnm_util:to_npan(Number), [{return, binary}]);
maybe_format_user(Contact, #bridge_endpoint{invite_format = <<"1npan">>, number=Number}) ->
    re:replace(Contact, "^[^\@]+", wnm_util:to_1npan(Number), [{return, binary}]);
maybe_format_user(Contact, _) ->
    Contact.

-spec maybe_set_interface/2 :: (ne_binary(), #bridge_endpoint{}) -> ne_binary().
maybe_set_interface(<<"sofia/", _/binary>>=Contact, _) ->
    Contact;
maybe_set_interface(Contact, _) ->
    <<?SIP_INTERFACE, Contact/binary>>.

-spec append_channel_vars/2 :: (ne_binary(), #bridge_endpoint{}) -> ne_binary().
append_channel_vars(Contact, #bridge_endpoint{channel_vars=["[",[],"]"]}) ->
    false = wh_util:is_empty(Contact),
    Contact;
append_channel_vars(Contact, #bridge_endpoint{channel_vars=ChannelVars}) ->
    false = wh_util:is_empty(Contact),
    list_to_binary([ChannelVars, Contact]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_masquerade_event/2 :: (ne_binary(), ne_binary()) -> ne_binary().
create_masquerade_event(Application, EventName) ->
    create_masquerade_event(Application, EventName, true).

create_masquerade_event(Application, EventName, Boolean) ->
    Prefix = case Boolean of
                 true -> <<"event ">>;
                 false -> <<>>
             end,
    <<Prefix/binary, "Event-Name=CUSTOM,Event-Subclass=whistle::masquerade"
      ,",whistle_event_name=", EventName/binary
      ,",whistle_application_name=", Application/binary>>.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec media_path/3 :: (ne_binary(), ne_binary(), wh_json:json_object()) -> ne_binary().
-spec media_path/4 :: (ne_binary(), 'extant' | 'new', ne_binary(), wh_json:json_object()) -> ne_binary().
-spec media_path/5 :: (ne_binary(), 'extant' | 'new', ne_binary(), wh_json:json_object(), atom()) -> ne_binary().
media_path(MediaName, UUID, JObj) ->
    media_path(MediaName, new, UUID, JObj).
media_path(MediaName, Type, UUID, JObj) ->
    media_path(MediaName, Type, UUID, JObj, undefined).

media_path(undefined, _Type, _UUID, _, _) ->
    <<"silence_stream://5">>;
media_path(MediaName, Type, UUID, JObj, Cache) when not is_binary(MediaName) ->
    media_path(wh_util:to_binary(MediaName), Type, UUID, JObj, Cache);
media_path(<<"silence_stream://", _/binary>> = Media, _Type, _UUID, _, _) ->
    Media;
media_path(<<"tone_stream://", _/binary>> = Media, _Type, _UUID, _, _) ->
    Media;
media_path(<<"local_stream://", FSPath/binary>>, _Type, _UUID, _, _) ->
    FSPath;
media_path(<<"http://", _/binary>> = URI, _Type, _UUID, _, _) ->
    get_fs_playback(URI);
media_path(MediaName, Type, UUID, JObj, Cache) ->
    case lookup_media(MediaName, UUID, JObj, Type, Cache) of
        {'error', _E} ->
            lager:debug("failed to get media ~s: ~p", [MediaName, _E]),
            wh_util:to_binary(MediaName);
        {ok, Url} ->
            lager:debug("recevied URL: ~s", [Url]),
            wh_util:to_binary(get_fs_playback(Url))
    end.

-spec fax_filename/1 :: (ne_binary()) -> file:filename().
fax_filename(UUID) ->
    Ext = ecallmgr_config:get(<<"default_fax_extension">>, <<".tiff">>),
    filename:join([ecallmgr_config:get(<<"fax_file_path">>, <<"/tmp/">>)
                   ,<<(amqp_util:encode(UUID))/binary, Ext/binary>>
                  ]).

-spec recording_filename/1 :: (ne_binary()) -> file:filename().
recording_filename(<<"local_stream://", MediaName/binary>>) ->
    recording_filename(MediaName);
recording_filename(MediaName) ->
    Ext = recording_extension(MediaName),
    RootName = filename:basename(MediaName, Ext),
    Directory = recording_directory(MediaName),

    filename:join([Directory
                   ,<<(amqp_util:encode(RootName))/binary, Ext/binary>>
                  ]).

recording_directory(<<"/", _/binary>> = FullPath) ->
    filename:dirname(FullPath);
recording_directory(_RelativePath) ->
    ecallmgr_config:get(<<"recording_file_path">>, <<"/tmp/">>).

recording_extension(MediaName) ->
    case filename:extension(MediaName) of
        Empty when Empty =:= <<>> orelse Empty =:= [] ->
            ecallmgr_config:get(<<"default_recording_extension">>, <<".mp3">>);
        <<".mp3">> = MP3 -> MP3;
        <<".wav">> = WAV -> WAV;
        _ -> <<".mp3">>
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_fs_playback/1 :: (ne_binary()) -> ne_binary().
get_fs_playback(<<"http://", _/binary>>=Url) ->
    <<"${http_get(", Url/binary, ")}">>;
get_fs_playback(Url) ->
    Url.

%% given a proplist of a FS event, return the Whistle-equivalent app name(s).
%% a FS event could have multiple Whistle equivalents
-spec convert_fs_evt_name/1 :: (ne_binary()) -> [ne_binary(),...] | [].
convert_fs_evt_name(EvtName) ->
    [ WhAppEvt || {FSEvt, WhAppEvt} <- ?FS_APPLICATION_NAMES, FSEvt =:= EvtName].

%% given a Whistle Dialplan Application name, return the FS-equivalent event name
%% A Whistle Dialplan Application name is 1-to-1 with the FS-equivalent
-spec convert_whistle_app_name/1 :: (ne_binary()) -> [ne_binary(),...] | [].
convert_whistle_app_name(App) ->
    [EvtName || {EvtName, AppName} <- ?FS_APPLICATION_NAMES, App =:= AppName].

-spec lookup_media/3 :: (ne_binary(), ne_binary(), wh_json:json_object()) ->
                                {'ok', binary()} |
                                {'error', any()}.
-spec lookup_media/4 :: (ne_binary(), ne_binary(), wh_json:json_object(), 'new' | 'extant') ->
                                {'ok', binary()} |
                                {'error', any()}.
-spec lookup_media/5 :: (ne_binary(), ne_binary(), wh_json:json_object(), 'new' | 'extant', atom()) ->
                                {'ok', binary()} |
                                {'error', any()}.
lookup_media(MediaName, CallId, JObj) ->
    lookup_media(MediaName, CallId, JObj, new).
lookup_media(MediaName, CallId, JObj, Type) ->
    lookup_media(MediaName, CallId, JObj, Type, undefined).
lookup_media(MediaName, CallId, JObj, Type, undefined) when Type =:= new orelse Type =:= extant ->
    Request = wh_json:set_values(
                props:filter_undefined(
                  [{<<"Media-Name">>, MediaName}
                   ,{<<"Stream-Type">>, Type}
                   ,{<<"Call-ID">>, CallId}
                   ,{<<"Msg-ID">>, wh_util:to_binary(wh_util:current_tstamp())}
                   | wh_api:default_headers(<<"media">>, <<"media_req">>, ?APP_NAME, ?APP_VERSION)
                  ])
                ,JObj),
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,Request
                                  ,fun wapi_media:publish_req/1
                                  ,fun wapi_media:resp_v/1
                                 ),
    case ReqResp of
        {error, _R}=E ->
            lager:debug("media lookup for '~s' failed: ~p", [MediaName, _R]),
            E;
        {ok, MediaResp} ->
            MediaName = wh_json:get_value(<<"Media-Name">>, MediaResp),
            {ok, wh_json:get_value(<<"Stream-URL">>, MediaResp, <<>>)}
    end;
lookup_media(MediaName, CallId, JObj, Type, Cache) ->
    RecordingName = recording_filename(MediaName),
    lager:debug("see if ~s is in cache ~s", [RecordingName, Cache]),
    case wh_cache:peek_local(Cache, ?ECALLMGR_RECORDED_MEDIA_KEY(RecordingName)) of
        {ok, _} -> {ok, RecordingName};
        {error, not_found} -> lookup_media(MediaName, CallId, JObj, Type, undefined)
    end.
