%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
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
-export([get_fs_kv/3]).
-export([set/3]).
-export([export/3]).
-export([get_expires/1]).
-export([get_interface_properties/1, get_interface_properties/2]).
-export([get_sip_to/1, get_sip_from/1, get_sip_request/1, get_orig_ip/1]).
-export([custom_channel_vars/1, custom_channel_vars/2]).
-export([eventstr_to_proplist/1, varstr_to_proplist/1, get_setting/1, get_setting/2]).
-export([is_node_up/1, is_node_up/2]).
-export([build_bridge_string/1, build_bridge_string/2]).
-export([build_channel/1]).
-export([build_simple_channels/1]).
-export([create_masquerade_event/2, create_masquerade_event/3]).
-export([media_path/3, media_path/4]).
-export([unserialize_fs_array/1]).
-export([convert_fs_evt_name/1, convert_whistle_app_name/1]).
-export([fax_filename/1
         ,recording_filename/1
        ]).
-export([maybe_sanitize_fs_value/2]).
-export([lookup_media/4]).

-include("ecallmgr.hrl").

-type send_cmd_ret() :: fs_sendmsg_ret() | fs_api_ret().
-export_type([send_cmd_ret/0]).

-record(bridge_endpoint, {invite_format = <<"username">> :: ne_binary()
                          ,endpoint_type = <<"sip">> :: ne_binary()
                          ,ip_address :: api_binary()
                          ,username :: api_binary()
                          ,user :: api_binary()
                          ,realm :: api_binary()
                          ,number :: api_binary()
                          ,route :: api_binary()
                          ,proxy_address :: api_binary()
                          ,forward_address :: api_binary()
                          ,transport :: api_binary()
                          ,span = <<"1">> :: ne_binary()
                          ,channel_selection = <<"a">> :: ne_binary()
                          ,interface = <<"RR">> :: ne_binary() % for Skype
                          ,sip_interface
                          ,channel_vars = ["[",[],"]"] :: iolist()
                          ,include_channel_vars = 'true' :: boolean()
                         }).
-type bridge_endpoint() :: #bridge_endpoint{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% send the SendMsg proplist to the freeswitch node
%% @end
%%--------------------------------------------------------------------
-spec send_cmd(atom(), ne_binary(), text(), text()) -> send_cmd_ret().
send_cmd(Node, UUID, App, Args) when not is_list(App) ->
    send_cmd(Node, UUID, wh_util:to_list(App), Args);
send_cmd(Node, UUID, "xferext", Dialplan) ->
    XferExt = [begin
                   lager:debug("building xferext on node ~s: ~s", [Node, V]),
                   {wh_util:to_list(K), wh_util:to_list(V)}
               end || {K, V} <- Dialplan],
    'ok' = freeswitch:sendmsg(Node, UUID, [{"call-command", "xferext"} | XferExt]);
send_cmd(Node, UUID, App, Args) when not is_list(Args) ->
    send_cmd(Node, UUID, App, wh_util:to_list(Args));
send_cmd(Node, UUID, "record_call", Args) ->
    lager:debug("execute on node ~s: uuid_record(~s)", [Node, Args]),
    case freeswitch:api(Node, 'uuid_record', Args) of
        {'ok', _Msg}=Ret ->
            lager:debug("executing uuid_record returned: ~s", [_Msg]),
            Ret;
        {'error', <<"-ERR ", E/binary>>} ->
            lager:debug("error executing uuid_record: ~s", [E]),
            Evt = list_to_binary([ecallmgr_util:create_masquerade_event(<<"record_call">>, <<"RECORD_STOP">>)
                                  ,",whistle_application_response="
                                  ,"'",binary:replace(E, <<"\n">>, <<>>),"'"
                                 ]),
            lager:debug("publishing event: ~s", [Evt]),
            _ = send_cmd(Node, UUID, "application", Evt),
            {'error', E};
        'timeout' ->
            lager:debug("timeout executing uuid_record"),
            Evt = list_to_binary([ecallmgr_util:create_masquerade_event(<<"record_call">>, <<"RECORD_STOP">>)
                                  ,",whistle_application_response=timeout"
                                 ]),
            lager:debug("publishing event: ~s", [Evt]),
            _ = send_cmd(Node, UUID, "application", Evt),
            {'error', 'timeout'}
    end;
send_cmd(Node, UUID, "playstop", _Args) ->
    lager:debug("execute on node ~s: uuid_break(~s all)", [Node, UUID]),
    freeswitch:api(Node, 'uuid_break', wh_util:to_list(<<UUID/binary, " all">>));
send_cmd(Node, UUID, "unbridge", _) ->
    lager:debug("execute on node ~s: uuid_park(~s)", [Node, UUID]),
    freeswitch:api(Node, 'uuid_park', wh_util:to_list(UUID));
send_cmd(Node, _UUID, "broadcast", Args) ->
    lager:debug("execute on node ~s: uuid_broadcast(~s)", [Node, Args]),
    Resp = freeswitch:api(Node, 'uuid_broadcast', wh_util:to_list(iolist_to_binary(Args))),
    lager:debug("broadcast resulted in: ~p", [Resp]),
    Resp;
send_cmd(Node, _UUID, "call_pickup", Args) ->
    lager:debug("execute on node ~s: uuid_bridge(~s)", [Node, Args]),
    freeswitch:api(Node, 'uuid_bridge', wh_util:to_list(Args));
send_cmd(Node, UUID, "hangup", _) ->
    lager:debug("terminate call on node ~s", [Node]),
    freeswitch:api(Node, 'uuid_kill', wh_util:to_list(UUID));
send_cmd(Node, UUID, "conference", Args) ->
    Args1 = iolist_to_binary([UUID, " conference:", Args, ",park inline"]),
    lager:debug("starting conference on ~s: ~s", [Node, Args1]),
    freeswitch:api(Node, 'uuid_transfer', wh_util:to_list(Args1));
send_cmd(Node, UUID, AppName, Args) ->
    Result = freeswitch:sendmsg(Node, UUID, [{"call-command", "execute"}
                                             ,{"execute-app-name", AppName}
                                             ,{"execute-app-arg", wh_util:to_list(Args)}
                                            ]),
    lager:debug("execute on node ~s ~s(~s): ~p", [Node, AppName, Args, Result]),
    Result.

-spec get_expires(wh_proplist()) -> integer().
get_expires(Props) ->
    Expiry = wh_util:to_integer(props:get_value(<<"Expires">>, Props
                                                ,props:get_value(<<"expires">>, Props, 300))),
    round(Expiry * 1.25).

-spec get_interface_properties(atom()) -> wh_proplist().
-spec get_interface_properties(atom(), text()) -> wh_proplist().

get_interface_properties(Node) ->
    get_interface_properties(Node, ?DEFAULT_FS_PROFILE).

get_interface_properties(Node, Interface) ->
    case freeswitch:api(Node, 'sofia', wh_util:to_list(list_to_binary(["status profile ", Interface]))) of
        {'ok', Response} ->
            R = binary:replace(Response, <<" ">>, <<>>, ['global']),
            [KV || Line <- binary:split(R, <<"\n">>, ['global']),
                   (KV = case binary:split(Line, <<"\t">>) of
                             [K, V] -> {K, V};
                             _ -> 'false'
                         end) =/= 'false'
            ];
        _Else -> []
    end.

%% retrieves the sip address for the 'to' field
-spec get_sip_to(wh_proplist()) -> ne_binary().
get_sip_to(Props) ->
    get_sip_to(Props, props:get_value(<<"Call-Direction">>, Props)).

get_sip_to(Props, <<"outbound">>) ->
    case props:get_value(<<"Channel-Presence-ID">>, Props) of
        'undefined' -> get_sip_request(Props);
        PresenceId -> PresenceId
    end;
get_sip_to(Props, _) ->
    case props:get_value(<<"variable_sip_to_uri">>, Props) of
        'undefined' -> get_sip_request(Props);
        ToUri -> ToUri
    end.

%% retrieves the sip address for the 'from' field
-spec get_sip_from(wh_proplist()) -> ne_binary().
get_sip_from(Props) ->
    get_sip_from(Props, props:get_value(<<"Call-Direction">>, Props)).

get_sip_from(Props, <<"outbound">>) ->
    case props:get_value(<<"Other-Leg-Channel-Name">>, Props) of
        'undefined' ->
            Number = props:get_value(<<"Other-Leg-Caller-ID-Number">>, Props, <<"nouser">>),
            Realm = props:get_first_defined([?GET_CCV(<<"Realm">>)
                                             ,<<"variable_sip_auth_realm">>
                                            ], Props, ?DEFAULT_REALM),
            props:get_value(<<"variable_sip_from_uri">>, Props,
                            <<Number/binary, "@", Realm/binary>>);
        OtherChannel ->
            lists:last(binary:split(OtherChannel, <<"/">>, ['global']))
    end;
get_sip_from(Props, _) ->
    Default = <<(props:get_value(<<"sip_from_user">>, Props, <<"nouser">>))/binary
                ,"@"
                ,(props:get_value(<<"sip_from_host">>, Props, ?DEFAULT_REALM))/binary
              >>,
    props:get_first_defined([<<"Channel-Presence-ID">>
                             ,<<"variable_sip_from_uri">>
                            ], Props, Default).

%% retrieves the sip address for the 'request' field
-spec get_sip_request(wh_proplist()) -> ne_binary().
get_sip_request(Props) ->
    User = props:get_first_defined([<<"Hunt-Destination-Number">>
                                    ,<<"Caller-Destination-Number">>
                                    ,<<"sip_to_user">>
                                   ], Props, <<"nouser">>),
    Realm = props:get_first_defined([?GET_CCV(<<"Realm">>)
                                     ,<<"variable_sip_auth_realm">>
                                     ,<<"sip_to_host">>
                                    ], Props, ?DEFAULT_REALM),
    <<User/binary, "@", Realm/binary>>.

-spec get_orig_ip(wh_proplist()) -> ne_binary().
get_orig_ip(Prop) ->
    props:get_value(<<"X-AUTH-IP">>, Prop, props:get_value(<<"ip">>, Prop)).

%% Extract custom channel variables to include in the event
-spec custom_channel_vars(wh_proplist()) -> wh_proplist().
-spec custom_channel_vars(wh_proplist(), wh_proplist()) -> wh_proplist().
custom_channel_vars(Props) ->
    custom_channel_vars(Props, []).

custom_channel_vars(Props, Initial) ->
    lists:foldl(fun({<<"variable_", ?CHANNEL_VAR_PREFIX, Key/binary>>, V}, Acc) -> [{Key, V} | Acc];
                   ({<<?CHANNEL_VAR_PREFIX, Key/binary>>, V}, Acc) -> [{Key, V} | Acc];
                   ({<<"variable_sip_h_Referred-By">>, V}, Acc) -> [{<<"Referred-By">>, wh_util:to_binary(mochiweb_util:unquote(V))} | Acc];
                   ({<<"variable_sip_refer_to">>, V}, Acc) -> [{<<"Referred-To">>, wh_util:to_binary(mochiweb_util:unquote(V))} | Acc];
                   (_, Acc) -> Acc
                end, Initial, Props).

%% convert a raw FS string of headers to a proplist
%% "Event-Name: NAME\nEvent-Timestamp: 1234\n" -> [{<<"Event-Name">>, <<"NAME">>}, {<<"Event-Timestamp">>, <<"1234">>}]
-spec eventstr_to_proplist(text()) -> wh_proplist().
eventstr_to_proplist(EvtStr) ->
    [to_kv(X, ": ") || X <- string:tokens(wh_util:to_list(EvtStr), "\n")].

-spec to_kv(nonempty_string(), nonempty_string()) -> {ne_binary(), ne_binary()}.
to_kv(X, Separator) ->
    [K, V] = string:tokens(X, Separator),
    [{V1,[]}] = mochiweb_util:parse_qs(V),
    {wh_util:to_binary(K), wh_util:to_binary(fix_value(K, V1))}.

fix_value("Event-Date-Timestamp", TStamp) ->
    wh_util:microseconds_to_seconds(wh_util:to_integer(TStamp));
fix_value(_K, V) -> V.

-spec unserialize_fs_array(api_binary()) -> ne_binaries().
unserialize_fs_array('undefined') -> [];
unserialize_fs_array(<<"ARRAY::", Serialized/binary>>) ->
    binary:split(Serialized, <<"|:">>, ['global']);
unserialize_fs_array(_) -> [].

%% convert a raw FS list of vars  to a proplist
%% "Event-Name=NAME,Event-Timestamp=1234" -> [{<<"Event-Name">>, <<"NAME">>}, {<<"Event-Timestamp">>, <<"1234">>}]
-spec varstr_to_proplist(nonempty_string()) -> wh_proplist().
varstr_to_proplist(VarStr) ->
    [to_kv(X, "=") || X <- string:tokens(wh_util:to_list(VarStr), ",")].

-spec get_setting(wh_json:key()) -> {'ok', term()}.
-spec get_setting(wh_json:key(), Default) -> {'ok', term() | Default}.
get_setting(Setting) -> {'ok', ecallmgr_config:get(Setting)}.
get_setting(Setting, Default) -> {'ok', ecallmgr_config:get(Setting, Default)}.

-spec is_node_up(atom()) -> boolean().
is_node_up(Node) -> ecallmgr_fs_nodes:is_node_up(Node).

-spec is_node_up(atom(), ne_binary()) -> boolean().
is_node_up(Node, UUID) ->
    ecallmgr_fs_nodes:is_node_up(Node) andalso ecallmgr_fs_channel:exists(UUID).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% set channel and call variables in FreeSWITCH
%% @end
%%--------------------------------------------------------------------
-spec get_fs_kv(ne_binary(), ne_binary(), ne_binary()) -> binary().
get_fs_kv(<<"Hold-Media">>, Media, UUID) ->
    list_to_binary(["hold_music="
                    ,wh_util:to_list(media_path(Media, 'extant', UUID, wh_json:new()))
                   ]);
get_fs_kv(Key, Val, _) ->
    case lists:keyfind(Key, 1, ?SPECIAL_CHANNEL_VARS) of
        'false' ->
            list_to_binary([?CHANNEL_VAR_PREFIX, wh_util:to_list(Key), "=", wh_util:to_list(Val)]);
        {_, Prefix} ->
            V = maybe_sanitize_fs_value(Key, Val),
            list_to_binary([Prefix, "=", wh_util:to_list(V), ""])
    end.

-spec get_fs_key_and_value(ne_binary(), ne_binary(), ne_binary()) -> {ne_binary(), binary()}.
get_fs_key_and_value(<<"Hold-Media">>, Media, UUID) ->
    {<<"hold_music">>, media_path(Media, 'extant', UUID, wh_json:new())};
get_fs_key_and_value(Key, Val, _UUID) ->
    case lists:keyfind(Key, 1, ?SPECIAL_CHANNEL_VARS) of
        'false' ->
            {list_to_binary([?CHANNEL_VAR_PREFIX, Key]), Val};
        {_, Prefix} ->
            {Prefix, maybe_sanitize_fs_value(Key, Val)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_sanitize_fs_value(text(), text()) -> binary().
maybe_sanitize_fs_value(<<"Outbound-Caller-ID-Name">>, Val) ->
    re:replace(Val, <<"[^a-zA-Z0-9\s]">>, <<>>, ['global', {'return', 'binary'}]);
maybe_sanitize_fs_value(<<"Outbound-Callee-ID-Name">>, Val) ->
    re:replace(Val, <<"[^a-zA-Z0-9\s]">>, <<>>, ['global', {'return', 'binary'}]);
maybe_sanitize_fs_value(<<"Caller-ID-Name">>, Val) ->
    re:replace(Val, <<"[^a-zA-Z0-9\s]">>, <<>>, ['global', {'return', 'binary'}]);
maybe_sanitize_fs_value(<<"Callee-ID-Name">>, Val) ->
    re:replace(Val, <<"[^a-zA-Z0-9\s]">>, <<>>, ['global', {'return', 'binary'}]);
maybe_sanitize_fs_value(Key, Val) when not is_binary(Key) ->
    maybe_sanitize_fs_value(wh_util:to_binary(Key), Val);
maybe_sanitize_fs_value(Key, Val) when not is_binary(Val) ->
    maybe_sanitize_fs_value(Key, wh_util:to_binary(Val));
maybe_sanitize_fs_value(_, Val) -> Val.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set(atom(), ne_binary(), wh_proplist()) ->
                 ecallmgr_util:send_cmd_ret().
set(_, _, []) -> 'ok';
set(Node, UUID, [{<<"Auto-Answer", _/binary>> = K, V}]) ->
    ecallmgr_fs_command:set(Node, UUID, [{<<"alert_info">>, <<"intercom">>}, get_fs_key_and_value(K, V, UUID)]);
set(Node, UUID, [{<<"Hold-Media">>, Value}]) ->
    Media = media_path(Value, 'extant', UUID, wh_json:new()),
    AppArg = wh_util:to_list(<<"hold_music=", Media/binary>>),
    %% NOTE: due to how we handle hold_music we need to export
    %%    this var rather than set...
    _ = freeswitch:sendmsg(Node, UUID, [{"call-command", "execute"}
                                        ,{"execute-app-name", "export"}
                                        ,{"execute-app-arg", AppArg}
                                       ]),
    'ok';
set(Node, UUID, [{K, V}]) ->
    ecallmgr_fs_command:set(Node, UUID, [get_fs_key_and_value(K, V, UUID)]);
set(Node, UUID, [{_, _}|_]=Props) ->
    Multiset = lists:foldl(fun(Prop, Acc) ->
                              set_fold(Node, UUID, Prop, Acc)
                           end, [], Props),
    ecallmgr_fs_command:set(Node, UUID, Multiset).

set_fold(Node, UUID, {<<"Hold-Media">>, Value}, Acc) ->
    Media = media_path(Value, 'extant', UUID, wh_json:new()),
    AppArg = wh_util:to_list(<<"hold_music=", Media/binary>>),
    %% NOTE: due to how we handle hold_music we need to export
    %%    this var rather than set...
    _ = freeswitch:sendmsg(Node, UUID, [{"call-command", "execute"}
                                        ,{"execute-app-name", "export"}
                                        ,{"execute-app-arg", AppArg}
                                       ]),
    Acc;
set_fold(_, UUID, {<<"Auto-Answer", _/binary>> = K, V}, Acc) ->
    [{<<"alert_info">>, <<"intercom">>}
     ,get_fs_key_and_value(K, V, UUID)
     | Acc
    ];
set_fold(Node, UUID, {K, V}, Acc) ->
    {FSVariable, FSValue} = get_fs_key_and_value(K, V, UUID),
    %% NOTE: uuid_setXXX does not support vars:
    %%   switch_channel.c:1287 Invalid data (XXX contains a variable)
    %%   so issue a set command if it is present.
    case binary:match(FSVariable, <<"${">>) =:= 'nomatch'
        andalso binary:match(FSValue, <<"${">>) =:= 'nomatch'
    of
        'true' -> [{FSVariable, FSValue} | Acc];
        'false' ->
            AppArg = wh_util:to_list(<<FSVariable/binary, "=", FSValue/binary>>),
            _ = freeswitch:sendmsg(Node, UUID, [{"call-command", "execute"}
                                                ,{"execute-app-name", "set"}
                                                ,{"execute-app-arg", AppArg}
                                               ]),
            Acc
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec export(atom(), ne_binary(), wh_proplist()) ->
                    ecallmgr_util:send_cmd_ret().
export(_, _, []) -> 'ok';
export(Node, UUID, [{<<"Auto-Answer", _/binary>> = K, V} | Props]) ->
    Exports = [get_fs_key_and_value(Key, Val, UUID) || {Key, Val} <- Props],
    ecallmgr_fs_command:export(Node, UUID, [{<<"alert_info">>, <<"intercom">>}
                                            ,get_fs_key_and_value(K, V, UUID)
                                            | Exports
                                           ]);
export(Node, UUID, Props) ->
    Exports = [get_fs_key_and_value(Key, Val, UUID) || {Key, Val} <- Props],
    ecallmgr_fs_command:export(Node, UUID, Exports).

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
-type bridge_channel() :: ne_binary().
-type bridge_channels() :: [] | [bridge_channel(),...].
-type build_return() :: bridge_channel() | {'worker', pid()}.
-type build_returns() :: [build_return(),...] | [].
-type bridge_endpoints() :: [bridge_endpoint(),...] | [].

-spec build_bridge_string(wh_json:objects()) -> ne_binary().
-spec build_bridge_string(wh_json:objects(), ne_binary()) -> ne_binary().
build_bridge_string(Endpoints) ->
    build_bridge_string(Endpoints, <<"|">>).

build_bridge_string(Endpoints, Seperator) ->
    %% De-dup the bridge strings by matching those with the same
    %%  Invite-Format, To-IP, To-User, To-realm, To-DID, and Route
    BridgeStrings = build_bridge_channels(Endpoints),
    %% NOTE: dont use binary_join here as it will crash on an empty list...
    wh_util:join_binary(lists:reverse(BridgeStrings), Seperator).

-spec endpoint_jobjs_to_records(wh_json:objects()) -> bridge_endpoints().
-spec endpoint_jobjs_to_records(wh_json:objects(), boolean()) -> bridge_endpoints().

endpoint_jobjs_to_records(Endpoints) ->
    endpoint_jobjs_to_records(Endpoints, 'true').

endpoint_jobjs_to_records(Endpoints, IncludeVars) ->
    [BridgeEndpoints
     || {_, BridgeEndpoints} <-
            endpoint_jobjs_to_records(Endpoints, IncludeVars, [])
    ].

endpoint_jobjs_to_records([], _, BridgeEndpoints) ->
    lists:reverse(BridgeEndpoints);
endpoint_jobjs_to_records([Endpoint|Endpoints], IncludeVars, BridgeEndpoints) ->
    Key = endpoint_key(Endpoint),
    case wapi_dialplan:bridge_endpoint_v(Endpoint)
        andalso (not lists:keymember(Key, 1, BridgeEndpoints)) of
        'false' ->
            lager:debug("skipping invalid or duplicate endpoint: ~-300p~n", [Key]),
            endpoint_jobjs_to_records(Endpoints, IncludeVars, BridgeEndpoints);
        'true' ->
            lager:debug("building bridge endpoint: ~-300p~n", [Key]),
            BridgeEndpoint = endpoint_jobj_to_record(Endpoint, IncludeVars),
            endpoint_jobjs_to_records(Endpoints, IncludeVars
                                      ,[{Key, BridgeEndpoint}|BridgeEndpoints])
    end.

endpoint_key(Endpoint) ->
    [wh_json:get_value(<<"Invite-Format">>, Endpoint)
     ,wh_json:get_value(<<"To-User">>, Endpoint)
     ,wh_json:get_value(<<"To-Realm">>, Endpoint)
     ,wh_json:get_value(<<"To-DID">>, Endpoint)
     ,wh_json:get_value(<<"Route">>, Endpoint)
    ].

-spec endpoint_jobj_to_record(wh_json:object()) -> bridge_endpoint().
endpoint_jobj_to_record(Endpoint) ->
    endpoint_jobj_to_record(Endpoint, 'true').

-spec endpoint_jobj_to_record(wh_json:object(), boolean()) -> bridge_endpoint().
endpoint_jobj_to_record(Endpoint, IncludeVars) ->
    ToUser = wh_json:get_ne_value(<<"To-User">>, Endpoint),
    #bridge_endpoint{invite_format = wh_json:get_ne_value(<<"Invite-Format">>, Endpoint, <<"username">>)
                     ,endpoint_type = wh_json:get_ne_value(<<"Endpoint-Type">>, Endpoint, <<"sip">>)
                     ,ip_address = wh_json:get_ne_value(<<"To-IP">>, Endpoint)
                     ,username = wh_json:get_ne_value(<<"To-Username">>, Endpoint, ToUser)
                     ,user = ToUser
                     ,realm = wh_json:get_ne_value(<<"To-Realm">>, Endpoint)
                     ,number = wh_json:get_ne_value(<<"To-DID">>, Endpoint)
                     ,route = wh_json:get_ne_value(<<"Route">>, Endpoint)
                     ,proxy_address = wh_json:get_ne_value(<<"Proxy-IP">>, Endpoint)
                     ,forward_address = wh_json:get_ne_value(<<"Forward-IP">>, Endpoint)
                     ,transport = wh_json:get_ne_value(<<"SIP-Transport">>, Endpoint)
                     ,span = get_endpoint_span(Endpoint)
                     ,channel_selection = get_endpoint_channel_selection(Endpoint)
                     ,interface = get_endpoint_interface(Endpoint)
                     ,sip_interface = wh_json:get_ne_value(<<"SIP-Interface">>, Endpoint)
                     ,channel_vars = get_leg_vars(Endpoint, ToUser)
                     ,include_channel_vars = IncludeVars
                    }.

-spec get_leg_vars(wh_json:object(), api_binary()) -> iolist().
get_leg_vars(Endpoint, ToUser) ->
    User = wh_json:get_ne_value(<<"To-Username">>, Endpoint, ToUser),
    Realm = wh_json:get_ne_value(<<"To-Realm">>, Endpoint),
    case wh_util:is_empty(User) orelse wh_util:is_empty(Realm) of
        'true' -> ecallmgr_fs_xml:get_leg_vars(Endpoint);
        'false' ->
            ToURI = <<"sip:", User/binary, "@", Realm/binary>>,
            E = wh_json:set_value(<<"To-URI">>, ToURI, Endpoint),
            ecallmgr_fs_xml:get_leg_vars(E)
    end.

-spec get_endpoint_span(wh_json:object()) -> ne_binary().
get_endpoint_span(Endpoint) ->
    wh_json:get_binary_value([<<"Endpoint-Options">>, <<"Span">>], Endpoint, <<"1">>).

-spec get_endpoint_channel_selection(wh_json:object()) -> ne_binary().
get_endpoint_channel_selection(Endpoint) ->
    case wh_json:get_binary_value([<<"Endpoint-Options">>, <<"Span">>], Endpoint) of
        <<"descending">> -> <<"A">>;
        _Else -> <<"a">>
    end.

-spec get_endpoint_interface(wh_json:object()) -> ne_binary().
get_endpoint_interface(Endpoint) ->
    case wh_json:is_true([<<"Endpoint-Options">>, <<"Skype-RR">>], Endpoint, 'false') of
        'false' -> wh_json:get_value([<<"Endpoint-Options">>, <<"Skype-Interface">>], Endpoint);
        'true' -> <<"RR">>
    end.

-spec build_simple_channels(bridge_endpoints()) -> bridge_channels().
build_simple_channels(Endpoints) ->
    EPs = endpoint_jobjs_to_records(Endpoints, 'false'),
    build_bridge_channels(EPs, []).

-spec build_bridge_channels(bridge_endpoints()) -> bridge_channels().
build_bridge_channels(Endpoints) ->
    EPs = endpoint_jobjs_to_records(Endpoints),
    build_bridge_channels(EPs, []).

-spec build_bridge_channels(bridge_endpoints(), build_returns()) -> bridge_channels().
%% If the Invite-Format is "route" then we have been handed a sip route, do that now
build_bridge_channels([#bridge_endpoint{invite_format = <<"route">>}=Endpoint|Endpoints], Channels) ->
    case build_channel(Endpoint) of
        {'error', _} -> build_bridge_channels(Endpoints, Channels);
        {'ok', Channel} -> build_bridge_channels(Endpoints, [Channel|Channels])
    end;
%% If this does not have an explicted sip route and we have no ip address, lookup the registration
build_bridge_channels([#bridge_endpoint{ip_address='undefined'}=Endpoint|Endpoints], Channels) ->
    S = self(),
    Pid = spawn(fun() -> S ! {self(), build_channel(Endpoint)} end),
    build_bridge_channels(Endpoints, [{'worker', Pid}|Channels]);
%% If we have been given a IP to route to then do that now
build_bridge_channels([Endpoint|Endpoints], Channels) ->
    case build_channel(Endpoint) of
        {'error', _} -> build_bridge_channels(Endpoints, Channels);
        {'ok', Channel} -> build_bridge_channels(Endpoints, [Channel|Channels])
    end;
%% wait for any registration lookups to complete
build_bridge_channels([], IntermediateResults) ->
    lists:foldr(fun({'worker', Pid}, Channels) ->
                        maybe_collect_worker_channel(Pid, Channels);
                 (Channel, Channels) ->
                        [Channel|Channels]
                end, [], IntermediateResults).

-spec maybe_collect_worker_channel(pid(), bridge_channels()) -> bridge_channels().
maybe_collect_worker_channel(Pid, Channels) ->
    receive
        {Pid, {'error', _}} -> Channels;
        {Pid, {'ok', Channel}} -> [Channel|Channels]
    after
        2000 -> Channels
    end.

build_channel(#bridge_endpoint{endpoint_type = <<"freetdm">>}=Endpoint) ->
    build_freetdm_channel(Endpoint);
build_channel(#bridge_endpoint{endpoint_type = <<"skype">>}=Endpoint) ->
    build_skype_channel(Endpoint);
build_channel(#bridge_endpoint{endpoint_type = <<"sip">>}=Endpoint) ->
    build_sip_channel(Endpoint);
build_channel(EndpointJObj) -> build_channel(endpoint_jobj_to_record(EndpointJObj)).

-spec build_freetdm_channel(bridge_endpoint()) ->
                                   {'ok', bridge_channel()} |
                                   {'error', 'number_not_provided'}.
build_freetdm_channel(#bridge_endpoint{number='undefined'}) ->
    {'error', 'number_not_provided'};
build_freetdm_channel(#bridge_endpoint{invite_format = <<"e164">>
                                       ,number=Number
                                       ,span=Span
                                       ,channel_selection=ChannelSelection
                                      }) ->
    {'ok', <<"freetdm/", Span/binary, "/", ChannelSelection/binary, "/", (wnm_util:to_e164(Number))/binary>>};
build_freetdm_channel(#bridge_endpoint{invite_format = <<"npan">>
                                       ,number=Number
                                       ,span=Span
                                       ,channel_selection=ChannelSelection
                                      }) ->
    {'ok', <<"freetdm/", Span/binary, "/", ChannelSelection/binary, "/", (wnm_util:to_npan(Number))/binary>>};
build_freetdm_channel(#bridge_endpoint{invite_format = <<"1npan">>
                                       ,number=Number
                                       ,span=Span
                                       ,channel_selection=ChannelSelection
                                      }) ->
    {'ok', <<"freetdm/", Span/binary, "/", ChannelSelection/binary, "/", (wnm_util:to_1npan(Number))/binary>>};
build_freetdm_channel(#bridge_endpoint{number=Number
                                       ,span=Span
                                       ,channel_selection=ChannelSelection
                                      }) ->
    {'ok', <<"freetdm/", Span/binary, "/", ChannelSelection/binary, "/", Number/binary>>}.

-spec build_skype_channel(bridge_endpoint()) ->
                                 {'ok', bridge_channel()} |
                                 {'error', 'number_not_provided'}.
build_skype_channel(#bridge_endpoint{user='undefined'}) ->
    {'error', 'number_not_provided'};
build_skype_channel(#bridge_endpoint{user=User, interface=IFace}) ->
    {'ok', <<"skypopen/", IFace/binary, "/", User/binary>>}.

-spec build_sip_channel(bridge_endpoint()) ->
                               {'ok', bridge_channel()} |
                               {'error', _}.
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
        Channel -> {'ok', Channel}
    catch
        _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:warning("Failed to build sip channel (~s): ~p", [_E, _R]),
            wh_util:log_stacktrace(ST),
            {'error', 'invalid'}
    end.

-spec get_sip_contact(bridge_endpoint()) -> ne_binary().
get_sip_contact(#bridge_endpoint{invite_format = <<"route">>, route=Route}) -> Route;
get_sip_contact(#bridge_endpoint{ip_address='undefined'
                                 ,realm=Realm
                                 ,username=Username
                                }) ->
    {'ok', Contact} = ecallmgr_registrar:lookup_contact(Realm, Username),
    binary:replace(Contact, <<">">>, <<>>);
get_sip_contact(#bridge_endpoint{ip_address=IPAddress}) -> IPAddress.

-spec maybe_clean_contact(ne_binary(), bridge_endpoint()) -> ne_binary().
maybe_clean_contact(<<"sip:", Contact/binary>>, Endpoint) ->
    maybe_clean_contact(Contact, Endpoint);
maybe_clean_contact(Contact, #bridge_endpoint{invite_format = <<"route">>}) ->
    Contact;
maybe_clean_contact(Contact, _) ->
    re:replace(Contact, <<"^.*?[^=]sip:">>, <<>>, [{'return', 'binary'}]).

-spec ensure_username_present(ne_binary(), bridge_endpoint()) -> ne_binary().
ensure_username_present(Contact, #bridge_endpoint{invite_format = <<"route">>}) ->
    Contact;
ensure_username_present(Contact, Endpoint) ->
    case binary:split(Contact, <<"@">>) of
        [_, _] -> Contact;
        _ -> <<(guess_username(Endpoint))/binary, "@", Contact/binary>>
    end.

-spec guess_username(bridge_endpoint()) -> ne_binary().
guess_username(#bridge_endpoint{number=Number}) when is_binary(Number) -> Number;
guess_username(#bridge_endpoint{username=Username}) when is_binary(Username) -> Username;
guess_username(#bridge_endpoint{user=User}) when is_binary(User) -> User;
guess_username(_) -> <<"kazoo">>.

-spec maybe_replace_fs_path(ne_binary(), bridge_endpoint()) -> ne_binary().
maybe_replace_fs_path(Contact, #bridge_endpoint{proxy_address='undefined'}) -> Contact;
maybe_replace_fs_path(Contact, #bridge_endpoint{proxy_address = <<"sip:", _/binary>> = Proxy}) ->
    case re:replace(Contact, <<";fs_path=[^;?]*">>, <<";fs_path=", Proxy/binary>>, [{'return', 'binary'}]) of
        Contact ->
            %% NOTE: this will be invalid if the channel has headers, see rfc3261 19.1.1
            <<Contact/binary, ";fs_path=", Proxy/binary>>;
        Updated -> Updated
    end;
maybe_replace_fs_path(Contact, #bridge_endpoint{proxy_address=Proxy}=Endpoint) ->
    maybe_replace_fs_path(Contact, Endpoint#bridge_endpoint{proxy_address = <<"sip:", Proxy/binary>>}).

-spec maybe_replace_transport(ne_binary(), bridge_endpoint()) -> ne_binary().
maybe_replace_transport(Contact, #bridge_endpoint{transport='undefined'}) -> Contact;
maybe_replace_transport(Contact, #bridge_endpoint{transport=Transport}) ->
    case re:replace(Contact, <<";transport=[^;?]*">>, <<";transport=", Transport/binary>>, [{'return', 'binary'}]) of
        Contact ->
            %% NOTE: this will be invalid if the channel has headers, see rfc3261 19.1.1
            <<Contact/binary, ";transport=", Transport/binary>>;
        Updated -> Updated
    end.

-spec maybe_format_user(ne_binary(), bridge_endpoint()) -> ne_binary().
maybe_format_user(Contact, #bridge_endpoint{invite_format = <<"username">>
                                            ,user=User
                                           }) when User =/= 'undefined' ->
    re:replace(Contact, "^[^\@]+", User, [{'return', 'binary'}]);
maybe_format_user(Contact, #bridge_endpoint{invite_format = <<"username">>
                                            ,username=Username
                                           }) when Username =/= 'undefined' ->
    re:replace(Contact, "^[^\@]+", Username, [{'return', 'binary'}]);

maybe_format_user(Contact, #bridge_endpoint{number='undefined'}) -> Contact;
maybe_format_user(Contact, #bridge_endpoint{invite_format = <<"e164">>, number=Number}) ->
    re:replace(Contact, "^[^\@]+", wnm_util:to_e164(Number), [{'return', 'binary'}]);
maybe_format_user(Contact, #bridge_endpoint{invite_format = <<"npan">>, number=Number}) ->
    re:replace(Contact, "^[^\@]+", wnm_util:to_npan(Number), [{'return', 'binary'}]);
maybe_format_user(Contact, #bridge_endpoint{invite_format = <<"1npan">>, number=Number}) ->
    re:replace(Contact, "^[^\@]+", wnm_util:to_1npan(Number), [{'return', 'binary'}]);
maybe_format_user(Contact, _) -> Contact.

-spec maybe_set_interface(ne_binary(), bridge_endpoint()) -> ne_binary().
maybe_set_interface(<<"sofia/", _/binary>>=Contact, _) -> Contact;
maybe_set_interface(<<"loopback/", _/binary>>=Contact, _) -> Contact;
maybe_set_interface(Contact, #bridge_endpoint{sip_interface='undefined'}) ->
    <<?SIP_INTERFACE, Contact/binary>>;
maybe_set_interface(Contact, #bridge_endpoint{sip_interface=SIPInterface}) ->
    <<SIPInterface/binary, Contact/binary>>.

-spec append_channel_vars(ne_binary(), bridge_endpoint()) -> ne_binary().
append_channel_vars(Contact, #bridge_endpoint{include_channel_vars='false'}) ->
    'false' = wh_util:is_empty(Contact),
    Contact;
append_channel_vars(Contact, #bridge_endpoint{channel_vars=["[",[],"]"]}) ->
    'false' = wh_util:is_empty(Contact),
    Contact;
append_channel_vars(Contact, #bridge_endpoint{channel_vars=ChannelVars}) ->
    'false' = wh_util:is_empty(Contact),
    list_to_binary([ChannelVars, Contact]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_masquerade_event(ne_binary(), ne_binary()) -> ne_binary().
create_masquerade_event(Application, EventName) ->
    create_masquerade_event(Application, EventName, 'true').

create_masquerade_event(Application, EventName, Boolean) ->
    Prefix = case Boolean of
                 'true' -> <<"event ">>;
                 'false' -> <<>>
             end,
    <<Prefix/binary, "Event-Name=CUSTOM,Event-Subclass=whistle::masquerade"
      ,",whistle_event_name=", EventName/binary
      ,",whistle_application_name=", Application/binary>>.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec media_path(ne_binary(), ne_binary(), wh_json:object()) -> ne_binary().
media_path(MediaName, UUID, JObj) -> media_path(MediaName, 'new', UUID, JObj).

-spec media_path(ne_binary(), media_types(), ne_binary(), wh_json:object()) -> ne_binary().
media_path('undefined', _Type, _UUID, _) -> <<"silence_stream://5">>;
media_path(MediaName, Type, UUID, JObj) when not is_binary(MediaName) ->
    media_path(wh_util:to_binary(MediaName), Type, UUID, JObj);
media_path(<<"silence">> = Media, _Type, _UUID, _) -> Media;
media_path(<<"silence_stream://", _/binary>> = Media, _Type, _UUID, _) -> Media;
media_path(<<"tone_stream://", _/binary>> = Media, _Type, _UUID, _) -> Media;
media_path(<<"local_stream://", FSPath/binary>>, _Type, _UUID, _) -> recording_filename(FSPath);
media_path(<<?LOCAL_MEDIA_PATH, _/binary>> = FSPath, _Type, _UUID, _) -> FSPath;
media_path(<<"http://", _/binary>> = URI, _Type, _UUID, _) -> get_fs_playback(URI);
media_path(<<"https://", _/binary>> = URI, _Type, _UUID, _) -> get_fs_playback(URI);
media_path(MediaName, Type, UUID, JObj) ->
    case lookup_media(MediaName, UUID, JObj, Type) of
        {'error', _E} ->
            lager:warning("failed to get media path for ~s: ~p", [MediaName, _E]),
            wh_util:to_binary(MediaName);
        {'ok', Path} -> wh_util:to_binary(get_fs_playback(Path))
    end.

-spec fax_filename(ne_binary()) -> file:filename().
fax_filename(UUID) ->
    Ext = ecallmgr_config:get(<<"default_fax_extension">>, <<".tiff">>),
    filename:join([ecallmgr_config:get(<<"fax_file_path">>, <<"/tmp/">>)
                   ,<<(amqp_util:encode(UUID))/binary, Ext/binary>>
                  ]).

-spec recording_filename(ne_binary()) -> file:filename().
recording_filename(<<"local_stream://", MediaName/binary>>) -> recording_filename(MediaName);
recording_filename(MediaName) ->
    Ext = recording_extension(MediaName),
    RootName = filename:basename(MediaName, Ext),
    Directory = recording_directory(MediaName),
    RecordingName = filename:join([Directory
                                   ,<<(amqp_util:encode(RootName))/binary
                                      ,Ext/binary>>
                                  ]),
    _ = wh_cache:store_local(?ECALLMGR_UTIL_CACHE
                             ,?ECALLMGR_PLAYBACK_MEDIA_KEY(MediaName)
                             ,RecordingName),
    RecordingName.

recording_directory(<<"/", _/binary>> = FullPath) -> filename:dirname(FullPath);
recording_directory(_RelativePath) -> ecallmgr_config:get(<<"recording_file_path">>, <<"/tmp/">>).

recording_extension(MediaName) ->
    case filename:extension(MediaName) of
        Empty when Empty =:= <<>> orelse Empty =:= [] ->
            ecallmgr_config:get(<<"default_recording_extension">>, <<".mp3">>);
        <<".mp3">> = MP3 -> MP3;
        <<".wav">> = WAV -> WAV;
        _ -> ecallmgr_config:get(<<"default_recording_extension">>, <<".mp3">>)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_fs_playback(ne_binary()) -> ne_binary().
get_fs_playback(<<?LOCAL_MEDIA_PATH, _/binary>> = URI) -> URI;
get_fs_playback(URI) -> maybe_playback_via_vlc(URI).

maybe_playback_via_vlc(URI) ->
    case wh_util:is_true(ecallmgr_config:get(<<"use_vlc">>, 'false')) of
        'false' -> maybe_playback_via_shout(URI);
        'true' ->
            lager:debug("media is streamed via VLC, prepending ~s", [URI]),
            <<"vlc://", URI/binary>>
    end.

maybe_playback_via_shout(URI) ->
    case filename:extension(URI) =:= <<".mp3">>
        andalso wh_util:is_true(ecallmgr_config:get(<<"use_shout">>, 'false'))
    of
        'false' -> maybe_playback_via_http_cache(URI);
        'true' ->
            lager:debug("media is streamed via shout, updating ~s", [URI]),
            binary:replace(URI, [<<"http">>, <<"https">>], <<"shout">>)
    end.

maybe_playback_via_http_cache(URI) ->
    case wh_util:is_true(ecallmgr_config:get(<<"use_http_cache">>, 'true')) of
        'false' -> URI;
        'true' ->
            lager:debug("media is streamed via http_cache, using ~s", [URI]),
            <<"${http_get(", URI/binary, ")}">>
    end.

%% given a proplist of a FS event, return the Whistle-equivalent app name(s).
%% a FS event could have multiple Whistle equivalents
-spec convert_fs_evt_name(ne_binary()) -> ne_binaries().
convert_fs_evt_name(EvtName) ->
    [ WhAppEvt || {FSEvt, WhAppEvt} <- ?FS_APPLICATION_NAMES, FSEvt =:= EvtName].

%% given a Whistle Dialplan Application name, return the FS-equivalent event name
%% A Whistle Dialplan Application name is 1-to-1 with the FS-equivalent
-spec convert_whistle_app_name(ne_binary()) -> ne_binaries().
convert_whistle_app_name(App) ->
    [EvtName || {EvtName, AppName} <- ?FS_APPLICATION_NAMES, App =:= AppName].

-type media_types() :: 'new' | 'extant'.
-spec lookup_media(ne_binary(), ne_binary(), wh_json:object(), media_types()) ->
                          {'ok', ne_binary()} |
                          {'error', _}.
lookup_media(MediaName, CallId, JObj, Type) ->
    case wh_cache:fetch_local(?ECALLMGR_UTIL_CACHE
                              ,?ECALLMGR_PLAYBACK_MEDIA_KEY(MediaName))
    of
        {'ok', _Path}=Ok ->
            lager:debug("media ~s exists in playback cache as ~s", [MediaName, _Path]),
            Ok;
        {'error', 'not_found'} ->
            request_media_url(MediaName, CallId, JObj, Type)
    end.

-spec request_media_url(ne_binary(), ne_binary(), wh_json:object(), media_types()) -> {'ok', ne_binary()} | {'error', _}.
request_media_url(MediaName, CallId, JObj, Type) ->
    Request = wh_json:set_values(
                props:filter_undefined(
                  [{<<"Media-Name">>, MediaName}
                   ,{<<"Stream-Type">>, wh_util:to_binary(Type)}
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
        {'error', _}=E -> E;
        {'ok', MediaResp} ->
            {'ok', wh_json:get_value(<<"Stream-URL">>, MediaResp, <<>>)}
    end.
