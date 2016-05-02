%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%% Various utilities specific to ecallmgr. More general utilities go
%%% in kazoo_util.erl
%%% @end
%%%
%%% @contributors
%%%   James Aimonetti <james@2600hz.org>
%%%   Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_util).

-export([send_cmd/4]).
-export([get_fs_kv/2, get_fs_kv/3, get_fs_key_and_value/3]).
-export([get_fs_key/1]).
-export([get_expires/1]).
-export([get_interface_properties/1, get_interface_properties/2]).
-export([get_sip_to/1, get_sip_from/1, get_sip_request/1, get_orig_ip/1, get_orig_port/1]).
-export([custom_channel_vars/1, custom_channel_vars/2]).
-export([eventstr_to_proplist/1, varstr_to_proplist/1, get_setting/1, get_setting/2]).
-export([is_node_up/1, is_node_up/2]).
-export([build_bridge_string/1, build_bridge_string/2]).
-export([build_channel/1]).
-export([build_simple_channels/1]).
-export([create_masquerade_event/2, create_masquerade_event/3]).
-export([media_path/3, media_path/4
         ,lookup_media/4
        ]).
-export([unserialize_fs_array/1]).
-export([convert_fs_evt_name/1, convert_kazoo_app_name/1]).
-export([fax_filename/1
         ,recording_filename/1
        ]).
-export([maybe_sanitize_fs_value/2]).

-export([custom_sip_headers/1 , is_custom_sip_header/1, normalize_custom_sip_header_name/1]).
-export([maybe_add_expires_deviation/1, maybe_add_expires_deviation_ms/1]).

-export([get_dial_separator/2]).
-export([fix_contact/3]).

-include_lib("kazoo/src/api/kapi_dialplan.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include("ecallmgr.hrl").
-include_lib("kazoo_sip/include/kzsip_uri.hrl").

-define(HTTP_GET_PREFIX, "http_cache://").

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
                          ,failover
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
    send_cmd(Node, UUID, kz_util:to_list(App), Args);
send_cmd(Node, UUID, "xferext", Dialplan) ->
    XferExt = [begin
                   lager:debug("building xferext on node ~s: ~s", [Node, V]),
                   {kz_util:to_list(K), kz_util:to_list(V)}
               end || {K, V} <- Dialplan],
    'ok' = freeswitch:sendmsg(Node, UUID, [{"call-command", "xferext"} | XferExt]);
send_cmd(Node, UUID, App, Args) when not is_list(Args) ->
    send_cmd(Node, UUID, App, kz_util:to_list(Args));
send_cmd(Node, UUID, "playstop", _Args) ->
    lager:debug("execute on node ~s: uuid_break(~s all)", [Node, UUID]),
    freeswitch:api(Node, 'uuid_break', kz_util:to_list(<<UUID/binary, " all">>));
send_cmd(Node, UUID, "unbridge", _) ->
    lager:debug("execute on node ~s: uuid_park(~s)", [Node, UUID]),
    freeswitch:api(Node, 'uuid_park', kz_util:to_list(UUID));
send_cmd(Node, _UUID, "broadcast", Args) ->
    lager:debug("execute on node ~s: uuid_broadcast(~s)", [Node, Args]),
    Resp = freeswitch:api(Node, 'uuid_broadcast', kz_util:to_list(iolist_to_binary(Args))),
    lager:debug("broadcast resulted in: ~p", [Resp]),
    Resp;
send_cmd(Node, UUID, "call_pickup", Target) ->
    Args = iolist_to_binary([UUID, " ", Target]),
    lager:debug("execute on node ~s: uuid_bridge(~s)", [Node, Args]),
    freeswitch:api(Node, 'uuid_bridge', kz_util:to_list(Args));
send_cmd(Node, UUID, "hangup", _) ->
    lager:debug("terminate call on node ~s", [Node]),
    freeswitch:api(Node, 'uuid_kill', kz_util:to_list(UUID));
send_cmd(Node, _UUID, "audio_level", Args) ->
    lager:debug("execute on node ~s: uuid_audio ~p", [Node, Args]),
    freeswitch:api(Node, 'uuid_audio', kz_util:to_list(iolist_to_binary(Args)));
send_cmd(Node, UUID, "conference", Args) ->
    Args1 = iolist_to_binary([UUID, " conference:", Args, " inline"]),
    lager:debug("starting conference on ~s: ~s", [Node, Args1]),
    freeswitch:api(Node, 'uuid_transfer', kz_util:to_list(Args1));
send_cmd(Node, _UUID, "transfer", Args) ->
    lager:debug("transfering on ~s: ~s", [Node, Args]),
    freeswitch:api(Node, 'uuid_transfer', kz_util:to_list(Args));
send_cmd(Node, UUID, AppName, Args) ->
    Result = freeswitch:sendmsg(Node, UUID, [{"call-command", "execute"}
                                             ,{"execute-app-name", AppName}
                                             ,{"execute-app-arg", kz_util:to_list(Args)}
                                            ]),
    lager:debug("execute on node ~s(~s) ~s(~s): ~p"
                ,[Node, UUID, AppName, Args, Result]
               ),
    Result.

-spec get_expires(kz_proplist()) -> integer().
get_expires(Props) ->
    Expiry = kz_util:to_integer(props:get_first_defined([<<"Expires">>, <<"expires">>], Props, 300)),
    round(Expiry * 1.25).

-spec get_interface_properties(atom()) -> kz_proplist().
-spec get_interface_properties(atom(), text()) -> kz_proplist().

get_interface_properties(Node) ->
    get_interface_properties(Node, ?DEFAULT_FS_PROFILE).

get_interface_properties(Node, Interface) ->
    case freeswitch:api(Node, 'sofia', kz_util:to_list(list_to_binary(["status profile ", Interface]))) of
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
-spec get_sip_to(kz_proplist()) -> ne_binary().
get_sip_to(Props) ->
    get_sip_to(Props, kzd_freeswitch:call_direction(Props)).

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
-spec get_sip_from(kz_proplist()) -> ne_binary().
-spec get_sip_from(kz_proplist(), api_binary()) -> ne_binary().
get_sip_from(Props) ->
    get_sip_from(Props, kzd_freeswitch:call_direction(Props)).

get_sip_from(Props, <<"outbound">>) ->
    case props:get_value(<<"Other-Leg-Channel-Name">>, Props) of
        'undefined' ->
            Number = props:get_value(<<"Other-Leg-Caller-ID-Number">>, Props, <<"nouser">>),
            Realm = props:get_first_defined([?GET_CCV(<<"Realm">>)
                                             ,<<"variable_sip_auth_realm">>
                                            ], Props, ?DEFAULT_REALM),
            props:get_value(<<"variable_sip_from_uri">>
                            ,Props
                            ,<<Number/binary, "@", Realm/binary>>
                           );
        OtherChannel ->
            lists:last(binary:split(OtherChannel, <<"/">>, ['global']))
    end;
get_sip_from(Props, _) ->
    Default = <<(props:get_value(<<"variable_sip_from_user">>, Props, <<"nouser">>))/binary
                ,"@"
                ,(props:get_first_defined([?GET_CCV(<<"Realm">>)
                                           ,<<"variable_sip_from_host">>
                                           ,<<"sip_from_host">>
                                          ], Props, ?DEFAULT_REALM))/binary
              >>,
    props:get_first_defined([<<"Channel-Presence-ID">>
                             ,<<"variable_sip_from_uri">>
                            ], Props, Default).

%% retrieves the sip address for the 'request' field
-spec get_sip_request(kz_proplist()) -> ne_binary().
get_sip_request(Props) ->
    [User | _] = binary:split(
                   props:get_first_defined(
                     [<<"Hunt-Destination-Number">>
                      ,<<"Caller-Destination-Number">>
                      ,<<"variable_sip_to_user">>
                      ,<<"variable_sip_req_uri">>
                      ,<<"variable_sip_loopback_req_uri">>
                      ,<<"sip_req_uri">>
                      ,<<"sip_to_user">>
                     ], Props, <<"nouser">>), <<"@">>, ['global']),
    Realm = lists:last(binary:split(
                    props:get_first_defined([?GET_CCV(<<"Realm">>)
                                     ,<<"variable_sip_auth_realm">>
                                     ,<<"variable_sip_to_host">>
                                     ,<<"sip_auth_realm">>
                                     ,<<"sip_to_host">>
                                     ,<<"variable_sip_req_host">>
                                     ,<<"sip_req_host">>
                                     ,<<"variable_sip_req_uri">>
                                     ,<<"sip_req_uri">>
                                     ,<<"variable_sip_loopback_req_uri">>
                                     ,<<"sip_loopback_req_uri">>
                                    ], Props, ?DEFAULT_REALM), <<"@">>, ['global'])),
    <<User/binary, "@", Realm/binary>>.

-spec get_orig_ip(kz_proplist()) -> api_binary().
get_orig_ip(Prop) ->
    props:get_first_defined([<<"X-AUTH-IP">>, <<"ip">>], Prop).

-spec get_orig_port(kz_proplist()) -> api_binary().
get_orig_port(Prop) ->
    case props:get_first_defined([<<"X-AUTH-PORT">>, <<"port">>], Prop) of
        <<>> -> 'undefined';
        <<"0">> -> 'undefined';
        Port -> Port
    end.

-spec get_sip_interface_from_db(ne_binaries()) -> ne_binary().
get_sip_interface_from_db([FsPath]) ->
    NetworkMap = ecallmgr_config:get(<<"network_map">>, kz_json:new()),
    case map_fs_path_to_sip_profile(FsPath, NetworkMap) of
        'undefined' ->
            lager:debug("unable to find network map for ~s, using default interface '~s'", [FsPath, ?SIP_INTERFACE]),
            ?SIP_INTERFACE;
        Else ->
            lager:debug("found custom interface '~s' in network map for ~s", [Else, FsPath]),
            Else
    end.

-spec map_fs_path_to_sip_profile(ne_binary(), kz_json:object()) -> api_binary().
map_fs_path_to_sip_profile(FsPath, NetworkMap) ->
    SIPInterfaceObj = kz_json:filter(fun({K, _}) ->
                               kz_network_utils:verify_cidr(FsPath, K)
                       end, NetworkMap),
    case kz_json:get_values(SIPInterfaceObj) of
        {[],[]} -> 'undefined';
        {[V|_], _} ->
            kz_json:get_ne_value(<<"custom_sip_interface">>, V)
    end.

%% Extract custom channel variables to include in the event
-spec custom_channel_vars(kz_proplist()) -> kz_proplist().
-spec custom_channel_vars(kz_proplist(), kz_proplist()) -> kz_proplist().
-spec custom_channel_vars_fold({ne_binary(), ne_binary()}, kz_proplist()) -> kz_proplist().
custom_channel_vars(Props) ->
    custom_channel_vars(Props, []).

custom_channel_vars(Props, Initial) ->
    maybe_update_referred_ccv(
      Props
      ,lists:usort(fun({A, _}, {B, _}) -> A =< B end
                ,lists:foldl(fun custom_channel_vars_fold/2
                  ,Initial
                  ,Props
                  ))
     ).

custom_channel_vars_fold({<<"variable_", ?CHANNEL_VAR_PREFIX, Key/binary>>, V}, Acc) ->
    [{Key, V} | Acc];
custom_channel_vars_fold({<<?CHANNEL_VAR_PREFIX, Key/binary>>, V}, Acc) ->
    [{Key, V} | Acc];
custom_channel_vars_fold({<<"variable_sip_h_X-", ?CHANNEL_VAR_PREFIX, Key/binary>>, V}, Acc) ->
    case props:is_defined(Key, Acc) of
        'true' -> Acc;
        'false' -> [{Key, V} | Acc]
    end;
custom_channel_vars_fold(_, Acc) -> Acc.

-spec maybe_update_referred_ccv(kz_proplist(), kz_proplist()) -> kz_proplist().
maybe_update_referred_ccv(Props, CCVs) ->
    update_referred_by_ccv(
      props:get_value(<<"variable_sip_h_Referred-By">>, Props)
      ,update_referred_to_ccv(
         props:get_value(<<"variable_sip_refer_to">>, Props)
         ,CCVs
        )
     ).

-spec update_referred_by_ccv(api_binary(), kz_proplist()) -> kz_proplist().
update_referred_by_ccv('undefined', CCVs) -> props:delete(<<"Referred-By">>, CCVs);
update_referred_by_ccv(ReferredBy, CCVs) ->
    props:set_value(
      <<"Referred-By">>
      ,kz_http_util:urldecode(ReferredBy)
      ,CCVs
     ).

-spec update_referred_to_ccv(api_binary(), kz_proplist()) -> kz_proplist().
update_referred_to_ccv('undefined', CCVs) -> props:delete(<<"Referred-To">>, CCVs);
update_referred_to_ccv(ReferredTo, CCVs) ->
    props:set_value(
      <<"Referred-To">>
      ,kz_http_util:urldecode(ReferredTo)
      ,CCVs
     ).

%% convert a raw FS string of headers to a proplist
%% "Event-Name: NAME\nEvent-Timestamp: 1234\n" -> [{<<"Event-Name">>, <<"NAME">>}, {<<"Event-Timestamp">>, <<"1234">>}]
-spec eventstr_to_proplist(text()) -> kz_proplist().
eventstr_to_proplist(EvtStr) ->
    [to_kv(X, ": ") || X <- string:tokens(kz_util:to_list(EvtStr), "\n")].

-spec to_kv(nonempty_string(), nonempty_string()) -> {ne_binary(), ne_binary()}.
to_kv(X, Separator) ->
    [K, V] = string:tokens(X, Separator),
    [{V1, _}] = kz_http_util:parse_query_string(list_to_binary(V)),
    {kz_util:to_binary(K), kz_util:to_binary(fix_value(K, V1))}.

fix_value("Event-Date-Timestamp", TStamp) ->
    kz_util:microseconds_to_seconds(kz_util:to_integer(TStamp));
fix_value(_K, V) -> V.

-spec unserialize_fs_array(api_binary()) -> ne_binaries().
unserialize_fs_array('undefined') -> [];
unserialize_fs_array(<<"ARRAY::", Serialized/binary>>) ->
    binary:split(Serialized, <<"|:">>, ['global']);
unserialize_fs_array(Single) ->
    [Single].

%% convert a raw FS list of vars to a proplist
%% "Event-Name=NAME,Event-Timestamp=1234" -> [{<<"Event-Name">>, <<"NAME">>}, {<<"Event-Timestamp">>, <<"1234">>}]
-spec varstr_to_proplist(nonempty_string()) -> kz_proplist().
varstr_to_proplist(VarStr) ->
    [to_kv(X, "=") || X <- string:tokens(kz_util:to_list(VarStr), ",")].

-spec get_setting(kz_json:key()) -> {'ok', any()}.
-spec get_setting(kz_json:key(), Default) -> {'ok', Default | any()}.
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
-spec get_fs_kv(ne_binary(), ne_binary()) -> binary().
-spec get_fs_kv(ne_binary(), ne_binary(), api_binary()) -> binary().
get_fs_kv(Key, Value) ->
    get_fs_kv(Key, Value, 'undefined').

get_fs_kv(<<?CHANNEL_VAR_PREFIX, Key/binary>>, Val, _) ->
    list_to_binary([?CHANNEL_VAR_PREFIX, kz_util:to_list(Key), "=", kz_util:to_list(Val)]);
get_fs_kv(<<"Hold-Media">>, Media, UUID) ->
    list_to_binary(["hold_music="
                    ,kz_util:to_list(media_path(Media, 'extant', UUID, kz_json:new()))
                   ]);
get_fs_kv(Key, Val, _) ->
    case lists:keyfind(Key, 1, ?SPECIAL_CHANNEL_VARS) of
        'false' ->
            list_to_binary([?CHANNEL_VAR_PREFIX, kz_util:to_list(Key), "=", kz_util:to_list(Val)]);
        {_, Prefix} ->
            V = maybe_sanitize_fs_value(Key, Val),
            list_to_binary([Prefix, "=", kz_util:to_list(V), ""])
    end.

-spec get_fs_key(ne_binary()) -> binary().
get_fs_key(<<?CHANNEL_VAR_PREFIX, _/binary>>=Key) -> Key;
get_fs_key(Key) ->
    case lists:keyfind(Key, 1, ?SPECIAL_CHANNEL_VARS) of
        'false' -> <<?CHANNEL_VAR_PREFIX, Key/binary>>;
        {_, Prefix} -> Prefix
    end.

-spec get_fs_key_and_value(ne_binary()
                           ,ne_binary() | ne_binaries() | kz_json:object()
                           ,ne_binary()
                          ) ->
                                  {ne_binary(), binary()} |
                                  [{ne_binary(), binary()}] |
                                  'skip'.
get_fs_key_and_value(<<"Hold-Media">>, Media, UUID) ->
    {<<"hold_music">>, media_path(Media, 'extant', UUID, kz_json:new())};
get_fs_key_and_value(<<"Diversions">>, Diversions, _UUID) ->
    lager:debug("setting diversions ~p on the channel", [Diversions]),
    [{<<"sip_h_Diversion">>, D} || D <- Diversions];
get_fs_key_and_value(<<?CHANNEL_VAR_PREFIX, Key/binary>>=Prefix, Val, _UUID) ->
    {Prefix, maybe_sanitize_fs_value(Key, Val)};
get_fs_key_and_value(Key, Val, _UUID) when is_binary(Val) ->
    case lists:keyfind(Key, 1, ?SPECIAL_CHANNEL_VARS) of
        'false' ->
            {list_to_binary([?CHANNEL_VAR_PREFIX, Key]), Val};
        {_, Prefix} ->
            {Prefix, maybe_sanitize_fs_value(Key, Val)}
    end;
get_fs_key_and_value(_, _, _) -> 'skip'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_sanitize_fs_value(text(), text()) -> binary().
maybe_sanitize_fs_value(<<"Outbound-Caller-ID-Name">>, Val) ->
    re:replace(Val, <<"[^a-zA-Z0-9-\s]">>, <<>>, ['global', {'return', 'binary'}]);
maybe_sanitize_fs_value(<<"Outbound-Callee-ID-Name">>, Val) ->
    re:replace(Val, <<"[^a-zA-Z0-9-\s]">>, <<>>, ['global', {'return', 'binary'}]);
maybe_sanitize_fs_value(<<"Caller-ID-Name">>, Val) ->
    re:replace(Val, <<"[^a-zA-Z0-9-\s]">>, <<>>, ['global', {'return', 'binary'}]);
maybe_sanitize_fs_value(<<"Callee-ID-Name">>, Val) ->
    re:replace(Val, <<"[^a-zA-Z0-9-\s]">>, <<>>, ['global', {'return', 'binary'}]);
maybe_sanitize_fs_value(Key, Val) when not is_binary(Key) ->
    maybe_sanitize_fs_value(kz_util:to_binary(Key), Val);
maybe_sanitize_fs_value(Key, Val) when not is_binary(Val) ->
    maybe_sanitize_fs_value(Key, kz_util:to_binary(Val));
maybe_sanitize_fs_value(_, Val) -> Val.

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
-type bridge_channels() :: ne_binaries().
-type build_return() :: bridge_channel() | {'worker', pid()}.
-type build_returns() :: [build_return()].
-type bridge_endpoints() :: [bridge_endpoint()].

-spec build_bridge_string(kz_json:objects()) -> ne_binary().
-spec build_bridge_string(kz_json:objects(), ne_binary()) -> ne_binary().
build_bridge_string(Endpoints) ->
    build_bridge_string(Endpoints, ?SEPARATOR_SINGLE).

build_bridge_string(Endpoints, Seperator) ->
    %% De-dup the bridge strings by matching those with the same
    %%  Invite-Format, To-IP, To-User, To-realm, To-DID, and Route
    BridgeStrings = build_bridge_channels(Endpoints),
    %% NOTE: dont use binary_join here as it will crash on an empty list...
    kz_util:join_binary(lists:reverse(BridgeStrings), Seperator).

-spec endpoint_jobjs_to_records(kz_json:objects()) -> bridge_endpoints().
-spec endpoint_jobjs_to_records(kz_json:objects(), boolean()) -> bridge_endpoints().

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
    case kapi_dialplan:bridge_endpoint_v(Endpoint)
        andalso (not lists:keymember(Key, 1, BridgeEndpoints))
    of
        'false' ->
            lager:debug("skipping invalid or duplicate endpoint: ~-300p~n", [Key]),
            endpoint_jobjs_to_records(Endpoints, IncludeVars, BridgeEndpoints);
        'true' ->
            lager:debug("building bridge endpoint: ~-300p~n", [Key]),
            BridgeEndpoint = endpoint_jobj_to_record(Endpoint, IncludeVars),
            endpoint_jobjs_to_records(Endpoints, IncludeVars
                                      ,[{Key, BridgeEndpoint}|BridgeEndpoints]
                                     )
    end.

-spec endpoint_key(kz_json:object()) -> api_binaries().
endpoint_key(Endpoint) ->
    [kz_json:get_value(<<"Invite-Format">>, Endpoint)
     ,kz_json:get_value(<<"To-User">>, Endpoint)
     ,kz_json:get_value(<<"To-Realm">>, Endpoint)
     ,kz_json:get_value(<<"To-DID">>, Endpoint)
     ,kz_json:get_value(<<"Route">>, Endpoint)
     ,kz_json:get_value(<<"Proxy-Zone">>, Endpoint)
     ,kz_json:get_value(<<"Proxy-IP">>, Endpoint)
    ].

-spec endpoint_jobj_to_record(kz_json:object()) -> bridge_endpoint().
endpoint_jobj_to_record(Endpoint) ->
    endpoint_jobj_to_record(Endpoint, 'true').

-spec endpoint_jobj_to_record(kz_json:object(), boolean()) -> bridge_endpoint().
endpoint_jobj_to_record(Endpoint, IncludeVars) ->
    ToUser = kz_json:get_ne_value(<<"To-User">>, Endpoint),
    #bridge_endpoint{invite_format = kz_json:get_ne_value(<<"Invite-Format">>, Endpoint, <<"username">>)
                     ,endpoint_type = kz_json:get_ne_value(<<"Endpoint-Type">>, Endpoint, <<"sip">>)
                     ,ip_address = kz_json:get_ne_value(<<"To-IP">>, Endpoint)
                     ,username = kz_json:get_ne_value(<<"To-Username">>, Endpoint, ToUser)
                     ,user = ToUser
                     ,realm = kz_json:get_ne_value(<<"To-Realm">>, Endpoint)
                     ,number = kz_json:get_ne_value(<<"To-DID">>, Endpoint)
                     ,route = kz_json:get_ne_value(<<"Route">>, Endpoint)
                     ,proxy_address = kz_json:get_ne_value(<<"Proxy-IP">>, Endpoint)
                     ,forward_address = kz_json:get_ne_value(<<"Forward-IP">>, Endpoint)
                     ,transport = kz_json:get_ne_value(<<"SIP-Transport">>, Endpoint)
                     ,span = get_endpoint_span(Endpoint)
                     ,channel_selection = get_endpoint_channel_selection(Endpoint)
                     ,interface = get_endpoint_interface(Endpoint)
                     ,sip_interface = kz_json:get_ne_value(<<"SIP-Interface">>, Endpoint)
                     ,channel_vars = ecallmgr_fs_xml:get_leg_vars(Endpoint)
                     ,include_channel_vars = IncludeVars
                     ,failover = kz_json:get_value(<<"Failover">>, Endpoint)
                    }.

-spec get_endpoint_span(kz_json:object()) -> ne_binary().
get_endpoint_span(Endpoint) ->
    kz_json:get_binary_value([<<"Endpoint-Options">>, <<"Span">>], Endpoint, <<"1">>).

-spec get_endpoint_channel_selection(kz_json:object()) -> ne_binary().
get_endpoint_channel_selection(Endpoint) ->
    case kz_json:get_binary_value([<<"Endpoint-Options">>, <<"Span">>], Endpoint) of
        <<"descending">> -> <<"A">>;
        _Else -> <<"a">>
    end.

-spec get_endpoint_interface(kz_json:object()) -> ne_binary().
get_endpoint_interface(Endpoint) ->
    case kz_json:is_true([<<"Endpoint-Options">>, <<"Skype-RR">>], Endpoint, 'false') of
        'false' -> kz_json:get_value([<<"Endpoint-Options">>, <<"Skype-Interface">>], Endpoint);
        'true' -> <<"RR">>
    end.

-spec build_simple_channels(bridge_endpoints()) -> bridge_channels().
build_simple_channels(Endpoints) ->
    EPs = endpoint_jobjs_to_records(Endpoints, 'false'),
    build_bridge_channels(EPs, []).

-spec build_bridge_channels(kz_json:objects()) -> bridge_channels().
build_bridge_channels(Endpoints) ->
    CWEP = maybe_apply_call_waiting(Endpoints),
    EPs = endpoint_jobjs_to_records(CWEP),
    build_bridge_channels(EPs, []).

-spec maybe_apply_call_waiting(kz_json:objects()) -> kz_json:objects().
maybe_apply_call_waiting(Endpoints) ->
    lists:map(fun call_waiting_map/1, Endpoints).

-spec call_waiting_map(kz_json:object()) -> kz_json:object().
call_waiting_map(Endpoint) ->
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, Endpoint, kz_json:new()),
    case kz_json:is_true(<<"Call-Waiting-Disabled">>, CCVs) of
        'false' -> Endpoint;
        'true' ->
            OwnerId = kz_json:get_value(<<"Owner-ID">>, CCVs),
            maybe_add_respond_header(Endpoint, OwnerId)
    end.

-spec maybe_add_respond_header(kz_json:object(), ne_binary()) -> kz_json:object().
maybe_add_respond_header(Endpoint, OwnerId) ->
    case ecallmgr_fs_channels:has_channels_for_owner(OwnerId) of
        'true' ->
            lager:debug("Channel must be busy!"),
            kz_json:set_value([<<"Custom-SIP-Headers">>, <<"X-KAZOO-Respond-With">>], <<"486 User Busy">>, Endpoint);
        'false' ->
            Endpoint
    end.

-spec build_bridge_channels(bridge_endpoints(), build_returns()) -> bridge_channels().
%% If the Invite-Format is "route" then we have been handed a sip route, do that now
build_bridge_channels([#bridge_endpoint{invite_format = <<"route">>}=Endpoint|Endpoints], Channels) ->
    case build_channel(Endpoint) of
        {'error', _} -> build_bridge_channels(Endpoints, Channels);
        {'ok', Channel} -> build_bridge_channels(Endpoints, [Channel|Channels])
    end;
build_bridge_channels([#bridge_endpoint{invite_format = <<"loopback">>}=Endpoint|Endpoints], Channels) ->
    case build_channel(Endpoint) of
        {'error', _} -> build_bridge_channels(Endpoints, Channels);
        {'ok', Channel} -> build_bridge_channels(Endpoints, [Channel|Channels])
    end;
%% If this does not have an explicted sip route and we have no ip address, lookup the registration
build_bridge_channels([#bridge_endpoint{ip_address='undefined'}=Endpoint|Endpoints], Channels) ->
    S = self(),
    Pid = kz_util:spawn(fun() -> S ! {self(), build_channel(Endpoint)} end),
    build_bridge_channels(Endpoints, [{'worker', Pid}|Channels]);
%% If we have been given a IP to route to then do that now
build_bridge_channels([Endpoint|Endpoints], Channels) ->
    case build_channel(Endpoint) of
        {'error', _} -> build_bridge_channels(Endpoints, Channels);
        {'ok', Channel} -> build_bridge_channels(Endpoints, [Channel|Channels])
    end;
%% wait for any registration lookups to complete
build_bridge_channels([], IntermediateResults) ->
    lists:foldr(fun intermediate_results_fold/2, [], IntermediateResults).

-spec intermediate_results_fold(build_return(), bridge_channels()) -> bridge_channels().
intermediate_results_fold({'worker', Pid}, Channels) ->
    maybe_collect_worker_channel(Pid, Channels);
intermediate_results_fold(Channel, Channels) ->
    [Channel|Channels].

-spec maybe_collect_worker_channel(pid(), bridge_channels()) -> bridge_channels().
maybe_collect_worker_channel(Pid, Channels) ->
    receive
        {Pid, {'error', _}} -> Channels;
        {Pid, {'ok', Channel}} -> [Channel|Channels]
    after
        2 * ?MILLISECONDS_IN_SECOND -> Channels
    end.

-spec build_channel(bridge_endpoint() | kz_json:object()) ->
                           {'ok', bridge_channel()} |
                           {'error', any()}.
build_channel(#bridge_endpoint{endpoint_type = <<"freetdm">>}=Endpoint) ->
    build_freetdm_channel(Endpoint);
build_channel(#bridge_endpoint{endpoint_type = <<"skype">>}=Endpoint) ->
    build_skype_channel(Endpoint);
build_channel(#bridge_endpoint{endpoint_type = <<"sip">>}=Endpoint) ->
    build_sip_channel(Endpoint);
build_channel(EndpointJObj) ->
    build_channel(endpoint_jobj_to_record(EndpointJObj)).

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
    {'ok', <<"freetdm/", Span/binary, "/", ChannelSelection/binary, "/", (knm_converters:normalize(Number))/binary>>};
build_freetdm_channel(#bridge_endpoint{invite_format = <<"npan">>
                                       ,number=Number
                                       ,span=Span
                                       ,channel_selection=ChannelSelection
                                      }) ->
    {'ok', <<"freetdm/", Span/binary, "/", ChannelSelection/binary, "/", (knm_converters:to_npan(Number))/binary>>};
build_freetdm_channel(#bridge_endpoint{invite_format = <<"1npan">>
                                       ,number=Number
                                       ,span=Span
                                       ,channel_selection=ChannelSelection
                                      }) ->
    {'ok', <<"freetdm/", Span/binary, "/", ChannelSelection/binary, "/", (knm_converters:to_1npan(Number))/binary>>};
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
                               {'error', any()}.
build_sip_channel(#bridge_endpoint{failover=Failover}=Endpoint) ->
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
        _E:{'badmatch', {'error', 'not_found'}} ->
            lager:warning("Failed to build sip channel trying failover", []),
            maybe_failover(Failover);
        _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:warning("Failed to build sip channel (~s): ~p", [_E, _R]),
            kz_util:log_stacktrace(ST),
            {'error', 'invalid'}
    end.

-spec maybe_failover(kz_json:object()) ->
                            {'ok', bridge_channel()} |
                            {'error', any()}.
maybe_failover(Endpoint) ->
    case kz_util:is_empty(Endpoint) of
        'true' -> {'error', 'invalid'};
        'false' -> build_sip_channel(endpoint_jobj_to_record(Endpoint))
    end.

-spec get_sip_contact(bridge_endpoint()) -> ne_binary().
get_sip_contact(#bridge_endpoint{invite_format = <<"route">>, route=Route}) -> Route;
get_sip_contact(#bridge_endpoint{invite_format = <<"loopback">>, route=Route}) ->
    <<"loopback/", Route/binary, "/", (?DEFAULT_FREESWITCH_CONTEXT)/binary>>;
get_sip_contact(#bridge_endpoint{ip_address='undefined'
                                 ,realm=Realm
                                 ,username=Username
                                }) ->
    {'ok', Contact} = ecallmgr_registrar:lookup_contact(Realm, Username),
    binary:replace(Contact, <<">">>, <<>>);
get_sip_contact(#bridge_endpoint{ip_address=IPAddress}) -> IPAddress.

-spec maybe_clean_contact(ne_binary(), bridge_endpoint()) -> ne_binary().
maybe_clean_contact(<<"sip:", Contact/binary>>, _Endpoint) -> Contact;
maybe_clean_contact(<<"sips:", Contact/binary>>, _Endpoint) -> Contact;
maybe_clean_contact(Contact, #bridge_endpoint{invite_format = <<"route">>}) ->
    Contact;
maybe_clean_contact(Contact, #bridge_endpoint{invite_format = <<"loopback">>}) ->
    Contact;
maybe_clean_contact(Contact, _) ->
    re:replace(Contact, <<"^.*?[^=]sip:">>, <<>>, [{'return', 'binary'}]).

-spec ensure_username_present(ne_binary(), bridge_endpoint()) -> ne_binary().
ensure_username_present(Contact, #bridge_endpoint{invite_format = <<"route">>}) ->
    Contact;
ensure_username_present(Contact, #bridge_endpoint{invite_format = <<"loopback">>}) ->
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
    case re:replace(Contact
                    ,<<";transport=[^;?]*">>
                    ,<<";transport=", Transport/binary>>
                    ,[{'return', 'binary'}]
                   )
    of
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
    re:replace(Contact, "^[^\@]+", knm_converters:normalize(Number), [{'return', 'binary'}]);
maybe_format_user(Contact, #bridge_endpoint{invite_format = <<"npan">>, number=Number}) ->
    re:replace(Contact, "^[^\@]+", knm_converters:to_npan(Number), [{'return', 'binary'}]);
maybe_format_user(Contact, #bridge_endpoint{invite_format = <<"1npan">>, number=Number}) ->
    re:replace(Contact, "^[^\@]+", knm_converters:to_1npan(Number), [{'return', 'binary'}]);
maybe_format_user(Contact, _) -> Contact.

-spec maybe_set_interface(ne_binary(), bridge_endpoint()) -> ne_binary().
maybe_set_interface(<<"sofia/", _/binary>>=Contact, _) -> Contact;
maybe_set_interface(<<"loopback/", _/binary>>=Contact, _) -> Contact;
maybe_set_interface(Contact, #bridge_endpoint{sip_interface='undefined'}=Endpoint) ->
    Options = ['ungreedy', {'capture', 'all_but_first', 'binary'}],
    case re:run(Contact, <<";fs_path=sip:(.*):\\d*;">>, Options) of
        {'match', FsPath} ->
            SIPInterface = kz_util:to_binary(get_sip_interface_from_db(FsPath)),
            maybe_set_interface(Contact, Endpoint#bridge_endpoint{sip_interface=SIPInterface});
        'nomatch' ->
            <<"sofia/", ?SIP_INTERFACE, "/", Contact/binary>>
    end;
maybe_set_interface(Contact, #bridge_endpoint{sip_interface= <<"sofia/", _/binary>>=SIPInterface}) ->
    <<(kz_util:strip_right_binary(SIPInterface, $/))/binary, "/", Contact/binary>>;
maybe_set_interface(Contact, #bridge_endpoint{sip_interface=SIPInterface}) ->
    <<"sofia/", SIPInterface/binary, "/", Contact/binary>>.

-spec append_channel_vars(ne_binary(), bridge_endpoint()) -> ne_binary().
append_channel_vars(Contact, #bridge_endpoint{include_channel_vars='false'}) ->
    'false' = kz_util:is_empty(Contact),
    Contact;
append_channel_vars(Contact, #bridge_endpoint{channel_vars=["[",[],"]"]}) ->
    'false' = kz_util:is_empty(Contact),
    Contact;
append_channel_vars(Contact, #bridge_endpoint{channel_vars=ChannelVars}) ->
    'false' = kz_util:is_empty(Contact),
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
    <<Prefix/binary, "Event-Name=CUSTOM,Event-Subclass=kazoo::masquerade"
      ,",kazoo_event_name=", EventName/binary
      ,",kazoo_application_name=", Application/binary
    >>.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec media_path(ne_binary(), ne_binary(), kz_json:object()) -> ne_binary().
media_path(MediaName, UUID, JObj) -> media_path(MediaName, 'new', UUID, JObj).

-spec media_path(api_binary(), media_types(), ne_binary(), kz_json:object()) -> ne_binary().
media_path('undefined', _Type, _UUID, _) -> <<"silence_stream://5">>;
media_path(<<>>, _Type, _UUID, _) -> <<"silence_stream://5">>;
media_path(MediaName, Type, UUID, JObj) when not is_binary(MediaName) ->
    media_path(kz_util:to_binary(MediaName), Type, UUID, JObj);
media_path(<<"silence">> = Media, _Type, _UUID, _) -> Media;
media_path(<<"silence_stream://", _/binary>> = Media, _Type, _UUID, _) -> Media;
media_path(<<"tone_stream://", _/binary>> = Media, _Type, _UUID, _) -> Media;
media_path(<<"local_stream://", FSPath/binary>>, _Type, _UUID, _) -> recording_filename(FSPath);
media_path(<<?LOCAL_MEDIA_PATH, _/binary>> = FSPath, _Type, _UUID, _) -> FSPath;
media_path(<<"http://", _/binary>> = URI, _Type, _UUID, _) -> get_fs_playback(URI);
media_path(<<"https://", _/binary>> = URI, _Type, _UUID, _) -> get_fs_playback(URI);
media_path(<<?HTTP_GET_PREFIX, _/binary>> = Media, _Type, _UUID, _) -> Media;
media_path(<<"$", _/binary>> = Media, _Type, _UUID, _) -> Media;
media_path(MediaName, Type, UUID, JObj) ->
    case lookup_media(MediaName, UUID, JObj, Type) of
        {'error', _E} ->
            lager:warning("failed to get media path for ~s: ~p", [MediaName, _E]),
            kz_util:to_binary(MediaName);
        {'ok', Path} ->
            lager:debug("found path ~s for ~s", [Path, MediaName]),
            kz_util:to_binary(get_fs_playback(Path))
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
                                   ,<<(amqp_util:encode(RootName))/binary, Ext/binary>>
                                  ]),
    _ = kz_cache:store_local(?ECALLMGR_UTIL_CACHE
                             ,?ECALLMGR_PLAYBACK_MEDIA_KEY(MediaName)
                             ,RecordingName
                            ),
    RecordingName.

-spec recording_directory(ne_binary()) -> ne_binary().
recording_directory(<<"/", _/binary>> = FullPath) -> filename:dirname(FullPath);
recording_directory(_RelativePath) -> ecallmgr_config:get(<<"recording_file_path">>, <<"/tmp/">>).

-spec recording_extension(ne_binary()) -> ne_binary().
recording_extension(MediaName) ->
    case filename:extension(MediaName) of
        Empty when Empty =:= <<>> orelse Empty =:= [] ->
            ecallmgr_config:get(<<"default_recording_extension">>, <<".mp3">>);
        <<".mp3">> = MP3 -> MP3;
        <<".mp4">> = MP4 -> MP4;
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

-spec maybe_playback_via_vlc(ne_binary()) -> ne_binary().
maybe_playback_via_vlc(URI) ->
    case kz_util:is_true(ecallmgr_config:get(<<"use_vlc">>, 'false')) of
        'false' -> maybe_playback_via_shout(URI);
        'true' ->
            lager:debug("media is streamed via VLC, prepending ~s", [URI]),
            <<"vlc://", URI/binary>>
    end.

-spec maybe_playback_via_shout(ne_binary()) -> ne_binary().
maybe_playback_via_shout(URI) ->
    case filename:extension(URI) =:= <<".mp3">>
        andalso kz_util:is_true(ecallmgr_config:get(<<"use_shout">>, 'false'))
    of
        'false' -> maybe_playback_via_http_cache(URI);
        'true' ->
            lager:debug("media is streamed via shout, updating ~s", [URI]),
            binary:replace(URI, [<<"http">>, <<"https">>], <<"shout">>)
    end.

-spec maybe_playback_via_http_cache(ne_binary()) -> ne_binary().
maybe_playback_via_http_cache(<<?HTTP_GET_PREFIX, _/binary>> = URI) ->
    lager:debug("media is streamed via http_cache, using ~s", [URI]),
    URI;
maybe_playback_via_http_cache(URI) ->
    case kz_util:is_true(ecallmgr_config:get(<<"use_http_cache">>, 'true')) of
        'false' ->
            lager:debug("using straight URI ~s", [URI]),
            URI;
        'true' ->
            lager:debug("media is streamed via http_cache, using ~s", [URI]),
            <<?HTTP_GET_PREFIX, URI/binary>>
    end.

%% given a proplist of a FS event, return the Kazoo-equivalent app name(s).
%% a FS event could have multiple Kazoo equivalents
-spec convert_fs_evt_name(ne_binary()) -> ne_binaries().
convert_fs_evt_name(EvtName) ->
    [WhAppEvt || {FSEvt, WhAppEvt} <- ?FS_APPLICATION_NAMES, FSEvt =:= EvtName].

%% given a Kazoo Dialplan Application name, return the FS-equivalent event name
%% A Kazoo Dialplan Application name is 1-to-1 with the FS-equivalent
-spec convert_kazoo_app_name(ne_binary()) -> ne_binaries().
convert_kazoo_app_name(App) ->
    [EvtName || {EvtName, AppName} <- ?FS_APPLICATION_NAMES, App =:= AppName].

-type media_types() :: 'new' | 'extant'.
-spec lookup_media(ne_binary(), ne_binary(), kz_json:object(), media_types()) ->
                          {'ok', ne_binary()} |
                          {'error', any()}.
lookup_media(MediaName, CallId, JObj, Type) ->
    case kz_cache:fetch_local(?ECALLMGR_UTIL_CACHE
                              ,?ECALLMGR_PLAYBACK_MEDIA_KEY(MediaName)
                             )
    of
        {'ok', _Path}=Ok ->
            lager:debug("media ~s exists in playback cache as ~s", [MediaName, _Path]),
            Ok;
        {'error', 'not_found'} ->
            request_media_url(MediaName, CallId, JObj, Type)
    end.

-spec request_media_url(ne_binary(), ne_binary(), kz_json:object(), media_types()) ->
                               {'ok', ne_binary()} |
                               {'error', any()}.
request_media_url(MediaName, CallId, JObj, Type) ->
    Request = kz_json:set_values(
                props:filter_undefined(
                  [{<<"Media-Name">>, MediaName}
                   ,{<<"Stream-Type">>, kz_util:to_binary(Type)}
                   ,{<<"Call-ID">>, CallId}
                   ,{<<"Msg-ID">>, kz_util:rand_hex_binary(8)}
                   | kz_api:default_headers(<<"media">>, <<"media_req">>, ?APP_NAME, ?APP_VERSION)
                  ])
                ,JObj),
    case kz_amqp_worker:call_collect(Request
                                     ,fun kapi_media:publish_req/1
                                     ,{'media_mgr', fun kapi_media:resp_v/1}
                                    )
    of
        {'ok', MediaResp} ->
            MediaUrl = kz_json:find(<<"Stream-URL">>, MediaResp, <<>>),
            CacheProps = media_url_cache_props(MediaName),
            _ = kz_cache:store_local(?ECALLMGR_UTIL_CACHE
                                     ,?ECALLMGR_PLAYBACK_MEDIA_KEY(MediaName)
                                     ,MediaUrl
                                     ,CacheProps
                                    ),
            lager:debug("media ~s stored to playback cache : ~s", [MediaName, MediaUrl]),
            {'ok', MediaUrl};
        {'returned', _JObj, _BR} ->
            lager:debug("no media manager available", []),
            {'error', 'timeout'};
        {'timeout', _Resp} ->
            lager:debug("timeout when getting media url from amqp", []),
            {'error', 'timeout'};
        {'error', _R}=E ->
            lager:debug("error when getting media url from amqp ~p", [_R]),
            E
    end.

-spec media_url_cache_props(ne_binary()) -> kz_cache:store_options().
media_url_cache_props(<<"/", _/binary>> = MediaName) ->
    case binary:split(MediaName, <<"/">>, ['global']) of
        [<<>>, AccountId, MediaId] ->
            AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
            [{'origin', {'db', AccountDb, MediaId}}];
        _Parts -> []
    end;
media_url_cache_props(<<"prompt://", Prompt/binary>>) ->
    case binary:split(Prompt, <<"/">>) of
        [?KZ_MEDIA_DB, _MediaId] ->
            [{'origin', {'db', ?KZ_MEDIA_DB, <<"media">>}}];
        [AccountId, _MediaId] ->
            AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
            [{'origin', {'db', AccountDb, <<"media">>}}];
        _ -> []
    end;
media_url_cache_props(<<"tts://", Text/binary>>) ->
    Id = kz_util:binary_md5(Text),
    [{'origin', {'db', <<"tts">>, Id}}];
media_url_cache_props(_MediaName) -> [].

-spec custom_sip_headers(kz_proplist()) -> kz_proplist().
custom_sip_headers(Props) ->
    lists:foldl(fun maybe_aggregate_headers/2
                ,[]
                ,props:filter(fun is_custom_sip_header/1, Props)
               ).

-spec maybe_aggregate_headers({ne_binary(), ne_binary()}, kz_proplist()) ->
                                     kz_proplist().
maybe_aggregate_headers(KV, Acc) ->
    {K, V} = normalize_custom_sip_header_name(KV),
    maybe_aggregate_headers(K, V, Acc).

maybe_aggregate_headers(<<"Diversion">>, Diversion, Acc) ->
    lager:debug("adding diversion ~s to SIP headers", [Diversion]),
    Diversions = props:get_value(<<"Diversions">>, Acc, []),
    props:set_value(<<"Diversions">>, [Diversion | Diversions], Acc);
maybe_aggregate_headers(K, V, Acc) ->
    [{K,V} | Acc].

-spec normalize_custom_sip_header_name(any()) -> any().
normalize_custom_sip_header_name({<<"variable_sip_h_", K/binary>>, V}) -> {K, V};
normalize_custom_sip_header_name({<<"sip_h_", K/binary>>, V}) -> {K, V};
normalize_custom_sip_header_name(A) -> A.

-spec is_custom_sip_header(any()) -> boolean().
is_custom_sip_header({<<"P-", _/binary>>, _}) -> 'true';
is_custom_sip_header({<<"X-", _/binary>>, _}) -> 'true';
is_custom_sip_header({<<"sip_h_", _/binary>>, _}) -> 'true';
is_custom_sip_header({<<"variable_sip_h_X-", ?CHANNEL_VAR_PREFIX, _/binary>>, _}) -> 'false';
is_custom_sip_header({<<"variable_sip_h_", _/binary>>, _}) -> 'true';
is_custom_sip_header(_Header) -> 'false'.

-spec maybe_add_expires_deviation(api_integer()) -> api_integer().
maybe_add_expires_deviation('undefined') -> 'undefined';
maybe_add_expires_deviation(Expires) when not is_integer(Expires) ->
    maybe_add_expires_deviation(kz_util:to_integer(Expires));
maybe_add_expires_deviation(0) -> 0;
maybe_add_expires_deviation(Expires) ->
    Expires + ecallmgr_config:get_integer(<<"expires_deviation_time">>, 180).

-spec maybe_add_expires_deviation_ms(api_integer()) -> api_integer().
maybe_add_expires_deviation_ms('undefined') -> 'undefined';
maybe_add_expires_deviation_ms(Expires) when not is_integer(Expires) ->
    maybe_add_expires_deviation_ms(kz_util:to_integer(Expires));
maybe_add_expires_deviation_ms(Expires) ->
    maybe_add_expires_deviation(Expires) * ?MILLISECONDS_IN_SECOND.

-spec get_dial_separator(api_object() | ne_binary(), kz_json:objects()) -> ne_binary().
get_dial_separator(?DIAL_METHOD_SIMUL, [_|T]) when T =/= [] -> ?SEPARATOR_SIMULTANEOUS;
get_dial_separator(?DIAL_METHOD_SINGLE, _Endpoints) -> ?SEPARATOR_SINGLE;
get_dial_separator('undefined', _Endpoints) -> ?SEPARATOR_SINGLE;
get_dial_separator(JObj, Endpoints) ->
    get_dial_separator(kz_json:get_value(<<"Dial-Endpoint-Method">>, JObj, ?DIAL_METHOD_SINGLE)
                       ,Endpoints
                      ).

-spec fix_contact(api_binary() | list(), ne_binary(), ne_binary()) -> api_binary().
fix_contact('undefined', _, _) -> 'undefined';
fix_contact(<<";", _/binary>> = OriginalContact, Username, Realm) ->
    fix_contact(<<"sip:", Username/binary, "@", Realm/binary, OriginalContact/binary>>, Username, Realm);
fix_contact(OriginalContact, Username, Realm)
  when is_binary(OriginalContact) ->
    fix_contact(binary:split(kz_util:strip_binary(OriginalContact), <<";">>, ['global']), Username, Realm);
fix_contact([<<>> | Options], Username, Realm) ->
    [<<"sip:", Username/binary, "@", Realm/binary>> | Options];
fix_contact([Contact | Options], Username, Realm) ->
    case kzsip_uri:uris(Contact) of
        [#uri{user = <<>>, domain = <<>>}=Uri] ->
            fix_contact([kzsip_uri:ruri(Uri#uri{user=Username, domain=Realm}) | Options], Username, Realm);
        [#uri{user = <<>>}=Uri] ->
            fix_contact([kzsip_uri:ruri(Uri#uri{user=Username}) | Options], Username, Realm);
        [#uri{domain = <<>>}=Uri] ->
            fix_contact([kzsip_uri:ruri(Uri#uri{domain=Realm}) | Options], Username, Realm);
        [#uri{}=Uri] ->
            list_to_binary([kzsip_uri:ruri(Uri)] ++ [<<";", Option/binary>> || Option <- Options]);
        _Else -> 'undefined'
    end.
