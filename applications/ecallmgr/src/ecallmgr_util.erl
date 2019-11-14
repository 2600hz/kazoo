%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Various utilities specific to ecallmgr. More general utilities go
%%% in kazoo_util.erl
%%%
%%%
%%% @author James Aimonetti <james@2600hz.org>
%%% @author Karl Anderson <karl@2600hz.org>
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_util).

-export([send_cmd/4]).
-export([get_fs_kv/2, get_fs_kv/3, get_fs_key_and_value/3]).
-export([get_fs_key/1]).
-export([process_fs_kv/4, format_fs_kv/4]).
-export([fs_args_to_binary/1, fs_args_to_binary/2, fs_args_to_binary/3]).
-export([fs_arg_encode/1, fs_arg_encode/2]).
-export([multi_set_args/3, multi_unset_args/3]).
-export([multi_set_args/4, multi_unset_args/4]).
-export([multi_set_args/5, multi_unset_args/5]).

-export([get_expires/1]).
-export([get_interface_list/1, get_interface_properties/1, get_interface_properties/2]).
-export([get_sip_to/1, get_sip_from/1, get_sip_request/1, get_orig_ip/1, get_orig_port/1]).

-export([custom_channel_vars/1
        ,custom_application_vars/1
        ,conference_channel_vars/1
        ]).

-export([eventstr_to_proplist/1, varstr_to_proplist/1, get_setting/1, get_setting/2]).
-export([is_node_up/1, is_node_up/2]).
-export([build_bridge_string/1, build_bridge_string/2]).
-export([build_channel/1]).
-export([build_bridge_channels/1, build_simple_channels/1]).
-export([create_masquerade_event/2, create_masquerade_event/3]).
-export([media_path/1, media_path/2, media_path/3, media_path/4
        ,lookup_media/4
        ]).
-export([remove_media/1]).
-export([unserialize_fs_array/1, unserialize_fs_props/1]).
-export([convert_fs_evt_name/1, convert_kazoo_app_name/1]).
-export([fax_filename/1
        ,recording_filename/1
        ]).
-export([maybe_sanitize_fs_value/2]).

-export([custom_sip_headers/1 , is_custom_sip_header/1, normalize_custom_sip_header_name/1]).
-export([maybe_add_expires_deviation/1, maybe_add_expires_deviation_ms/1]).

-export([get_dial_separator/2]).
-export([fix_contact/3]).

-export([dialplan_application/1]).

-ifdef(TEST).
-export([endpoint_jobjs_to_records/1
        ,maybe_filter_failover_channels/2
        ,build_bridge_channels/2
        ]).
-endif.

-include("ecallmgr.hrl").
-include_lib("kazoo_amqp/src/api/kapi_dialplan.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_sip/include/kzsip_uri.hrl").

-define(FS_MULTI_VAR_SEP, kapps_config:get_ne_binary(?APP_NAME, <<"multivar_separator">>, <<"~">>)).
-define(FS_MULTI_VAR_SEP_PREFIX, "^^").
-define(SANITIZE_FS_VALUE_REGEX
       ,kapps_config:get_ne_binary(?APP_NAME, <<"sanitize_fs_value_regex">>, <<"[^0-9\\w\\s-]">>)
       ).
-define(FAILOVER_IF_ALL_UNREGED
       ,kapps_config:get_boolean(?APP_NAME, <<"failover_when_all_unreg">>, 'false')
       ).

-type send_cmd_ret() :: fs_sendmsg_ret() | fs_api_ret().
-export_type([send_cmd_ret/0]).

-record(bridge_endpoint, {invite_format = <<"username">> :: kz_term:ne_binary()
                         ,endpoint_type = <<"sip">> :: kz_term:ne_binary()
                         ,ip_address :: kz_term:api_binary()
                         ,username :: kz_term:api_binary()
                         ,user :: kz_term:api_binary()
                         ,realm :: kz_term:api_binary()
                         ,number :: kz_term:api_binary()
                         ,route :: kz_term:api_binary()
                         ,proxy_address :: kz_term:api_binary()
                         ,forward_address :: kz_term:api_binary()
                         ,transport :: kz_term:api_binary()
                         ,span = <<"1">> :: kz_term:ne_binary()
                         ,channel_selection = <<"a">> :: kz_term:ne_binary()
                         ,interface = <<"RR">> :: kz_term:ne_binary() % for Skype
                         ,sip_interface
                         ,channel_vars = [] :: kz_term:ne_binaries()
                         ,header_vars = [] :: kz_term:ne_binaries()
                         ,include_channel_vars = 'true' :: boolean()
                         ,failover :: kz_term:api_object()
                         }).
-type bridge_endpoint() :: #bridge_endpoint{}.

%%------------------------------------------------------------------------------
%% @doc send the SendMsg proplist to the freeswitch node
%% @end
%%------------------------------------------------------------------------------
-spec send_cmd(atom(), kz_term:ne_binary(), kz_term:text(), kz_term:text()) -> send_cmd_ret().
send_cmd(Node, UUID, App, Args) when not is_list(App) ->
    send_cmd(Node, UUID, kz_term:to_list(App), Args);
send_cmd(Node, UUID, "xferext", Dialplan) ->
    XferExt = [begin
                   lager:debug("building xferext on node ~s: ~s", [Node, V]),
                   {kz_term:to_list(K), kz_term:to_list(V)}
               end
               || {K, V} <- Dialplan,
                  not cmd_is_empty({kz_term:to_list(K), kz_term:to_list(V)})
              ],
    'ok' = freeswitch:sendmsg(Node, UUID, [{"call-command", "xferext"} | XferExt]);
send_cmd(Node, UUID, App, Args) when not is_list(Args) ->
    send_cmd(Node, UUID, App, kz_term:to_list(Args));
send_cmd(_Node, _UUID, "kz_multiset", "^^") -> 'ok';
send_cmd(Node, UUID, "playstop", _Args) ->
    lager:debug("execute on node ~s: uuid_break(~s all)", [Node, UUID]),
    freeswitch:api(Node, 'uuid_break', kz_term:to_list(<<UUID/binary, " all">>));
send_cmd(Node, UUID, "unbridge", _) ->
    lager:debug("execute on node ~s: uuid_park(~s)", [Node, UUID]),
    freeswitch:api(Node, 'uuid_park', kz_term:to_list(UUID));
send_cmd(Node, _UUID, "broadcast", Args) ->
    lager:debug("execute on node ~s: uuid_broadcast(~s)", [Node, Args]),
    Resp = freeswitch:api(Node, 'uuid_broadcast', kz_term:to_list(iolist_to_binary(Args))),
    lager:debug("broadcast resulted in: ~p", [Resp]),
    Resp;
send_cmd(Node, UUID, "call_pickup", Target) ->
    Args = iolist_to_binary([UUID, " ", Target]),
    lager:debug("execute on node ~s: uuid_bridge(~s)", [Node, Args]),
    freeswitch:api(Node, 'uuid_bridge', kz_term:to_list(Args));
send_cmd(Node, UUID, "hangup", Args) ->
    lager:debug("terminate call on node ~s", [Node]),
    freeswitch:api(Node, 'uuid_kill', iolist_to_binary([UUID, " ", Args]));
send_cmd(Node, UUID, "break", _) ->
    lager:debug("break call on node ~s", [Node]),
    freeswitch:api(Node, 'uuid_break', kz_term:to_list(UUID));
send_cmd(Node, _UUID, "audio_level", Args) ->
    lager:debug("execute on node ~s: uuid_audio ~p", [Node, Args]),
    freeswitch:api(Node, 'uuid_audio', kz_term:to_list(iolist_to_binary(Args)));
send_cmd(Node, UUID, "conference", Args) ->
    Args1 = iolist_to_binary([UUID, " conference:", Args, " inline"]),
    lager:debug("starting conference on ~s: ~s", [Node, Args1]),
    freeswitch:api(Node, 'uuid_transfer', kz_term:to_list(Args1));
send_cmd(Node, _UUID, "transfer", Args) ->
    lager:debug("transferring on ~s: ~s", [Node, Args]),
    freeswitch:api(Node, 'uuid_transfer', kz_term:to_list(Args));
send_cmd(Node, _UUID, "uuid_" ++ _ = API, Args) ->
    lager:debug("using api for ~s command ~s: ~s", [API, Node, Args]),
    freeswitch:api(Node, kz_term:to_atom(API, 'true'), kz_term:to_list(Args));
send_cmd(Node, _UUID, "kz_uuid_" ++ _ = API, Args) ->
    lager:debug("using api for ~s command ~s: ~s", [API, Node, Args]),
    freeswitch:api(Node, kz_term:to_atom(API, 'true'), kz_term:to_list(Args));
send_cmd(Node, UUID, App, Args) ->
    AppName = dialplan_application(App),
    case freeswitch:sendmsg(Node, UUID, [{"call-command", "execute"}
                                        ,{"execute-app-name", AppName}
                                        ,{"execute-app-arg", kz_term:to_list(Args)}
                                        ])
    of
        {'error', 'baduuid'} ->
            lager:info("uuid ~s on node ~s is bad", [UUID, Node]),
            throw({'error', 'baduuid'});
        Result ->
            lager:debug("execute on node ~s(~s) ~s(~s): ~p"
                       ,[Node, UUID, AppName, Args, Result]
                       ),
            Result
    end.

-spec cmd_is_empty({list(), list()}) -> boolean().
cmd_is_empty({"kz_multiset", "^^"}) -> 'true';
cmd_is_empty({_, "kz_multiset ^^"}) -> 'true';
cmd_is_empty(_) -> 'false'.

-spec dialplan_application(string()) -> string().
dialplan_application("blind_xfer") -> "transfer";
dialplan_application(App) -> App.

-spec get_expires(kz_term:proplist()) -> integer().
get_expires(Props) ->
    Expiry = kz_term:to_integer(props:get_first_defined([<<"Expires">>, <<"expires">>], Props, 300)),
    round(Expiry * 1.25).


-spec get_interface_list(atom()) -> kz_term:ne_binaries().
get_interface_list(Node) ->
    case freeswitch:api(Node, 'sofia', "status") of
        {'ok', Response} ->
            R = binary:replace(Response, <<" ">>, <<>>, ['global']),
            Lines = binary:split(R, <<"\n">>, ['global']),
            [KV || Line <- Lines,
                   (KV = case binary:split(Line, <<"\t">>, ['global']) of
                             [<<"Name">>, _T, _A, _S] -> 'false';
                             [I, <<"profile">>, _A, <<"RUNNING", _/binary>>] -> I;
                             _Other -> 'false'
                         end) =/= 'false'
            ];
        _Else -> []
    end.


-spec get_interface_properties(atom()) -> kz_term:proplist().
get_interface_properties(Node) ->
    [{Interface, get_interface_properties(Node, Interface)} || Interface <- get_interface_list(Node)].

-spec get_interface_properties(atom(), kz_term:ne_binary()) -> kz_term:proplist().
get_interface_properties(Node, Interface) ->
    case freeswitch:api(Node, 'sofia', kz_term:to_list(list_to_binary(["status profile ", Interface]))) of
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
-spec get_sip_to(kzd_freeswitch:data()) -> kz_term:ne_binary().
get_sip_to(Props) ->
    get_sip_to(Props, kzd_freeswitch:original_call_direction(Props)).

get_sip_to(Props, <<"outbound">>) ->
    case props:get_value(<<"Channel-Presence-ID">>, Props) of
        'undefined' ->
            Number = props:get_first_defined([<<"Other-Leg-Callee-ID-Number">>
                                             ,<<"variable_sip_to_user">>
                                             ]
                                            ,Props
                                            ,<<"nouser">>
                                            ),
            Realm = get_sip_to_realm(Props),
            <<Number/binary, "@", Realm/binary>>;
        PresenceId -> PresenceId
    end;
get_sip_to(Props, _) ->
    case props:get_first_defined([<<"variable_sip_to_uri">>
                                 ,<<"variable_sip_req_uri">>
                                 ]
                                ,Props
                                )
    of
        'undefined' -> get_sip_request(Props);
        ToUri -> ToUri
    end.

-spec get_sip_to_realm(kzd_freeswitch:data()) -> kz_term:ne_binary().
get_sip_to_realm(Props) ->
    case kzd_freeswitch:to_realm(Props) of
        'undefined' -> ?DEFAULT_REALM;
        Realm -> Realm
    end.

%% retrieves the sip address for the 'from' field

-spec get_sip_from(kz_term:proplist()) -> kz_term:ne_binary().
get_sip_from(Props) ->
    get_sip_from(Props, kzd_freeswitch:original_call_direction(Props)).

-spec get_sip_from(kz_term:proplist(), kz_term:api_binary()) -> kz_term:ne_binary().
get_sip_from(Props, <<"outbound">>) ->
    Num = props:get_first_defined([<<"Other-Leg-RDNIS">>
                                  ,<<"Other-Leg-Caller-ID-Number">>
                                  ,<<"variable_sip_from_user">>
                                  ,<<"variable_sip_from_uri">>
                                  ]
                                 ,Props
                                 ,<<"nouser">>
                                 ),
    [Number | _] = binary:split(Num, <<"@">>, ['global']),
    Realm = get_sip_from_realm(Props),
    <<Number/binary, "@", Realm/binary>>;
get_sip_from(Props, _) ->
    Default = list_to_binary([kzd_freeswitch:from_user(Props)
                             ,"@"
                             ,get_sip_from_realm(Props)
                             ]),

    props:get_first_defined([<<"Channel-Presence-ID">>
                            ,<<"variable_sip_from_uri">>
                            ]
                           ,Props
                           ,Default
                           ).

get_sip_from_realm(Props) ->
    case kzd_freeswitch:from_realm(Props) of
        'undefined' -> ?DEFAULT_REALM;
        Realm -> Realm
    end.

%% retrieves the sip address for the 'request' field
-spec get_sip_request(kzd_freeswitch:data()) -> kz_term:ne_binary().
get_sip_request(Props) ->
    U = props:get_first_defined([<<"Hunt-Destination-Number">>
                                ,<<"variable_sip_req_uri">>
                                ,<<"variable_sip_loopback_req_uri">>
                                ,<<"Caller-Destination-Number">>
                                ,<<"variable_sip_to_user">>
                                ]
                               ,Props
                               ,<<"nouser">>
                               ),
    [User | _] = binary:split(U, <<"@">>, ['global']),
    Realm = get_sip_request_realm(Props),
    <<User/binary, "@", Realm/binary>>.

-spec get_sip_request_realm(kzd_freeswitch:data()) -> kz_term:ne_binary().
get_sip_request_realm(Props) ->
    case kzd_freeswitch:request_realm(Props) of
        'undefined' -> ?DEFAULT_REALM;
        Realm -> Realm
    end.

-spec get_orig_ip(kz_term:proplist()) -> kz_term:api_binary().
get_orig_ip(Prop) ->
    props:get_first_defined([<<"X-AUTH-IP">>, <<"ip">>], Prop).

-spec get_orig_port(kz_term:proplist()) -> kz_term:api_binary().
get_orig_port(Prop) ->
    case props:get_first_defined([<<"X-AUTH-PORT">>, <<"port">>], Prop) of
        <<>> -> 'undefined';
        <<"0">> -> 'undefined';
        Port -> Port
    end.

-spec get_sip_interface_from_db(kz_term:ne_binaries()) -> kz_term:ne_binary().
get_sip_interface_from_db([FsPath]) ->
    NetworkMap = kapps_config:get_json(?APP_NAME, <<"network_map">>, kz_json:new()),
    case map_fs_path_to_sip_profile(FsPath, NetworkMap) of
        'undefined' ->
            lager:debug("unable to find network map for ~s, using default interface '~s'"
                       ,[FsPath, ?SIP_INTERFACE]),
            ?SIP_INTERFACE;
        Else ->
            lager:debug("found custom interface '~s' in network map for ~s", [Else, FsPath]),
            Else
    end.

-spec map_fs_path_to_sip_profile(kz_term:ne_binary(), kz_json:object()) -> kz_term:api_binary().
map_fs_path_to_sip_profile(FsPath, NetworkMap) ->
    SIPInterfaceObj = kz_json:filter(fun({K, _}) ->
                                             kz_network_utils:verify_cidr(FsPath, K)
                                     end, NetworkMap),
    case kz_json:get_values(SIPInterfaceObj) of
        {[], _Keys} -> 'undefined';
        {[V|_], _Keys} -> kz_json:get_ne_value(<<"custom_sip_interface">>, V)
    end.

-spec conference_channel_vars(kzd_freeswitch:data()) -> kz_term:proplist().
conference_channel_vars(Props) ->
    [conference_channel_var_map(KV) || KV <- conference_channel_vars(Props, [])].

conference_channel_vars(Props, Initial) ->
    lists:foldl(fun conference_channel_vars_fold/2, Initial, Props).

-spec conference_channel_vars_fold({kz_term:ne_binary(), kz_term:ne_binary()}, kz_term:proplist()) -> kz_term:proplist().
conference_channel_vars_fold({Key, V}, Acc) ->
    case lists:member(Key, ?CONFERENCE_VARS) of
        'true' -> [{Key, V} | Acc];
        'false' -> Acc
    end.

conference_channel_var_map({Key, Value}=KV) ->
    case props:get_value(Key, ?CONFERENCE_VAR_MAP) of
        'undefined' -> KV;
        {NewName, Fun} -> {NewName, Fun(Value)};
        Fun -> {Key, Fun(Value)}
    end.

-spec channel_var_map({kz_term:ne_binary(), kz_term:ne_binary()}) -> {kz_term:ne_binary(), kz_term:ne_binary() | kz_term:ne_binaries()}.
channel_var_map({Key, <<"ARRAY::", Serialized/binary>>}) ->
    {Key, binary:split(Serialized, <<"|:">>, ['global'])};
channel_var_map({Key, Other}) -> {Key, Other}.

%% Extract custom channel variables to include in the event

-spec custom_channel_vars(kzd_freeswitch:data()) -> kz_term:proplist().
custom_channel_vars(Props) ->
    lists:map(fun channel_var_map/1, custom_channel_vars(Props, [])).

-spec custom_channel_vars(kz_term:proplist(), kz_term:proplist()) -> kz_term:proplist().
custom_channel_vars(Props, Initial) ->
    CCVs = lists:foldl(fun custom_channel_vars_fold/2, Initial, Props),
    maybe_update_referred_ccv(Props, channel_vars_sort(CCVs)).

-spec channel_vars_sort(kz_term:proplist()) -> kz_term:proplist().
channel_vars_sort(ChannelVars) ->
    lists:usort(fun channel_var_sort/2, ChannelVars).

-spec channel_var_sort(tuple(), tuple()) -> boolean().
channel_var_sort({A, _}, {B, _}) -> A =< B.

-spec custom_channel_vars_fold({kz_term:ne_binary(), kz_term:ne_binary()}, kz_term:proplist()) -> kz_term:proplist().
custom_channel_vars_fold({?GET_CCV(Key), V}, Acc) ->
    props:set_value(Key, V, Acc);
custom_channel_vars_fold({?CCV(Key), V}, Acc) ->
    props:set_value(Key, V, Acc);
custom_channel_vars_fold({?GET_CCV_HEADER(Key), V}, Acc) ->
    props:insert_value(Key, V, Acc);
custom_channel_vars_fold(_KV, Acc) -> Acc.

-spec custom_application_vars(kzd_freeswitch:data()) -> kz_term:proplist().
custom_application_vars(Props) ->
    lists:map(fun application_var_map/1, custom_application_vars(Props, [])).

-spec custom_application_vars(kz_term:proplist(), kz_term:proplist()) -> kz_term:proplist().
custom_application_vars(Props, Initial) ->
    CCVs = lists:foldl(fun custom_application_vars_fold/2, Initial, Props),
    application_vars_sort(CCVs).

-spec application_vars_sort(kz_term:proplist()) -> kz_term:proplist().
application_vars_sort(ApplicationVars) ->
    lists:usort(fun application_var_sort/2, ApplicationVars).

-spec application_var_sort(tuple(), tuple()) -> boolean().
application_var_sort({A, _}, {B, _}) -> A =< B.

-spec custom_application_vars_fold({kz_term:ne_binary(), kz_term:ne_binary()}, kz_term:proplist()) -> kz_term:proplist().
custom_application_vars_fold({?GET_CAV(Key), V}, Acc) ->
    props:set_value(Key, V, Acc);
custom_application_vars_fold({?CAV(Key), V}, Acc) ->
    props:set_value(Key, V, Acc);
custom_application_vars_fold({?GET_CAV_HEADER(Key), V}, Acc) ->
    props:insert_value(Key, V, Acc);
custom_application_vars_fold(_KV, Acc) -> Acc.

-spec application_var_map({kz_term:ne_binary(), kz_term:ne_binary()}) -> {kz_term:ne_binary(), kz_term:ne_binary() | kz_term:ne_binaries()}.
application_var_map({Key, <<"ARRAY::", Serialized/binary>>}) ->
    {Key, binary:split(Serialized, <<"|:">>, ['global'])};
application_var_map({Key, Other}) -> {Key, Other}.

-spec maybe_update_referred_ccv(kz_term:proplist(), kz_term:proplist()) -> kz_term:proplist().
maybe_update_referred_ccv(Props, CCVs) ->
    ReferTo = props:get_value(<<"variable_sip_refer_to">>, Props),
    update_referred_by_ccv(props:get_value(<<"variable_sip_h_Referred-By">>, Props)
                          ,update_referred_to_ccv(ReferTo, CCVs)
                          ).

-spec update_referred_by_ccv(kz_term:api_binary(), kz_term:proplist()) -> kz_term:proplist().
update_referred_by_ccv('undefined', CCVs) -> props:delete(<<"Referred-By">>, CCVs);
update_referred_by_ccv(ReferredBy, CCVs) ->
    props:set_value(<<"Referred-By">>
                   ,kz_http_util:urldecode(ReferredBy)
                   ,CCVs
                   ).

-spec update_referred_to_ccv(kz_term:api_binary(), kz_term:proplist()) -> kz_term:proplist().
update_referred_to_ccv('undefined', CCVs) -> props:delete(<<"Referred-To">>, CCVs);
update_referred_to_ccv(ReferredTo, CCVs) ->
    props:set_value(<<"Referred-To">>
                   ,kz_http_util:urldecode(ReferredTo)
                   ,CCVs
                   ).

%% convert a raw FS string of headers to a proplist
%% "Event-Name: NAME\nEvent-Timestamp: 1234\n" -> [{<<"Event-Name">>, <<"NAME">>}, {<<"Event-Timestamp">>, <<"1234">>}]
-spec eventstr_to_proplist(kz_term:text()) -> kz_term:proplist().
eventstr_to_proplist(EvtStr) ->
    [to_kv(X, ": ") || X <- string:tokens(kz_term:to_list(EvtStr), "\n")].

-spec to_kv(nonempty_string(), nonempty_string()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
to_kv(X, Separator) ->
    [K, V] = string:tokens(X, Separator),
    [{V1, _}] = kz_http_util:parse_query_string(list_to_binary(V)),
    {kz_term:to_binary(K), kz_term:to_binary(fix_value(K, V1))}.

fix_value("Event-Date-Timestamp", TStamp) ->
    kz_time:microseconds_to_seconds(kz_term:to_integer(TStamp));
fix_value(_K, V) -> V.

-spec unserialize_fs_array(kz_term:api_binary()) -> kz_term:ne_binaries().
unserialize_fs_array('undefined') -> [];
unserialize_fs_array(<<"ARRAY::", Serialized/binary>>) ->
    binary:split(Serialized, <<"|:">>, ['global']);
unserialize_fs_array(List)
  when is_list(List) ->
    List;
unserialize_fs_array(Single) ->
    [Single].

-spec unserialize_fs_props(kz_term:proplist()) -> kz_term:proplist().
unserialize_fs_props(Props) ->
    lists:map(fun unserialize_fs_prop/1, Props).

-spec unserialize_fs_prop(tuple()) -> tuple().
unserialize_fs_prop({K, <<"ARRAY::", _/binary>> = V}) ->
    {K, unserialize_fs_array(V)};
unserialize_fs_prop(KV) -> KV.

%% convert a raw FS list of vars to a proplist
%% "Event-Name=NAME,Event-Timestamp=1234" -> [{<<"Event-Name">>, <<"NAME">>}, {<<"Event-Timestamp">>, <<"1234">>}]
-spec varstr_to_proplist(nonempty_string()) -> kz_term:proplist().
varstr_to_proplist(VarStr) ->
    [to_kv(X, "=") || X <- string:tokens(kz_term:to_list(VarStr), ",")].

-spec get_setting(kz_json:get_key()) -> {'ok', any()}.
get_setting(<<"default_ringback">>=Key) ->
    {'ok', kapps_config:get(?APP_NAME, Key, <<"%(2000,4000,440,480)">>)};
get_setting(Setting) -> {'ok', kapps_config:get(?APP_NAME, Setting)}.

-spec get_setting(kz_json:path(), Default) -> {'ok', Default | any()}.
get_setting(Setting, Default) -> {'ok', kapps_config:get(?APP_NAME, Setting, Default)}.

-spec is_node_up(atom()) -> boolean().
is_node_up(Node) -> ecallmgr_fs_nodes:is_node_up(Node).

-spec is_node_up(atom(), kz_term:ne_binary()) -> boolean().
is_node_up(Node, UUID) ->
    ecallmgr_fs_nodes:is_node_up(Node)
        andalso ecallmgr_fs_channel:exists(UUID).

%%------------------------------------------------------------------------------
%% @doc set channel and call variables in FreeSWITCH
%% @end
%%------------------------------------------------------------------------------
-spec multi_set_args(atom(), kz_term:ne_binary(), kz_term:proplist()) -> binary().
multi_set_args(Node, UUID, KVs) ->
    multi_set_args(Node, UUID, KVs, ?FS_MULTI_VAR_SEP).

-spec multi_set_args(atom(), kz_term:ne_binary(), kz_term:proplist(), kz_term:ne_binary()) -> binary().
multi_set_args(Node, UUID, KVs, Separator) ->
    multi_set_args(Node, UUID, KVs, Separator, ?FS_MULTI_VAR_SEP_PREFIX).

-spec multi_set_args(atom(), kz_term:ne_binary(), kz_term:proplist(), kz_term:ne_binary(), binary() | string()) -> binary().
multi_set_args(Node, UUID, KVs, Separator, Prefix) ->
    fs_args_to_binary(lists:reverse(process_fs_kv(Node, UUID, KVs, 'set')), Separator, Prefix).

-spec multi_unset_args(atom(), kz_term:ne_binary(), kz_term:proplist()) -> binary().
multi_unset_args(Node, UUID, KVs) ->
    multi_unset_args(Node, UUID, KVs, ?FS_MULTI_VAR_SEP).

-spec multi_unset_args(atom(), kz_term:ne_binary(), kz_term:proplist(), kz_term:ne_binary()) -> binary().
multi_unset_args(Node, UUID, KVs, Separator) ->
    multi_unset_args(Node, UUID, KVs, Separator, ?FS_MULTI_VAR_SEP_PREFIX).

-spec multi_unset_args(atom(), kz_term:ne_binary(), kz_term:proplist(), kz_term:ne_binary(), binary() | string()) -> binary().
multi_unset_args(Node, UUID, KVs, Separator, Prefix) ->
    fs_args_to_binary(lists:reverse(process_fs_kv(Node, UUID, KVs, 'unset')), Separator, Prefix).

-spec fs_args_to_binary(list()) -> binary().
fs_args_to_binary([_]=Args) ->
    list_to_binary(Args);
fs_args_to_binary(Args) ->
    fs_args_to_binary(Args, ?FS_MULTI_VAR_SEP).

-spec fs_args_to_binary(list(), kz_term:ne_binary()) -> binary().
fs_args_to_binary(Args, Sep) ->
    fs_args_to_binary(Args, Sep, ?FS_MULTI_VAR_SEP_PREFIX).

-spec fs_args_to_binary(list(), kz_term:ne_binary(), binary() | string()) -> binary().
fs_args_to_binary(Args, Sep, Prefix) ->
    Bins = [list_to_binary([Sep, Arg]) || Arg <- Args],
    list_to_binary([Prefix, Bins]).

-spec fs_arg_encode(binary()) -> kz_term:ne_binary().
fs_arg_encode(<<Source/binary>>) ->
    fs_arg_encode(Source, <<>>, <<>>).

-spec fs_arg_encode(binary(), byte() | binary()) -> kz_term:ne_binary().
fs_arg_encode(<<Source/binary>>, Sep) ->
    fs_arg_encode(Source, Sep, <<>>).

-spec fs_arg_encode(binary(), byte() | binary(), binary()) -> binary().
fs_arg_encode(<<>>, _Sep, Acc) -> Acc;

fs_arg_encode(<<C, R/binary>>, C, Acc) ->
    SafeChar = fs_arg_encode_char(C),
    fs_arg_encode(R, C, <<Acc/binary, "%", SafeChar/binary>>);

fs_arg_encode(<<C, R/binary>>, Sep, Acc) ->
    case C of
        $\s -> fs_arg_encode(R, Sep, <<Acc/binary, C>>);
        $. -> fs_arg_encode(R, Sep, <<Acc/binary, C>>);
        $- -> fs_arg_encode(R, Sep, <<Acc/binary, C>>);
        $= -> fs_arg_encode(R, Sep, <<Acc/binary, C>>);
        $_ -> fs_arg_encode(R, Sep, <<Acc/binary, C>>);
        C when C >= $0
               andalso C=< $9 ->
            fs_arg_encode(R, Sep, <<Acc/binary, C>>);
        C when C >= $a
               andalso C=< $z ->
            fs_arg_encode(R, Sep, <<Acc/binary, C>>);
        C when C >= $A
               andalso C=< $Z ->
            fs_arg_encode(R, Sep, <<Acc/binary, C>>);
        _NotSafe ->
            SafeChar = fs_arg_encode_char(C),
            fs_arg_encode(R, Sep, <<Acc/binary, "%", SafeChar/binary>>)
    end.

-spec fs_arg_encode_char(integer()) -> binary().
fs_arg_encode_char(Char) ->
    case integer_to_list(Char, 16) of
        Val when length(Val) < 2 -> list_to_binary(["0", Val]);
        ProperLen                -> list_to_binary(ProperLen)
    end.

-spec process_fs_kv(atom(), kz_term:ne_binary(), kz_term:proplist(), atom()) -> [binary()].
process_fs_kv(_, _, [], _) -> [];
process_fs_kv(Node, UUID, [{_K, 'undefined'} | KVs], Action) ->
    process_fs_kv(Node, UUID, KVs, Action);
process_fs_kv(Node, UUID, [{K, V}|KVs], Action) ->
    X1 = format_fs_kv(K, V, UUID, Action),
    lists:foldl(fun(Prop, Acc) ->
                        process_fs_kv_fold(Node, UUID, Prop, Action, Acc)
                end, X1, KVs);
process_fs_kv(Node, UUID, [K|KVs], 'unset'=Action)
  when is_binary(K) ->
    X1 = get_fs_key(K),
    lists:foldl(fun(Prop, Acc) ->
                        process_fs_kv_fold(Node, UUID, Prop, Action, Acc)
                end, [<<X1/binary, "=">>], KVs).

process_fs_kv_fold(_Node, UUID, {K, V}, Action, Acc) ->
    [format_fs_kv(K, V, UUID, Action) | Acc];
process_fs_kv_fold(_Node, _UUID, K, 'unset', Acc)
  when is_binary(K) ->
    Key = get_fs_key(K),
    [<<Key/binary, "=">> | Acc];
process_fs_kv_fold(_, _, _, _, Acc) ->
    Acc.

-spec format_fs_kv(kz_term:ne_binary(), binary(), kz_term:ne_binary(), atom()) -> [binary()].
format_fs_kv(Key, Value, UUID, 'unset') ->
    case get_fs_key_and_value(Key, Value, UUID) of
        'skip' -> [];
        {K, _V} -> [<<K/binary, "=">>];
        KVs -> [<<K/binary, "=">> || {K,_V} <- KVs]
    end;
format_fs_kv(_Key, 'undefined', _UUID, _) -> [];
format_fs_kv(Key, Value, UUID, _) ->
    case get_fs_key_and_value(Key, Value, UUID) of
        'skip' -> [];
        {K, V} -> [<<K/binary, "=", V/binary>>];
        KVs -> [<<K/binary, "=", V/binary>> || {K,V} <- KVs]
    end.

-spec get_fs_kv(kz_term:ne_binary(), kz_term:ne_binary()) -> binary().
get_fs_kv(Key, Value) ->
    get_fs_kv(Key, Value, 'undefined').

-spec get_fs_kv(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary()) -> binary().
get_fs_kv(<<"Hold-Media">>, Media, UUID) ->
    list_to_binary(["hold_music="
                   ,kz_term:to_list(media_path(Media, 'extant', UUID, kz_json:new()))
                   ]);
get_fs_kv(?CCV(Key), Val, UUID) ->
    get_fs_kv(Key, Val, UUID);
get_fs_kv(Key, Val, _) ->
    list_to_binary([get_fs_key(Key), "=", maybe_sanitize_fs_value(Key, Val)]).

-spec get_fs_key(kz_term:ne_binary()) -> binary().
get_fs_key(?CCV(Key)) -> get_fs_key(Key);
get_fs_key(?CAV(_)=CAV) -> CAV;
get_fs_key(<<"X-", _/binary>>=Key) -> <<"sip_h_", Key/binary>>;
get_fs_key(Key) ->
    case lists:keyfind(Key, 1, ?SPECIAL_CHANNEL_VARS) of
        'false' -> ?CCV(Key);
        {_, Prefix} -> Prefix
    end.

-spec get_fs_key_and_value(kz_term:ne_binary()
                          ,kz_term:ne_binary() | kz_term:ne_binaries() | kz_json:object()
                          ,kz_term:ne_binary()
                          ) ->
                                  {kz_term:ne_binary(), binary()} |
                                  [{kz_term:ne_binary(), binary()}] |
                                  'skip'.
get_fs_key_and_value(<<"Hold-Media">>=Key, Media, UUID) ->
    {get_fs_key(Key), media_path(Media, 'extant', UUID, kz_json:new())};
get_fs_key_and_value(<<"Diversions">>=Key, Diversions, _UUID) ->
    K = get_fs_key(Key),
    lager:debug("setting diversions ~p on the channel", [Diversions]),
    [{K, D} || D <- Diversions];
get_fs_key_and_value(<<"Auto-Answer">> = Key, Value, _UUID) ->
    [{get_fs_key(<<"Alert-Info">>), <<"intercom">>}
    ,{get_fs_key(Key), maybe_sanitize_fs_value(Key, Value)}
    ];
get_fs_key_and_value(<<"Auto-Answer-Suppress-Notify">> = Key, Value, _UUID) ->
    [{get_fs_key(<<"Alert-Info">>), <<"intercom">>}
    ,{get_fs_key(<<"Auto-Answer">>), maybe_sanitize_fs_value(<<"Auto-Answer">>, <<"true">>)}
    ,{get_fs_key(Key), maybe_sanitize_fs_value(Key, Value)}
    ];
get_fs_key_and_value(<<"ringback">>=Key, Value, _UUID) ->
    [{<<"ringback">>, maybe_sanitize_fs_value(Key, Value)}
    ,{<<"transfer_ringback">>, maybe_sanitize_fs_value(<<"transfer_ringback">>, Value)}
    ];
get_fs_key_and_value(?CCV(Key), Val, UUID) ->
    get_fs_key_and_value(Key, Val, UUID);
get_fs_key_and_value(Key, Val, _UUID)
  when is_binary(Val);
       is_atom(Val);
       is_number(Val);
       is_list(Val);
       is_boolean(Val) ->
    {get_fs_key(Key), maybe_sanitize_fs_value(Key, Val)};
get_fs_key_and_value(_, _, _) -> 'skip'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_sanitize_fs_value(kz_term:text(), kz_term:text()) -> binary().
maybe_sanitize_fs_value(Key, Val) when Key =:= <<"Outbound-Caller-ID-Name">>
                                       orelse Key =:= <<"Outbound-Callee-ID-Name">>
                                       orelse Key =:= <<"Caller-ID-Name">>
                                       orelse Key =:= <<"Callee-ID-Name">> ->
    re:replace(Val, ?SANITIZE_FS_VALUE_REGEX, <<>>, ['ucp', 'global', 'unicode', {'return', 'binary'}]);
maybe_sanitize_fs_value(<<"Export-Bridge-Variables">>, Val) ->
    kz_binary:join(Val, <<",">>);
maybe_sanitize_fs_value(<<"Export-Variables">>, Val) ->
    kz_binary:join(Val, <<",">>);
maybe_sanitize_fs_value(Key, Val) when not is_binary(Key) ->
    maybe_sanitize_fs_value(kz_term:to_binary(Key), Val);
maybe_sanitize_fs_value(Key, Val) when not is_binary(Val) ->
    maybe_sanitize_fs_value(Key, kz_term:to_binary(Val));
maybe_sanitize_fs_value(_, Val) -> Val.

%%------------------------------------------------------------------------------
%% @doc takes endpoints (/sofia/foo/bar), and optionally a caller id name/num
%% and create the dial string ([origination_caller_id_name=Name
%%                              ,origination_caller_id_number=Num]Endpoint)
%% joined by the optional separator.  Saves time by not spawning
%% endpoints with the invite format of "route" (about 100ms per endpoint)
%% @end
%%------------------------------------------------------------------------------
-type bridge_channel() :: kz_term:ne_binary().
-type bridge_channels() :: kz_term:ne_binaries().
-type build_return() :: bridge_channel() | {'worker', pid()}.
-type build_returns() :: [build_return()].
-type bridge_endpoints() :: [bridge_endpoint()].

-spec build_bridge_string(kz_json:objects()) -> kz_term:ne_binary().
build_bridge_string(Endpoints) ->
    build_bridge_string(Endpoints, ?SEPARATOR_SINGLE).

%%------------------------------------------------------------------------------
%% @doc De-dupe the bridge strings by matching those with the same
%% Invite-Format, To-IP, To-User, To-realm, To-DID, and Route.
%%
%% NOTE: don't use binary_join here as it will crash on an empty list...
%% @end
%%------------------------------------------------------------------------------
-spec build_bridge_string(kz_json:objects(), kz_term:ne_binary()) -> kz_term:ne_binary().
build_bridge_string(Endpoints, Separator) ->
    BridgeChannels = build_bridge_channels(Endpoints),
    kz_binary:join(lists:reverse(BridgeChannels), Separator).

-spec failover_if_all_unregistered(bridge_endpoints()) -> bridge_endpoints().
failover_if_all_unregistered(Endpoints) ->
    case classify_endpoints(Endpoints) of
        {[], Failover} ->
            lager:info("no device endpoints available, using failover routes."),
            Failover;
        {Devices, _} ->
            lager:info("device endpoints registered, ignoring failover routes."),
            Devices
    end.

-spec classify_endpoints(bridge_endpoints()) -> {bridge_endpoints(), bridge_endpoints()}.
classify_endpoints(Endpoints) ->
    lists:foldl(fun classify_endpoint/2, {[], []}, Endpoints).

-spec classify_endpoint(bridge_endpoint(), {bridge_endpoints(), bridge_endpoints()}) -> {bridge_endpoints(), bridge_endpoints()}.
classify_endpoint(#bridge_endpoint{channel_vars=CVs
                                  ,failover=EndpointFailover
                                  }=Endpoint
                 ,{Devices, Failovers}
                 ) ->
    IsFailoverEndpoint = lists:member(<<?CHANNEL_VAR_PREFIX, "Is-Failover='true'">>, CVs),
    IsRegistered = is_registered(Endpoint),

    case IsFailoverEndpoint of
        'true' ->
            lager:info("endpoint is a failover endpoint"),
            {Devices, maybe_use_fwd_endpoint(Endpoint, Failovers)};
        'false' when IsRegistered ->
            lager:info("endpoint is an endpoint"),
            {[Endpoint | Devices], Failovers};
        'false' when EndpointFailover =:= 'undefined' ->
            lager:info("endpoint is not failover nor registered, removing"),
            {Devices, Failovers};
        'false' ->
            lager:info("endpoint is not registered but has failover route defined"),
            maybe_add_failover_route(EndpointFailover, {Devices, Failovers})
    end.

maybe_add_failover_route(EndpointFailover, {Devices, Failovers}) ->
    Endpoint = endpoint_jobj_to_record(EndpointFailover),
    {Devices, maybe_use_fwd_endpoint(Endpoint, Failovers)}.

-spec is_registered(bridge_endpoint()) -> boolean().
is_registered(Endpoint) ->
    try get_sip_contact(Endpoint) of
        _ -> 'true'
    catch
        'error':'badarg' -> 'false';
        'error':{'badmatch',{'error', 'not_found'}} -> 'false'
    end.

-spec maybe_use_fwd_endpoint(bridge_endpoint(), bridge_endpoints()) -> bridge_endpoints().
maybe_use_fwd_endpoint(#bridge_endpoint{invite_format = <<"loopback">>
                                       ,route=ForwardDestination
                                       }=Endpoint
                      ,Failovers
                      ) ->
    case lists:keyfind(ForwardDestination, #bridge_endpoint.route, Failovers) of
        'false' ->
            lager:info("adding failover endpoint (to ~s)", [ForwardDestination]),
            [Endpoint| Failovers];
        _EP ->
            lager:info("skipping endpoint with existing destination ~s", [ForwardDestination]),
            Failovers
    end.

-spec endpoint_jobjs_to_records(kz_json:objects()) -> bridge_endpoints().
endpoint_jobjs_to_records(Endpoints) ->
    endpoint_jobjs_to_records(Endpoints, 'true').

-spec endpoint_jobjs_to_records(kz_json:objects(), boolean()) -> bridge_endpoints().
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

-spec endpoint_key(kz_json:object()) -> kz_term:api_binaries().
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
    Bridge = #bridge_endpoint{invite_format = kz_json:get_ne_value(<<"Invite-Format">>, Endpoint, <<"username">>)
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
                             ,include_channel_vars = IncludeVars
                             ,failover = kz_json:get_json_value(<<"Failover">>, Endpoint)
                             },
    endpoint_jobj_to_record_vars(Endpoint, Bridge).

-spec endpoint_jobj_to_record_vars(kz_json:object(), bridge_endpoint()) -> bridge_endpoint().
endpoint_jobj_to_record_vars(Endpoint, #bridge_endpoint{include_channel_vars='true'}=Bridge) ->
    Bridge#bridge_endpoint{channel_vars=ecallmgr_fs_xml:build_leg_vars(Endpoint)};
endpoint_jobj_to_record_vars(Endpoint, #bridge_endpoint{include_channel_vars='false'}=Bridge) ->
    Props = lists:filter(fun({<<"Custom-SIP-Headers">>, _}) -> 'true';
                            (_) -> 'false'
                         end
                        ,kz_json:to_proplist(Endpoint)
                        ),
    Bridge#bridge_endpoint{header_vars=ecallmgr_fs_xml:build_leg_vars(Props)}.

-spec get_endpoint_span(kz_json:object()) -> kz_term:ne_binary().
get_endpoint_span(Endpoint) ->
    kz_json:get_binary_value([<<"Endpoint-Options">>, <<"Span">>], Endpoint, <<"1">>).

-spec get_endpoint_channel_selection(kz_json:object()) -> kz_term:ne_binary().
get_endpoint_channel_selection(Endpoint) ->
    case kz_json:get_binary_value([<<"Endpoint-Options">>, <<"Span">>], Endpoint) of
        <<"descending">> -> <<"A">>;
        _Else -> <<"a">>
    end.

-spec get_endpoint_interface(kz_json:object()) -> kz_term:ne_binary().
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
    FilteredEPs = maybe_filter_failover_channels(EPs, ?FAILOVER_IF_ALL_UNREGED),
    build_bridge_channels(FilteredEPs, []).

%% HELP-34627: Only handle failover when all endpoints are unregistered.
%% Additionally split bridge strings out and only return failover endpoints
%% if no devices registered.
-spec maybe_filter_failover_channels(bridge_endpoints(), boolean()) -> bridge_endpoints().
maybe_filter_failover_channels(Channels, 'false') -> Channels;
maybe_filter_failover_channels(Channels, 'true') ->
    lager:info("ecallmgr 'failover_when_all_unreg' is enabled."),

    %% NOTE: because we want to de-dup failover endpoints with the same destination
    %% it is important to pass bridge strings in intial order to {@link failover_if_all_unregistered/1}
    %% function so only the first duplicated endpoint will ne used.
    failover_if_all_unregistered(lists:reverse(Channels)).

-spec maybe_apply_call_waiting(kz_json:objects()) -> kz_json:objects().
maybe_apply_call_waiting(Endpoints) ->
    [call_waiting_map(E) || E <- Endpoints].

-spec call_waiting_map(kz_json:object()) -> kz_json:object().
call_waiting_map(Endpoint) ->
    CCVs = kz_json:get_json_value(<<"Custom-Channel-Vars">>, Endpoint, kz_json:new()),
    case kz_json:is_true(<<"Call-Waiting-Disabled">>, CCVs) of
        'false' -> Endpoint;
        'true' ->
            OwnerId = kz_json:get_value(<<"Owner-ID">>, CCVs),
            maybe_add_respond_header(Endpoint, OwnerId)
    end.

-spec maybe_add_respond_header(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
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
%% If this does not have an explicit sip route and we have no ip address, lookup the registration
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
                           {'error', 'invalid' | 'number_not_provided'}.
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
                               {'error', 'invalid'}.
build_sip_channel(#bridge_endpoint{failover=Failover}=Endpoint) ->
    Routines = [fun get_sip_contact/1
               ,fun maybe_clean_contact/2
               ,fun ensure_username_present/2
               ,fun maybe_replace_fs_path/2
               ,fun maybe_replace_transport/2
               ,fun maybe_format_user/2
               ,fun maybe_set_interface/2
               ,fun maybe_append_channel_vars/2
               ],
    try lists:foldl(fun build_sip_channel_fold/2, Endpoint, Routines) of
        {Channel, _} -> {'ok', Channel}
    catch
        _E:{'badmatch', {'error', 'not_found'}} ->
            lager:warning("failed to build sip channel trying failover"),
            maybe_failover(Failover);
        _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:warning("failed to build sip channel (~s): ~p", [_E, _R]),
            kz_util:log_stacktrace(ST),
            {'error', 'invalid'}
    end.

-type build_channel_fun_1() :: fun((bridge_endpoint()) -> bridge_channel() | {bridge_channel(), bridge_endpoint()}).
-type build_channel_fun_2() :: fun((bridge_channel(), bridge_endpoint()) -> bridge_channel() | {bridge_channel(), bridge_endpoint()}).

-type build_channel_fun() :: build_channel_fun_1() | build_channel_fun_2().
-type build_channel_acc() :: bridge_channel() | {bridge_channel(), bridge_endpoint()}.

-spec build_sip_channel_fold(build_channel_fun(), build_channel_acc()) -> build_channel_acc().
build_sip_channel_fold(Fun, {Contact, Endpoint}) ->
    case Fun(Contact, Endpoint) of
        {NewContact, NewEndpoint} -> {NewContact, NewEndpoint};
        NewContact -> {NewContact, Endpoint}
    end;
build_sip_channel_fold(Fun, Endpoint) ->
    case Fun(Endpoint) of
        {NewContact, NewEndpoint} -> {NewContact, NewEndpoint};
        NewContact -> {NewContact, Endpoint}
    end.

-spec maybe_failover(kz_json:object()) ->
                            {'ok', bridge_channel()} |
                            {'error', 'invalid'}.
maybe_failover(Endpoint) ->
    case kz_term:is_empty(Endpoint) of
        'true' -> {'error', 'invalid'};
        'false' -> build_sip_channel(endpoint_jobj_to_record(Endpoint))
    end.

-ifdef(TEST).
-spec get_sip_contact(bridge_endpoint()) -> kz_term:ne_binary().
get_sip_contact(#bridge_endpoint{invite_format = <<"route">>, route = <<"loopback/", Route/binary>>}) ->
    <<"loopback/", Route/binary, "/", (?DEFAULT_FREESWITCH_CONTEXT)/binary>>;
get_sip_contact(#bridge_endpoint{invite_format = <<"route">>, route=Route}) ->
    Route;
get_sip_contact(#bridge_endpoint{invite_format = <<"loopback">>, route=Route}) ->
    <<"loopback/", Route/binary, "/", (?DEFAULT_FREESWITCH_CONTEXT)/binary>>;
get_sip_contact(#bridge_endpoint{ip_address='undefined'
                                ,realm=_Realm
                                ,username = <<"unregistered">>
                                }) ->
    %% Simulate a badmatch when registration is not found
    {'ok', _Contact, _Props} = not_found();
get_sip_contact(#bridge_endpoint{ip_address='undefined'
                                ,realm=_Realm
                                ,username = 'undefined'
                                }) ->
    error('badarg');
get_sip_contact(#bridge_endpoint{ip_address='undefined'
                                ,realm=Realm
                                ,username=Username
                                }) ->
    <<Username/binary, "@", Realm/binary>>;
get_sip_contact(#bridge_endpoint{ip_address=IPAddress}) -> IPAddress.

not_found() ->
    {'error', 'not_found'}.

-else.
-spec get_sip_contact(bridge_endpoint()) -> kz_term:ne_binary().
get_sip_contact(#bridge_endpoint{invite_format = <<"route">>, route = <<"loopback/", Route/binary>>}) ->
    <<"loopback/", Route/binary, "/", (?DEFAULT_FREESWITCH_CONTEXT)/binary>>;
get_sip_contact(#bridge_endpoint{invite_format = <<"route">>, route=Route}) ->
    Route;
get_sip_contact(#bridge_endpoint{invite_format = <<"loopback">>, route=Route}) ->
    <<"loopback/", Route/binary, "/", (?DEFAULT_FREESWITCH_CONTEXT)/binary>>;
get_sip_contact(#bridge_endpoint{ip_address='undefined'
                                ,realm=Realm
                                ,username=Username
                                ,channel_vars=CVs
                                }=EP) ->
    {'ok', Contact, Props} = ecallmgr_registrar:lookup_contact(Realm, Username),
    Vars = ecallmgr_fs_xml:build_leg_vars(Props),
    NewEP = EP#bridge_endpoint{channel_vars=Vars ++ CVs},
    {binary:replace(Contact, <<">">>, <<>>), NewEP};
get_sip_contact(#bridge_endpoint{ip_address=IPAddress}) -> IPAddress.
-endif.

-spec maybe_clean_contact(kz_term:ne_binary(), bridge_endpoint()) -> kz_term:ne_binary().
maybe_clean_contact(<<"sip:", Contact/binary>>, _Endpoint) -> Contact;
maybe_clean_contact(<<"sips:", Contact/binary>>, _Endpoint) -> Contact;
maybe_clean_contact(Contact, #bridge_endpoint{invite_format = <<"route">>}) ->
    Contact;
maybe_clean_contact(Contact, #bridge_endpoint{invite_format = <<"loopback">>}) ->
    Contact;
maybe_clean_contact(Contact, _) ->
    re:replace(Contact, <<"^.*?[^=]sip:">>, <<>>, [{'return', 'binary'}]).

-spec ensure_username_present(kz_term:ne_binary(), bridge_endpoint()) -> kz_term:ne_binary().
ensure_username_present(Contact, #bridge_endpoint{invite_format = <<"route">>}) ->
    Contact;
ensure_username_present(Contact, #bridge_endpoint{invite_format = <<"loopback">>}) ->
    Contact;
ensure_username_present(Contact, Endpoint) ->
    case binary:split(Contact, <<"@">>) of
        [_, _] -> Contact;
        _ -> <<(guess_username(Endpoint))/binary, "@", Contact/binary>>
    end.

-spec guess_username(bridge_endpoint()) -> kz_term:ne_binary().
guess_username(#bridge_endpoint{number=Number}) when is_binary(Number) -> Number;
guess_username(#bridge_endpoint{username=Username}) when is_binary(Username) -> Username;
guess_username(#bridge_endpoint{user=User}) when is_binary(User) -> User;
guess_username(_) -> <<"kazoo">>.

-spec maybe_replace_fs_path(kz_term:ne_binary(), bridge_endpoint()) -> kz_term:ne_binary().
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

-spec maybe_replace_transport(kz_term:ne_binary(), bridge_endpoint()) -> kz_term:ne_binary().
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

-spec maybe_format_user(kz_term:ne_binary(), bridge_endpoint()) -> kz_term:ne_binary().
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

-spec maybe_set_interface(kz_term:ne_binary(), bridge_endpoint()) -> kz_term:ne_binary().
maybe_set_interface(<<"sofia/", _/binary>>=Contact, _) -> Contact;
maybe_set_interface(<<"loopback/", _/binary>>=Contact, _) -> Contact;
maybe_set_interface(Contact, #bridge_endpoint{sip_interface='undefined'}=Endpoint) ->
    Options = ['ungreedy', {'capture', 'all_but_first', 'binary'}],
    case re:run(Contact, <<";fs_path=sip:(.*):\\d*;">>, Options) of
        {'match', FsPath} ->
            SIPInterface = kz_term:to_binary(get_sip_interface_from_db(FsPath)),
            maybe_set_interface(Contact, Endpoint#bridge_endpoint{sip_interface=SIPInterface});
        'nomatch' ->
            <<"sofia/", ?SIP_INTERFACE, "/", Contact/binary>>
    end;
maybe_set_interface(Contact, #bridge_endpoint{sip_interface= <<"sofia/", _/binary>>=SIPInterface}) ->
    <<(kz_binary:strip_right(SIPInterface, $/))/binary, "/", Contact/binary>>;
maybe_set_interface(Contact, #bridge_endpoint{sip_interface=SIPInterface}) ->
    <<"sofia/", SIPInterface/binary, "/", Contact/binary>>.

-spec maybe_append_channel_vars(kz_term:ne_binary(), bridge_endpoint()) -> kz_term:ne_binary().
maybe_append_channel_vars(Contact, #bridge_endpoint{include_channel_vars='false'
                                                   ,header_vars=[]
                                                   }) ->
    'false' = kz_term:is_empty(Contact),
    Contact;
maybe_append_channel_vars(Contact, #bridge_endpoint{include_channel_vars='false'
                                                   ,header_vars=HeaderVars
                                                   }) ->
    'false' = kz_term:is_empty(Contact),
    list_to_binary([ecallmgr_fs_xml:get_leg_vars(HeaderVars), Contact]);
maybe_append_channel_vars(Contact, #bridge_endpoint{channel_vars=[]
                                                   ,header_vars=[]
                                                   }) ->
    'false' = kz_term:is_empty(Contact),
    Contact;
maybe_append_channel_vars(Contact, #bridge_endpoint{channel_vars=ChannelVars
                                                   ,header_vars=HeaderVars
                                                   }) ->
    'false' = kz_term:is_empty(Contact),
    list_to_binary([ecallmgr_fs_xml:get_leg_vars(ChannelVars++HeaderVars), Contact]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec create_masquerade_event(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
create_masquerade_event(Application, EventName) ->
    create_masquerade_event(Application, EventName, 'true').

-spec create_masquerade_event(kz_term:ne_binary(), kz_term:ne_binary(), boolean()) -> kz_term:ne_binary().
create_masquerade_event(Application, EventName, Boolean) ->
    Prefix = case Boolean of
                 'true' -> <<"event ">>;
                 'false' -> <<>>
             end,
    list_to_binary([Prefix, "Event-Name=CUSTOM,Event-Subclass=kazoo::masquerade"
                   ,",kazoo_event_name=", EventName
                   ,",kazoo_application_name=", Application
                   ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec media_path(kz_term:ne_binary()) -> kz_term:ne_binary().
media_path(MediaName) -> media_path(MediaName, 'new', kz_binary:rand_hex(16), kz_json:new()).

-spec media_path(kz_term:ne_binary(), kz_json:object()) -> kz_term:ne_binary().
media_path(MediaName, JObj) -> media_path(MediaName, 'new', kz_binary:rand_hex(16), JObj).

-spec media_path(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> kz_term:ne_binary().
media_path(MediaName, UUID, JObj) -> media_path(MediaName, 'new', UUID, JObj).

-spec media_path(kz_term:api_binary(), media_types(), kz_term:ne_binary(), kz_json:object()) -> kz_term:ne_binary().
media_path('undefined', _Type, _UUID, _) -> <<"silence_stream://5">>;
media_path(<<>>, _Type, _UUID, _) -> <<"silence_stream://5">>;
media_path(MediaName, Type, UUID, JObj) when not is_binary(MediaName) ->
    media_path(kz_term:to_binary(MediaName), Type, UUID, JObj);
media_path(<<"say:", _/binary>> = Say, _Type, _UUID, _) -> Say;
media_path(<<"silence">> = Media, _Type, _UUID, _) -> Media;
media_path(<<"silence_stream://", _/binary>> = Media, _Type, _UUID, _) -> Media;
media_path(<<"tone_stream://", _/binary>> = Media, _Type, _UUID, _) -> Media;
media_path(<<"shout://", _/binary>> = Media, _Type, _UUID, _) -> Media;
media_path(<<"local_stream://", FSPath/binary>>, _Type, _UUID, _) -> recording_filename(FSPath);
media_path(<<?LOCAL_MEDIA_PATH, _/binary>> = FSPath, _Type, _UUID, _) -> FSPath;
media_path(<<"http://", _/binary>> = URI, _Type, _UUID, _) -> get_fs_playback(URI);
media_path(<<"https://", _/binary>> = URI, _Type, _UUID, _) -> get_fs_playback(URI);
media_path(<<?HTTP_GET_PREFIX, _/binary>> = Media, _Type, _UUID, _) -> Media;
media_path(<<"\$", _/binary>> = Media, _Type, _UUID, _) -> Media;
media_path(MediaName, Type, UUID, JObj) ->
    case lookup_media(MediaName, Type, UUID, JObj) of
        {'error', _E} ->
            lager:warning("failed to get media path for ~s: ~p", [MediaName, _E]),
            kz_term:to_binary(MediaName);
        {'ok', Path} ->
            lager:debug("found path ~s for ~s", [Path, MediaName]),
            kz_term:to_binary(get_fs_playback(Path))
    end.

-spec fax_filename(kz_term:ne_binary()) -> file:filename_all().
fax_filename(UUID) ->
    Ext = kapps_config:get_ne_binary(?APP_NAME, <<"default_fax_extension">>, <<".tiff">>),
    filename:join([kapps_config:get_ne_binary(?APP_NAME, <<"fax_file_path">>, <<"/tmp/">>)
                  ,<<(kz_amqp_util:encode(UUID))/binary, Ext/binary>>
                  ]).

-spec recording_filename(kz_term:ne_binary()) -> file:filename_all().
recording_filename(<<"local_stream://", MediaName/binary>>) -> recording_filename(MediaName);
recording_filename(MediaName) ->
    Ext = recording_extension(MediaName),
    RootName = filename:basename(MediaName, Ext),
    Directory = recording_directory(MediaName),
    RecordingName = filename:join([Directory
                                  ,<<(kz_amqp_util:encode(RootName))/binary, Ext/binary>>
                                  ]),
    _ = kz_cache:store_local(?ECALLMGR_UTIL_CACHE
                            ,?ECALLMGR_PLAYBACK_MEDIA_KEY(MediaName)
                            ,RecordingName
                            ),
    RecordingName.

-spec recording_directory(kz_term:ne_binary()) -> kz_term:ne_binary().
recording_directory(<<"/", _/binary>> = FullPath) -> filename:dirname(FullPath);
recording_directory(_RelativePath) -> kapps_config:get_ne_binary(?APP_NAME, <<"recording_file_path">>, <<"/tmp/">>).

-spec recording_extension(kz_term:ne_binary()) -> kz_term:ne_binary().
recording_extension(MediaName) ->
    case filename:extension(MediaName) of
        Empty when Empty =:= <<>>;
                   Empty =:= [] ->
            kapps_config:get_ne_binary(?APP_NAME, <<"default_recording_extension">>, <<".mp3">>);
        <<".mp3">> = MP3 -> MP3;
        <<".mp4">> = MP4 -> MP4;
        <<".wav">> = WAV -> WAV;
        _ ->
            kapps_config:get_ne_binary(?APP_NAME, <<"default_recording_extension">>, <<".mp3">>)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_fs_playback(kz_term:ne_binary()) -> kz_term:ne_binary().
get_fs_playback(<<?LOCAL_MEDIA_PATH, _/binary>> = URI) -> URI;
get_fs_playback(URI) -> maybe_playback_via_vlc(URI).

-spec maybe_playback_via_vlc(kz_term:ne_binary()) -> kz_term:ne_binary().
maybe_playback_via_vlc(URI) ->
    case kapps_config:is_true(?APP_NAME, <<"use_vlc">>, 'false') of
        'false' -> maybe_playback_via_shout(URI);
        'true' ->
            lager:debug("media is streamed via VLC, prepending ~s", [URI]),
            <<"vlc://", URI/binary>>
    end.

-spec maybe_playback_via_shout(kz_term:ne_binary()) -> kz_term:ne_binary().
maybe_playback_via_shout(URI) ->
    case filename:extension(URI) =:= <<".mp3">>
        andalso kapps_config:is_true(?APP_NAME, <<"use_shout">>, 'false')
    of
        'false' -> maybe_playback_via_http_cache(URI);
        'true' ->
            lager:debug("media is streamed via shout, updating ~s", [URI]),
            binary:replace(URI, [<<"http">>, <<"https">>], <<"shout">>)
    end.

-spec maybe_playback_via_http_cache(kz_term:ne_binary()) -> kz_term:ne_binary().
maybe_playback_via_http_cache(<<?HTTP_GET_PREFIX, _/binary>> = URI) ->
    lager:debug("media is streamed via http_cache, using ~s", [URI]),
    URI;
maybe_playback_via_http_cache(URI) ->
    case kapps_config:is_true(?APP_NAME, <<"use_http_cache">>, 'true') of
        'false' ->
            lager:debug("using straight URI ~s", [URI]),
            URI;
        'true' ->
            lager:debug("media is streamed via http_cache, using ~s", [URI]),
            <<"http_cache://", URI/binary>>
    end.

%% given a proplist of a FS event, return the Kazoo-equivalent app name(s).
%% a FS event could have multiple Kazoo equivalents
-spec convert_fs_evt_name(kz_term:ne_binary()) -> kz_term:ne_binaries().
convert_fs_evt_name(EvtName) ->
    [WhAppEvt || {FSEvt, WhAppEvt} <- ?FS_APPLICATION_NAMES, FSEvt =:= EvtName].

%% given a Kazoo Dialplan Application name, return the FS-equivalent event name
%% A Kazoo Dialplan Application name is 1-to-1 with the FS-equivalent
-spec convert_kazoo_app_name(kz_term:ne_binary()) -> kz_term:ne_binaries().
convert_kazoo_app_name(App) ->
    [EvtName || {EvtName, AppName} <- ?FS_APPLICATION_NAMES, App =:= AppName].

-type media_types() :: 'new' | 'extant'.
-spec lookup_media(kz_term:ne_binary(), media_types(), kz_term:ne_binary(), kz_json:object()) ->
                          {'ok', kz_term:ne_binary()} |
                          {'error', any()}.
lookup_media(MediaName, Type, CallId, JObj) ->
    case kz_cache:fetch_local(?ECALLMGR_UTIL_CACHE
                             ,?ECALLMGR_PLAYBACK_MEDIA_KEY(MediaName)
                             )
    of
        {'ok', _Path}=Ok ->
            lager:debug("media ~s exists in playback cache as ~s", [MediaName, _Path]),
            Ok;
        {'error', 'not_found'} ->
            request_media_url(MediaName, Type, CallId, JObj)
    end.
-spec remove_media(kz_term:ne_binary()) -> 'ok'.
remove_media(MediaName) ->
    kz_cache:erase_local(?ECALLMGR_UTIL_CACHE
                        ,?ECALLMGR_PLAYBACK_MEDIA_KEY(MediaName)
                        ).

-spec request_media_url(kz_term:ne_binary(), media_types(), kz_term:ne_binary(), kz_json:object()) ->
                               {'ok', kz_term:ne_binary()} |
                               {'error', any()}.
request_media_url(MediaName, Type, CallId, JObj) ->
    MsgProps = props:filter_undefined(
                 [{<<"Media-Name">>, MediaName}
                 ,{<<"Stream-Type">>, kz_term:to_binary(Type)}
                 ,{<<"Call-ID">>, CallId}
                 ,{<<"Msg-ID">>, kz_binary:rand_hex(8)}
                  | kz_api:default_headers(<<"media">>, <<"media_req">>, ?APP_NAME, ?APP_VERSION)
                 ]),
    case kz_amqp_worker:call_collect(kz_json:set_values(MsgProps, JObj)
                                    ,fun kapi_media:publish_req/1
                                    ,{'media_mgr', fun kapi_media:resp_v/1}
                                    )
    of
        {'ok', MediaResp} ->
            maybe_cache_media_response(MediaName, MediaResp);
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

-spec maybe_cache_media_response(kz_term:ne_binary(), kz_json:objects()) ->
                                        {'ok', kz_term:ne_binary()} |
                                        {'error', 'not_found'}.
maybe_cache_media_response(MediaName, MediaResp) ->
    case kz_json:find(<<"Stream-URL">>, MediaResp, <<>>) of
        <<>> ->
            lager:info("no stream URL found for media ~s", [MediaName]),
            {'error', 'not_found'};
        MediaUrl ->
            CacheProps = media_url_cache_props(MediaName),
            _ = kz_cache:store_local(?ECALLMGR_UTIL_CACHE
                                    ,?ECALLMGR_PLAYBACK_MEDIA_KEY(MediaName)
                                    ,MediaUrl
                                    ,CacheProps
                                    ),
            lager:debug("media ~s stored to playback cache : ~s", [MediaName, MediaUrl]),
            {'ok', MediaUrl}
    end.

-spec media_url_cache_props(kz_term:ne_binary()) -> kz_cache:store_options().
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
    Id = kz_binary:md5(Text),
    [{'origin', {'db', <<"tts">>, Id}}];
media_url_cache_props(_MediaName) -> [].

-spec custom_sip_headers(kz_term:proplist()) -> kz_term:proplist().
custom_sip_headers(Props) ->
    lists:foldl(fun maybe_aggregate_headers/2
               ,[]
               ,props:filter(fun is_custom_sip_header/1, Props)
               ).

-spec maybe_aggregate_headers({kz_term:ne_binary(), kz_term:ne_binary()}, kz_term:proplist()) ->
                                     kz_term:proplist().
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
is_custom_sip_header({?GET_CCV_HEADER(_), _}) -> 'false';
is_custom_sip_header({<<"variable_sip_h_", _/binary>>, _}) -> 'true';
is_custom_sip_header(_Header) -> 'false'.

-spec maybe_add_expires_deviation(kz_term:api_integer()) -> kz_term:api_integer().
maybe_add_expires_deviation('undefined') -> 'undefined';
maybe_add_expires_deviation(Expires) when not is_integer(Expires) ->
    maybe_add_expires_deviation(kz_term:to_integer(Expires));
maybe_add_expires_deviation(0) -> 0;
maybe_add_expires_deviation(Expires) ->
    Expires + ?EXPIRES_DEVIATION_TIME.

-spec maybe_add_expires_deviation_ms(kz_term:api_integer()) -> kz_term:api_integer().
maybe_add_expires_deviation_ms('undefined') -> 'undefined';
maybe_add_expires_deviation_ms(Expires) when not is_integer(Expires) ->
    maybe_add_expires_deviation_ms(kz_term:to_integer(Expires));
maybe_add_expires_deviation_ms(Expires) ->
    maybe_add_expires_deviation(Expires) * ?MILLISECONDS_IN_SECOND.

-spec get_dial_separator(kz_term:api_object() | kz_term:ne_binary(), kz_json:objects()) -> kz_term:ne_binary().
get_dial_separator(?DIAL_METHOD_SINGLE, _Endpoints) -> ?SEPARATOR_SINGLE;
get_dial_separator(?DIAL_METHOD_SIMUL, [_, _|_]) -> ?SEPARATOR_SIMULTANEOUS;
get_dial_separator(?DIAL_METHOD_SIMUL, [_]) -> ?SEPARATOR_SINGLE;
get_dial_separator('undefined', _Endpoints) -> ?SEPARATOR_SINGLE;
get_dial_separator(JObj, Endpoints) ->
    get_dial_separator(kz_json:get_value(<<"Dial-Endpoint-Method">>, JObj, ?DIAL_METHOD_SINGLE)
                      ,Endpoints
                      ).

-spec fix_contact(kz_term:api_binary() | list(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_binary().
fix_contact('undefined', _, _) -> 'undefined';
fix_contact(<<";", _/binary>> = OriginalContact, Username, Realm) ->
    fix_contact(<<"sip:", Username/binary, "@", Realm/binary, OriginalContact/binary>>, Username, Realm);
fix_contact(OriginalContact, Username, Realm)
  when is_binary(OriginalContact) ->
    fix_contact(binary:split(kz_binary:strip(OriginalContact), <<";">>, ['global']), Username, Realm);
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
