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
-export([get_sip_to/1, get_sip_from/1, get_sip_request/1, get_orig_ip/1]).
-export([custom_channel_vars/1, custom_sip_headers/1]).
-export([eventstr_to_proplist/1, varstr_to_proplist/1, get_setting/1, get_setting/2]).
-export([is_node_up/1, is_node_up/2]).
-export([fs_log/3, put_callid/1]).
-export([build_bridge_string/1, build_bridge_string/2]).
-export([create_masquerade_event/2, create_masquerade_event/3]).
-export([media_path/3, media_path/4, media_path/5]).
-export([unserialize_fs_array/1]).
-export([convert_fs_evt_name/1, convert_whistle_app_name/1]).
-export([fax_filename/1
         ,recording_filename/1
        ]).

-export([lookup_media/3, lookup_media/4, lookup_media/5]).

-include_lib("ecallmgr/src/ecallmgr.hrl").

-type send_cmd_ret() :: fs_sendmsg_ret() | fs_api_ret().
-export_type([send_cmd_ret/0]).

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
                   _ = ecallmgr_util:fs_log(Node, "whistle queuing command in 'xferext' extension: ~s", [V]),
                   lager:debug("building xferext on node ~s: ~s", [Node, V]),
                   {wh_util:to_list(K), wh_util:to_list(V)}
               end || {K, V} <- Dialplan],
    ok = freeswitch:sendmsg(Node, UUID, [{"call-command", "xferext"} | XferExt]),
    ecallmgr_util:fs_log(Node, "whistle transfered call to 'xferext' extension", []);
send_cmd(Node, UUID, App, Args) when not is_list(Args) ->
    send_cmd(Node, UUID, App, wh_util:to_list(Args));
send_cmd(Node, UUID, "record_call", Args) ->
    lager:debug("execute on node ~s: uuid_record(~s)", [Node, Args]),
    _ = ecallmgr_util:fs_log(Node, "whistle executing uuid_record ~s", [Args]),
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
    _ = ecallmgr_util:fs_log(Node, "whistle executing uuid_break ~s all", [UUID]),
    freeswitch:api(Node, uuid_break, wh_util:to_list(<<UUID/binary, " all">>));
send_cmd(Node, UUID, "unbridge", _) ->
    lager:debug("execute on node ~s: uuid_park(~s)", [Node, UUID]),
    _ = ecallmgr_util:fs_log(Node, "whistle executing uuid_park ~s", [UUID]),
    freeswitch:api(Node, uuid_park, wh_util:to_list(UUID));
send_cmd(Node, _UUID, "broadcast", Args) ->
    lager:debug("execute on node ~s: uuid_broadcast(~s)", [Node, Args]),
    _ = ecallmgr_util:fs_log(Node, "whistle executing uuid_broadcast ~s", [Args]),
    Resp = freeswitch:api(Node, uuid_broadcast, wh_util:to_list(iolist_to_binary(Args))),
    lager:debug("broadcast resulted in: ~p", [Resp]),
    Resp;
send_cmd(Node, UUID, "call_pickup", Args) ->
    lager:debug("execute on node ~s: uuid_bridge(~s)", [Node, Args]),
    _ = ecallmgr_util:fs_log(Node, "whistle executing call_pickup (uuid_bridge ~s)", [Args]),
    freeswitch:api(Node, uuid_bridge, wh_util:to_list(Args));
send_cmd(Node, UUID, "hangup", _) ->
    lager:debug("terminate call on node ~s", [Node]),
    _ = ecallmgr_util:fs_log(Node, "whistle terminating call", []),
    freeswitch:api(Node, uuid_kill, wh_util:to_list(UUID));
send_cmd(Node, UUID, "set", "ecallmgr_Account-ID=" ++ Value) ->
    send_cmd(Node, UUID, "export", "ecallmgr_Account-ID=" ++ Value);
send_cmd(Node, UUID, AppName, Args) ->
    lager:debug("execute on node ~s: ~s(~s)", [Node, AppName, Args]),
    _ = ecallmgr_util:fs_log(Node, "whistle executing ~s ~s", [AppName, Args]),
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

-spec get_expires/1 :: (proplist()) -> integer().
get_expires(Props) ->
    Expiry = wh_util:to_integer(props:get_value(<<"Expires">>, Props
                                                ,props:get_value(<<"expires">>, Props, 300))),
    round(Expiry * 1.25).

-spec get_interface_properties/1 :: (atom()) -> proplist().
-spec get_interface_properties/2 :: (atom(), string() | ne_binary()) -> proplist().

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
-spec get_sip_to/1 :: (proplist()) -> ne_binary().
get_sip_to(Prop) ->
    list_to_binary([props:get_value(<<"sip_to_user">>, Prop
                                    ,props:get_value(<<"variable_sip_to_user">>, Prop, "nouser"))
                    ,"@"
                    ,props:get_value(<<"sip_to_host">>, Prop
                                      ,props:get_value(<<"variable_sip_to_host">>, Prop, ?DEFAULT_DOMAIN))
                   ]).

%% retrieves the sip address for the 'from' field
-spec get_sip_from/1 :: (proplist()) -> ne_binary().
get_sip_from(Prop) ->
    list_to_binary([props:get_value(<<"sip_from_user">>, Prop
                                    ,props:get_value(<<"variable_sip_from_user">>, Prop, "nouser"))
                    ,"@"
                    ,props:get_value(<<"sip_from_host">>, Prop
                                      ,props:get_value(<<"variable_sip_from_host">>, Prop, ?DEFAULT_DOMAIN))
                   ]).

%% retrieves the sip address for the 'request' field
-spec get_sip_request/1 :: (proplist()) -> ne_binary().
get_sip_request(Prop) ->
    list_to_binary([props:get_value(<<"Hunt-Destination-Number">>, Prop
                                    ,props:get_value(<<"Caller-Destination-Number">>, Prop, "nouser"))
                    ,"@"
                    ,props:get_value(list_to_binary(["variable_", ?CHANNEL_VAR_PREFIX, "Realm"]), Prop
                                                 ,props:get_value(<<"variable_sip_auth_realm">>, Prop, ?DEFAULT_DOMAIN))
                   ]).

-spec get_orig_ip/1 :: (proplist()) -> ne_binary().
get_orig_ip(Prop) ->
    props:get_value(<<"X-AUTH-IP">>, Prop, props:get_value(<<"ip">>, Prop)).

%% Extract custom channel variables to include in the event
-spec custom_channel_vars/1 :: (proplist()) -> proplist().
custom_channel_vars(Prop) ->
    lists:foldl(fun({<<"variable_", ?CHANNEL_VAR_PREFIX, Key/binary>>, V}, Acc) -> [{Key, V} | Acc];
                   ({<<?CHANNEL_VAR_PREFIX, Key/binary>>, V}, Acc) -> [{Key, V} | Acc];
                   ({<<"variable_sip_h_Referred-By">>, V}, Acc) -> [{<<"Referred-By">>, wh_util:to_binary(mochiweb_util:unquote(V))} | Acc];
                   ({<<"variable_sip_refer_to">>, V}, Acc) -> [{<<"Referred-To">>, wh_util:to_binary(mochiweb_util:unquote(V))} | Acc];
                   (_, Acc) -> Acc
                end, [], Prop).

-spec custom_sip_headers/1 :: (proplist()) -> proplist().
custom_sip_headers(Prop) ->
    lists:foldl(fun({<<"variable_sip_h_", Header/binary>>, V}, Acc) -> [{Header, wh_util:to_binary(mochiweb_util:unquote(V))} | Acc];
                   ({<<"variable_sip_rh_", Header/binary>>, V}, Acc) -> [{Header, wh_util:to_binary(mochiweb_util:unquote(V))} | Acc];
                   ({<<"variable_sip_ph_", Header/binary>>, V}, Acc) -> [{Header, wh_util:to_binary(mochiweb_util:unquote(V))} | Acc];
                   ({<<"variable_sip_bye_h_", Header/binary>>, V}, Acc) -> [{Header, wh_util:to_binary(mochiweb_util:unquote(V))} | Acc];
                   (_, Acc) -> Acc
                end, [], Prop).

%% convert a raw FS string of headers to a proplist
%% "Event-Name: NAME\nEvent-Timestamp: 1234\n" -> [{<<"Event-Name">>, <<"NAME">>}, {<<"Event-Timestamp">>, <<"1234">>}]
-spec eventstr_to_proplist/1 :: (ne_binary() | nonempty_string()) -> proplist().
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

-spec unserialize_fs_array/1 :: ('undefined' | ne_binary()) -> [ne_binary(),...].
unserialize_fs_array(undefined) ->
    [];
unserialize_fs_array(<<"ARRAY::", Serialized/binary>>) ->
    binary:split(Serialized, <<"|:">>, [global]).

%% convert a raw FS list of vars  to a proplist
%% "Event-Name=NAME,Event-Timestamp=1234" -> [{<<"Event-Name">>, <<"NAME">>}, {<<"Event-Timestamp">>, <<"1234">>}]
-spec varstr_to_proplist/1 :: (nonempty_string()) -> proplist().
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

-spec fs_log/3 :: (atom(), nonempty_string(), list()) -> fs_api_ret().
fs_log(Node, Format, Args) ->
    Log = case lists:flatten(io_lib:format("Notice log|~s|" ++ Format, [get(callid)] ++ Args)) of
              L when length(L) > 1016 ->
                  [lists:sublist(L, 1, 1016), "..."];
              Else  ->
                  Else
          end,
    freeswitch:api(Node, log, lists:flatten(Log)).

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
    KeyedEPs = [{[wh_json:get_value(<<"Invite-Format">>, Endpoint)
                  ,wh_json:get_value(<<"To-User">>, Endpoint)
                  ,wh_json:get_value(<<"To-Realm">>, Endpoint)
                  ,wh_json:get_value(<<"To-DID">>, Endpoint)
                  ,wh_json:get_value(<<"Route">>, Endpoint)
                 ]
                 ,Endpoint}
                || Endpoint <- Endpoints, wh_json:is_json_object(Endpoint)
               ],
    BridgeStrings = build_bridge_endpoints(props:unique(KeyedEPs), []),
    %% NOTE: dont use binary_join here as it will crash on an empty list...
    wh_util:join_binary(lists:reverse(BridgeStrings), Seperator).

-type build_return() :: ne_binary() | {'worker', pid()}.
-type bridge_endpoints() :: [{[ne_binary() | 'undefined',...], wh_json:json_object()},...] | [].
-spec build_bridge_endpoints/2 :: (bridge_endpoints(), [build_return(),...] | []) -> [ne_binary(),...].
build_bridge_endpoints([{[<<"route">>|_], Endpoint}|Endpoints], Channels) ->
    build_bridge_endpoints(Endpoints, [build_bridge_endpoint(Endpoint)|Channels]);
build_bridge_endpoints([{_, Endpoint}|Endpoints], Channels) ->
    S = self(),
    Pid = spawn(fun() ->
                        S ! {self(), build_bridge_endpoint(Endpoint)}
                end),
    build_bridge_endpoints(Endpoints, [{worker, Pid}|Channels]);
build_bridge_endpoints([], Channels) ->
    lists:foldr(fun({worker, Pid}, BridgeStrings) ->
                        receive
                            {Pid, BridgeString} ->
                                case wh_util:is_empty(BridgeString) of
                                    true -> BridgeStrings;
                                    false -> [BridgeString|BridgeStrings]
                                end
                        after
                            2000 -> BridgeStrings
                        end;
                 (BridgeString, BridgeStrings) -> [BridgeString|BridgeStrings]
                end, [], Channels).

-spec build_bridge_endpoint/1 :: (wh_json:json_object()) -> binary().
-spec build_bridge_endpoint/3 :: (wh_json:json_object(), ne_binary(), [nonempty_string(),...] | []) -> binary().
build_bridge_endpoint(JObj) ->
    build_bridge_endpoint(JObj
                          ,wh_json:get_value(<<"Endpoint-Type">>, JObj, <<"sip">>)
                          ,ecallmgr_fs_xml:get_leg_vars(JObj)
                         ).

build_bridge_endpoint(JObj, <<"sip">>, CVs) ->
    case ecallmgr_fs_xml:build_sip_route(JObj, wh_json:get_value(<<"Invite-Format">>, JObj)) of
        {'error', 'timeout'} ->
            lager:debug("unable to build route to endpoint"),
            <<>>;
        EndPoint ->
            list_to_binary([CVs, EndPoint])
    end;
build_bridge_endpoint(JObj, <<"freetdm">>, CVs) ->
    Endpoint = ecallmgr_fs_xml:build_freetdm_route(JObj),
    lager:info("freetdm endpoint: ~p", [Endpoint]),
    lager:info("freetdm ccvs: ~p", [CVs]),
    list_to_binary([CVs, Endpoint]).

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
recording_filename(MediaName) ->
    Ext = case filename:extension(MediaName) of
              Empty when Empty =:= <<>> orelse Empty =:= [] ->
                  ecallmgr_config:get(<<"default_recording_extension">>, <<".mp3">>);
              E -> E
          end,
    RootName = filename:rootname(MediaName),

    filename:join([ecallmgr_config:get(<<"recording_file_path">>, <<"/tmp/">>)
                   ,<<(amqp_util:encode(RootName))/binary, Ext/binary>>
                  ]).

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
                [{<<"Media-Name">>, MediaName}
                 ,{<<"Stream-Type">>, Type}
                 ,{<<"Call-ID">>, CallId}
                 ,{<<"Msg-ID">>, wh_util:to_binary(wh_util:current_tstamp())}
                 | wh_api:default_headers(<<"media">>, <<"media_req">>, ?APP_NAME, ?APP_VERSION)
                ]
                ,JObj),
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,Request
                                  ,fun wapi_media:publish_req/1
                                  ,fun wapi_media:resp_v/1),
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
