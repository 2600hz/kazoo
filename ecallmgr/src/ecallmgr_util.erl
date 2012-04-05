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

-export([get_interface_properties/1, get_interface_properties/2]).
-export([get_sip_to/1, get_sip_from/1, get_sip_request/1, get_orig_ip/1, custom_channel_vars/1]).
-export([eventstr_to_proplist/1, varstr_to_proplist/1, get_setting/1, get_setting/2]).
-export([is_node_up/1, is_node_up/2]).
-export([fs_log/3, put_callid/1]).
-export([build_bridge_string/1, build_bridge_string/2]).
-export([create_masquerade_event/2, create_masquerade_event/3]).
-export([media_path/2, media_path/3]).
-export([unserialize_fs_array/1]).
-export([convert_fs_evt_name/1, convert_whistle_app_name/1]).

-include("ecallmgr.hrl").

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
    ecallmgr_fs_handler:is_node_up(Node).

-spec is_node_up/2 :: (atom(), ne_binary()) -> boolean().
is_node_up(Node, UUID) ->
    case ecallmgr_fs_handler:is_node_up(Node) andalso freeswitch:api(Node, uuid_exists, wh_util:to_list(UUID)) of
        {'ok', IsUp} -> wh_util:is_true(IsUp);
        timeout -> timer:sleep(100), is_node_up(Node, UUID);
        _ -> false
    end.

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
    case props:get_value(<<"Call-ID">>, JObj) of
        undefined -> put(callid, wh_json:get_value(<<"Msg-ID">>, JObj, <<"0000000000">>));
        CallID -> put(callid, CallID)
    end.

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
                || Endpoint <- Endpoints
               ],    
    BridgeStrings = build_bridge_endpoints(props:unique(KeyedEPs), []),
    %% NOTE: dont use binary_join here as it will crash on an empty list...
    wh_util:join_binary(lists:reverse(BridgeStrings), Seperator).

-type build_return() :: ne_binary() | {'worker', pid()}.
-spec build_bridge_endpoints/2 :: (wh_json:json_objects(), [build_return(),...] | []) -> [ne_binary(),...].
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
                end, [], Channels);
build_bridge_endpoints([{[<<"route">>|_], Endpoint}|Endpoints], Channels) ->
    build_bridge_endpoints(Endpoints, [build_bridge_endpoint(Endpoint)|Channels]);
build_bridge_endpoints([{_, Endpoint}|Endpoints], Channels) ->
    S = self(),
    Pid = spawn(fun() ->
                        S ! {self(), build_bridge_endpoint(Endpoint)} 
                end),
    build_bridge_endpoints(Endpoints, [{worker, Pid}|Channels]).

-spec build_bridge_endpoint/1 :: (wh_json:json_object()) -> binary().
build_bridge_endpoint(JObj) ->
    build_bridge_endpoint(JObj, wh_json:get_value(<<"Endpoint-Type">>, JObj, <<"sip">>), ecallmgr_fs_xml:get_leg_vars(JObj)).

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
-spec media_path/2 :: (ne_binary(), ne_binary()) -> ne_binary().
-spec media_path/3 :: (ne_binary(), 'extant' | 'new', ne_binary()) -> ne_binary().
media_path(MediaName, UUID) ->
    media_path(MediaName, new, UUID).

media_path(undefined, _Type, _UUID) ->
    <<"silence_stream://5">>;
media_path(MediaName, Type, UUID) when not is_binary(MediaName) ->
    media_path(wh_util:to_binary(MediaName), Type, UUID);
media_path(<<"silence_stream://", _/binary>> = Media, _Type, _UUID) ->
    Media;
media_path(<<"tone_stream://", _/binary>> = Media, _Type, _UUID) ->
    Media;
media_path(<<"local_stream://", FSPath/binary>>, _Type, _UUID) ->
    FSPath;
media_path(MediaName, Type, UUID) ->
    case ecallmgr_media_registry:lookup_media(MediaName, Type, UUID) of
        {'error', _E} ->
            lager:debug("Failed to get media ~s: ~p", [MediaName, _E]),
            wh_util:to_binary(MediaName);
        {ok, Url} ->
            lager:debug("Recevied URL: ~s", [Url]),
            wh_util:to_binary(get_fs_playback(Url))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_fs_playback/1 :: (ne_binary()) -> ne_binary().
get_fs_playback(<<"http://", _/binary>>=Url) ->
    {ok, RemoteAudioScript} = get_setting(remote_audio_script, <<"/usr/local/bin/fetch_remote_audio.sh">>),
    <<"shell_stream://", (wh_util:to_binary(RemoteAudioScript))/binary, " ", Url/binary>>;
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
