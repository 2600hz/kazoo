%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Various utilities specific to ecallmgr. More general utilities go
%%% in whistle_util.erl
%%% @end
%%% Created : 15 Nov 2010 by James Aimonetti <james@2600hz.org>

-module(ecallmgr_util).

-export([get_sip_to/1, get_sip_from/1, get_sip_request/1, get_orig_ip/1, custom_channel_vars/1]).
-export([eventstr_to_proplist/1, varstr_to_proplist/1, get_setting/1, get_setting/2]).
-export([is_node_up/1, is_node_up/2]).
-export([fs_log/3, put_callid/1]).
-export([media_path/2, media_path/3]).

-include("ecallmgr.hrl").

%% retrieves the sip address for the 'to' field
-spec get_sip_to/1 :: (Prop) -> binary() when
      Prop :: proplist().
get_sip_to(Prop) ->
    list_to_binary([props:get_value(<<"sip_to_user">>, Prop, props:get_value(<<"variable_sip_to_user">>, Prop, "nouser"))
		    , "@"
		    , props:get_value(<<"sip_to_host">>, Prop, props:get_value(<<"variable_sip_to_host">>, Prop, "nodomain"))
		   ]).

%% retrieves the sip address for the 'from' field
-spec get_sip_from/1 :: (Prop) -> binary() when
      Prop :: proplist().
get_sip_from(Prop) ->
    list_to_binary([
		    props:get_value(<<"sip_from_user">>, Prop, props:get_value(<<"variable_sip_from_user">>, Prop, "nouser"))
		    ,"@"
		    , props:get_value(<<"sip_from_host">>, Prop, props:get_value(<<"variable_sip_from_host">>, Prop, "nodomain"))
		   ]).

%% retrieves the sip address for the 'request' field
-spec get_sip_request/1 :: (Prop) -> binary() when
      Prop :: proplist().
get_sip_request(Prop) ->
    list_to_binary([
		    props:get_value(<<"Caller-Destination-Number">>, Prop, props:get_value(<<"variable_sip_req_user">>, Prop, "nouser"))
		    ,"@"
                    ,props:get_value(<<"variable_sip_auth_realm">>, Prop
                               ,props:get_value( list_to_binary(["variable_", ?CHANNEL_VAR_PREFIX, "Realm"]), Prop, "nodomain"))
		   ]).

-spec get_orig_ip/1 :: (Prop) -> binary() when
      Prop :: proplist().
get_orig_ip(Prop) ->
    props:get_value(<<"X-AUTH-IP">>, Prop, props:get_value(<<"ip">>, Prop)).

%% Extract custom channel variables to include in the event
-spec custom_channel_vars/1 :: (Prop) -> proplist() when
      Prop :: proplist().
custom_channel_vars(Prop) ->
    lists:foldl(fun({<<"variable_", ?CHANNEL_VAR_PREFIX, Key/binary>>, V}, Acc) -> [{Key, V} | Acc];
		   ({<<?CHANNEL_VAR_PREFIX, Key/binary>>, V}, Acc) -> [{Key, V} | Acc];
		   ({<<"variable_sip_h_Referred-By">>, V}, Acc) -> [{<<"Referred-By">>, wh_util:to_binary(mochiweb_util:unquote(V))} | Acc];
		   ({<<"variable_sip_refer_to">>, V}, Acc) -> [{<<"Referred-To">>, wh_util:to_binary(mochiweb_util:unquote(V))} | Acc];
		   (_, Acc) -> Acc
		end, [], Prop).

%% convert a raw FS string of headers to a proplist
%% "Event-Name: NAME\nEvent-Timestamp: 1234\n" -> [{<<"Event-Name">>, <<"NAME">>}, {<<"Event-Timestamp">>, <<"1234">>}]
-spec eventstr_to_proplist/1 :: (EvtStr) -> proplist() when
      EvtStr :: string() | binary().
eventstr_to_proplist(EvtStr) ->
    [to_kv(X, ": ") || X <- string:tokens(wh_util:to_list(EvtStr), "\n")].

-spec to_kv/2 :: (X, Separator) -> {binary(), binary()} when
      X :: string(),
      Separator :: string().
to_kv(X, Separator) ->
    [K, V] = string:tokens(X, Separator),
    [{V1,[]}] = mochiweb_util:parse_qs(V),
    {wh_util:to_binary(K), wh_util:to_binary(fix_value(K, V1))}.

fix_value("Event-Date-Timestamp", TStamp) ->
    wh_util:microseconds_to_seconds(wh_util:to_integer(TStamp));
fix_value(_K, V) -> V.


%% convert a raw FS list of vars  to a proplist
%% "Event-Name=NAME,Event-Timestamp=1234" -> [{<<"Event-Name">>, <<"NAME">>}, {<<"Event-Timestamp">>, <<"1234">>}]
-spec varstr_to_proplist/1 :: (VarStr) -> proplist() when
      VarStr :: string().
varstr_to_proplist(VarStr) ->
    [to_kv(X, "=") || X <- string:tokens(wh_util:to_list(VarStr), ",")].

-spec get_setting/1 :: (Setting) -> {ok, term()} when
      Setting :: atom().
-spec get_setting/2 :: (Setting, Default) -> {ok, term()} when
      Setting :: atom(),
      Default :: term().
get_setting(Setting) ->
    get_setting(Setting, undefined).
get_setting(Setting, Default) ->
    {ok, Cache} = ecallmgr_sup:cache_proc(),
    case wh_cache:fetch_local(Cache, cache_key(Setting)) of
        {ok, _}=Success -> Success;
        {error, _} ->
            case file:consult(?SETTINGS_FILE) of
                {ok, Settings} ->
                    Value = props:get_value(Setting, Settings, Default),
                    wh_cache:store_local(Cache, cache_key(Setting), Value),
                    {ok, Value};
                {error, _} ->
                    wh_cache:store_local(Cache, cache_key(Setting), Default),
                    {ok, Default}
            end
    end.

cache_key(Setting) ->
    {?MODULE, Setting}.

-spec is_node_up/1 :: (Node) -> boolean() when
      Node :: atom().
is_node_up(Node) ->
    ecallmgr_fs_handler:is_node_up(Node).

-spec is_node_up/2 :: (Node, UUID) -> boolean() when
      Node :: atom(),
      UUID :: binary().
is_node_up(Node, UUID) ->
    case ecallmgr_fs_handler:is_node_up(Node) andalso freeswitch:api(Node, uuid_exists, wh_util:to_list(UUID)) of
	{'ok', IsUp} -> wh_util:is_true(IsUp);
	timeout -> timer:sleep(100), is_node_up(Node, UUID);
	_ -> false
    end.

-spec fs_log/3 :: (Node, Format, Args) -> fs_api_ret() when
      Node :: atom(),
      Format :: string(),
      Args :: list().
fs_log(Node, Format, Args) ->
    Log = case lists:flatten(io_lib:format("Notice log|~s|" ++ Format, [get(callid)] ++ Args)) of
              L when length(L) > 1016 ->
                  [lists:sublist(L, 1, 1016), "..."];
              Else  ->
                  Else
          end,
    freeswitch:api(Node, log, lists:flatten(Log)).

-spec put_callid/1 :: (JObj) -> 'undefined' | term() when
      JObj :: json_object().
put_callid(JObj) ->
    case props:get_value(<<"Call-ID">>, JObj) of
	undefined -> put(callid, wh_json:get_value(<<"Msg-ID">>, JObj, <<"0000000000">>));
	CallID -> put(callid, CallID)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec media_path/2 :: (MediaName, UUID) -> binary() when
      MediaName :: binary(),
      UUID :: binary().
-spec media_path/3 :: (MediaName, Type, UUID) -> binary() when
      MediaName :: binary(),
      Type :: 'extant' | 'new',
      UUID :: binary().

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
media_path(MediaName, Type, UUID) ->
    case ecallmgr_media_registry:lookup_media(MediaName, Type, UUID) of
        {'error', _} ->
            MediaName;
        {ok, Url} ->
            get_fs_playback(Url)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_fs_playback/1 :: (Url) -> binary() when
      Url :: binary().
get_fs_playback(<<"http://", _/binary>>=Url) ->
    {ok, RemoteAudioScript} = get_setting(remote_audio_script, <<"/tmp/fetch_remote_audio.sh">>),
    <<"shell_stream://", (wh_util:to_binary(RemoteAudioScript))/binary, " ", Url/binary>>;
get_fs_playback(Url) ->
    Url.
