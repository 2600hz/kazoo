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
-export([fs_log/3]).

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
                    ,props:get_value(<<"variable_sip_req_host">>, Prop
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
		   (_, Acc) -> Acc
		end, [], Prop).

%% convert a raw FS string of headers to a proplist
%% "Event-Name: NAME\nEvent-Timestamp: 1234\n" -> [{<<"Event-Name">>, <<"NAME">>}, {<<"Event-Timestamp">>, <<"1234">>}]
-spec eventstr_to_proplist/1 :: (EvtStr) -> proplist() when
      EvtStr :: string().
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
    case wh_cache:fetch({ecallmgr_setting, Setting}) of
        {ok, _}=Success -> Success;
        {error, _} ->
            case file:consult(?SETTINGS_FILE) of
                {ok, Settings} ->
                    Value = props:get_value(Setting, Settings, Default),
                    wh_cache:store({ecallmgr_setting, Setting}, Value),
                    {ok, Value};
                {error, _} ->
                    wh_cache:store({ecallmgr_setting, Setting}, Default),
                    {ok, Default}
            end
    end.

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

-spec fs_log/3 :: (Node, Format, Args) -> ok when
      Node :: binary(),
      Format :: string(),
      Args :: list().
fs_log(Node, Format, Args) ->
    Log = case lists:flatten(io_lib:format("Notice log|~s|" ++ Format, [get(callid)] ++ Args)) of
              L when length(L) > 1016 ->
                  [lists:sublist(L, 1, 1016), "..."];
              Else  ->
                  Else
          end,
    _ = freeswitch:api(Node, log, lists:flatten(Log)).
