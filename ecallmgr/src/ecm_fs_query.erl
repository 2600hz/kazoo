%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Listener for whapp requests to query the underlying switches
%%% @end
%%% Created :  5 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecm_fs_query).

-behaviour(gen_listener).

%% API
-export([start_link/0, handle_channel_query/2, handle_channel_status/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2,
	 terminate/2, code_change/3]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-define(RESPONDERS, [
		     {{?MODULE, handle_channel_status}, [{<<"call_event">>, <<"status_req">>}]}
		     ,{{?MODULE, handle_channel_query}, [{<<"locate">>, <<"channel_req">>}]}
		    ]).
-define(BINDINGS, [
		   {switch_lookups, []}
		   ,{channel_query, []}
		  ]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_listener:start_link(?MODULE,
			    [{responders, ?RESPONDERS}
			     ,{bindings, ?BINDINGS}
			    ], []).

-spec handle_channel_status/2 :: (json_object(), proplist()) -> 'ok'.
handle_channel_status(JObj, _Props) ->
    true = wapi_call:status_req_v(JObj),
    wh_util:put_callid(JObj),
    CallID = wh_json:get_value(<<"Call-ID">>, JObj),

    ?LOG_START("channel status request received"),

    case [ecallmgr_fs_node:hostname(NH) || NH <- ecallmgr_fs_sup:node_handlers(), ecallmgr_fs_node:uuid_exists(NH, CallID)] of
	[] -> ?LOG("No node found having call");
	[{ok, Hostname}] ->
	    ?LOG("Call is on ~s", [Hostname]),
	    Resp = [{<<"Call-ID">>, CallID}
                    ,{<<"Status">>, <<"active">>}
                    ,{<<"Switch-Hostname">>, Hostname}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)],
	    wapi_call:publish_status_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp);
	[{error, Err}] ->
	    ?LOG("Hostname lookup failed, but call is active"),
	    Resp = [{<<"Call-ID">>, CallID}
                    ,{<<"Status">>, <<"active">>}
                    ,{<<"Error-Msg">>, wh_util:to_binary(Err)}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)],
	    wapi_call:publish_status_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp);
	[timeout] ->
	    Resp = [{<<"Call-ID">>, CallID}
                    ,{<<"Status">>, <<"active">>}
                    ,{<<"Error-Msg">>, <<"switch timeout">>}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)],
	    wapi_call:publish_status_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp)
    end.

handle_channel_query(JObj, _Props) ->
    true = wapi_channel_query:req_v(JObj),
    wh_util:put_callid(JObj),

    ?LOG("Channel query received"),

    ListOfChannels = [ecallmgr_fs_node:show_channels(Pid) || Pid <- ecallmgr_fs_sup:node_handlers()],

    SearchParams = lists:foldl(fun(Field, Acc) ->
				       case wh_json:get_value(Field, JObj) of
					   undefined -> Acc;
					   Value -> [{Field, Value} | Acc]
				       end
			       end, [], wapi_channel_query:optional_headers()),

    case lists:foldl(fun(NodeChannels, Acc) ->
			     filter_for_matching_uuids(SearchParams, NodeChannels, Acc)
		     end, [], ListOfChannels) of
	[] ->
	    ?LOG("No channels found that meet search parameters"),
	    ok;
	Matching ->
	    RespQ = wh_json:get_value(<<"Server-ID">>, JObj),
	    send_channel_query_resp(RespQ, Matching)
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

handle_event(_JObj, _State) ->
    {reply, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec filter_for_matching_uuids/3 :: (json_object(), [proplist(),...], [json_object(),...] | []) -> [json_object(),...] | [].
filter_for_matching_uuids(_, [], UUIDs) -> UUIDs;
filter_for_matching_uuids(SearchParams, [C|Cs], UUIDs) ->
    UUIDs1 = case lists:any(fun({<<"Call-ID">>,_}) -> false;
			       ({Key, FSValue}) ->
				    case wh_json:get_value(Key, SearchParams) of
					FSValue -> true;
					_ -> false
				    end
			    end, C) of
		 true ->
		     try
			 [ make_jobj(C) | UUIDs]
		     catch
			 throw:_E ->
			     ?LOG("Throw making jobj: ~p", [_E]),
			     UUIDs;
			 error:_E ->
			     ?LOG("Error making jobj: ~p", [_E]),
			     UUIDs
		     end;
		 false -> UUIDs
	     end,
    filter_for_matching_uuids(SearchParams, Cs, UUIDs1).

-spec make_jobj/1 :: (proplist()) -> json_object().
make_jobj(C) ->
    wh_json:from_list([{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, C)}
		       ,{<<"Switch-Hostname">>, wh_json:get_value(<<"Hostname">>, C)}
		      ]).

send_channel_query_resp(RespQ, UUIDs) ->
    Resp = [{<<"Active-Calls">>, UUIDs}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)],
    wapi_channel_query:publish_resp(RespQ, Resp).
