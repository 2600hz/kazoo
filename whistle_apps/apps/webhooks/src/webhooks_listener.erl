%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% Listener for a webhook. This module should do almost nothing with the
%%% the response besides receive it and pass it along to
%%% wh_api:disambiguate_and_publish/3
%%%
%%% @end
%%% Created : 29 Nov 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(webhooks_listener).

-behaviour(gen_listener).

%% API
-export([start_link/2, handle_req/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2
         ,handle_event/2, terminate/2, code_change/3]).

-include("webhooks.hrl").

-define(SERVER, ?MODULE). 

-record(state, {acctdb = 'undefined' :: 'undefined' | ne_binary()
               ,acctid = 'undefined' :: 'undefined' | ne_binary()
               ,webhook = 'undefined' :: 'undefined' | wh_json:json_object()
               ,realm = 'undefined' :: 'undefined' | ne_binary()
               }).

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
start_link(AcctDB, Webhooks) ->
    gen_listener:start_link(?MODULE, [{responders, [{?MODULE, {<<"*">>, <<"*">>}}]}
                                      ,{bindings, [{self, []}]}
                                     ], [AcctDB, Webhooks]).

handle_req(JObj, Props) ->
    wh_util:put_callid(JObj),

    Srv = props:get_value(server, Props),
    Hook = gen_listener:call(Srv, get_hook),

    Uri = wh_json:get_value(<<"callback_uri">>, Hook),
    Method = get_method_atom(wh_json:get_binary_value(<<"http_method">>, Hook, <<"post">>)),

    ?LOG("Sending json to ~s using method ~s", [Uri, Method]),

    Retries = wh_json:get_integer_value(<<"retries">>, Hook, 3),

    {ok, Resp} = try_send_req(Uri, Method, JObj, Retries),

    Q = props:get_value(queue, Props),

    try_send_resp(JObj, Resp, Q, wh_json:get_value(<<"bind_event">>, Hook)).

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
init([AcctDB, Webhook]) ->
    AcctID = whapps_util:get_db_name(AcctDB, raw),
    put(callid, AcctID),

    {ok, AcctDoc} = couch_mgr:open_doc(AcctDB, AcctID),
    Realm = wh_json:get_value(<<"realm">>, AcctDoc),

    Self = self(),
    spawn(fun() ->
                  BindOptions = case wh_json:get_value(<<"bind_options">>, Webhook, []) of
                                    Prop when is_list(Prop) -> Prop;
                                    JObj -> wh_json:to_proplist(JObj) % maybe [{restrict_to, [call, events]},...] or other json-y type
                                end,

                  gen_listener:add_binding(Self
                                           ,wh_json:get_value(<<"bind_event">>, Webhook)
                                           ,[{realm, Realm}, {acctid, AcctID} | BindOptions]
                                          )
          end),

    ?LOG("Starting webhook listener for ~s (~s)", [AcctID, Realm]),

    {ok, #state{acctdb=AcctDB
                ,acctid=AcctID
                ,realm=Realm
                ,webhook=Webhook
               }}.

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
handle_call(get_callback_uri, _From, #state{webhook=Hook}=State) ->
    {reply, wh_json:get_value(<<"callback_uri">>, Hook), State}.

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
handle_cast(_, State) ->
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

handle_event(_JObj, #state{webhook=Webhook, realm=Realm, acctid=AcctId}) ->
    {reply, [{hooks, Webhook}, {realm, Realm}, {acctid, AcctId}]}.

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
-spec try_send_req/4 :: (ne_binary(), 'put' | 'post' | 'get', wh_json:json_object(), non_neg_integer()) -> {'ok', wh_json:json_object()} | {'error', 'retries_exceeded'}.
try_send_req(_, _, _, R) when R =< 0 ->
    ?LOG("Retries exceeded"),
    {error, retries_exceeded};
try_send_req(Uri, Method, ReqJObj, Retries) ->
    try
        case ibrowse:send_req(wh_util:to_list(Uri)
                              ,[{"content-type", "application/json"}
                                ,{"accept", "application/json"}
                               ]
                              ,Method
                              ,wh_json:encode(ReqJObj)
                             ) of
            {ok, Status, ResponseHeaders, ResponseBody} ->
                ?LOG("Resp status: ~s", [Status]),
                _ = [?LOG("Resp header: ~s: ~s", [K,V]) || {K,V} <- ResponseHeaders],
                ?LOG("Resp body: ~s", [ResponseBody]),

                case wh_util:to_binary(props:get_value("Content-Type", ResponseHeaders)) of
                    <<"application/xml">> -> {ok, decode_xml(ResponseBody)};
                    _ -> {ok, wh_json:decode(ResponseBody)}
                end;
            {error, Reason} ->
                ?LOG("Request failed: ~p", [Reason]),
                try_send_req(Uri, Method, ReqJObj, Retries-1)
        end
    catch
        T:R ->
            ?LOG("Caught ~s:~p", [T, R]),
            try_send_req(Uri, Method, ReqJObj, Retries-1)
    end.

-spec get_method_atom/1 :: (<<_:24,_:_*8>>) -> 'put' | 'post' | 'get'.
get_method_atom(<<"put">>) -> put;
get_method_atom(<<"post">>) -> post;
get_method_atom(<<"get">>) -> get.

-spec decode_xml/1 :: (ne_binary()) -> no_return(). % throw({'not_supported', 'xml'}).
decode_xml(_Body) ->
    %% eventually support TwiML and other XML-based formats
    throw({not_supported, xml}).

try_send_resp(ReqJObj, HTTPJObj, MyQ, Binding) ->
    DefaultJObj = wh_json:from_list(wh_api:default_headers(MyQ, ?APP_NAME, ?APP_VERSION)),
    RespJObj = wh_json:merge_jobj(DefaultJObj, HTTPJObj),

    ?LOG("Disambiguating on binding ~s", [Binding]),
    wh_api:disambiguate_and_publish(ReqJObj, RespJObj, Binding).
