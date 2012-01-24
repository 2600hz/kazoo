%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handle an individual webhook request's lifecycle
%%% @end
%%% Created : 23 Jan 2012 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(hook_req).

-behaviour(gen_server).

%% API
-export([start_link/2, handle_req/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
         ,terminate/2, code_change/3]).

-include("webhooks.hrl").

-define(SERVER, ?MODULE).

-record(state, {
          callback_uri = 'undefined' :: 'undefined' | nonempty_string()
         ,callback_method = 'post' :: 'post' | 'put' | 'get'
         ,retries = 'undefined' :: 'undefined' | non_neg_integer()
         ,bind_event = 'undefined' :: 'undefined' | hook_types()
         ,acct_id = 'undefined' :: 'undefined' | ne_binary()
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
start_link(JObj, Props) ->
    gen_listener:start_link(?MODULE, [{responders, [{?MODULE, [{<<"*">>, <<"*">>}]}]}
                                      ,{bindings, [{self, []}]}
                                     ], [JObj, Props]).

handle_req(JObj, Props) ->
    ?LOG("received another payload, sending to callback"),
    gen_listener:cast(props:get_value(server, Props), {event, JObj}).

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
init([JObj, Props]) ->
    wh_util:put_callid(JObj),

    Hook = props:get_value(hook, Props),

    gen_listener:cast(self(), {start_req, JObj}),

    ?LOG("started req handler for acct ~s: ~s", [props:get_value(acct_id, Props)
                                                 ,wh_json:get_value(<<"hook_type">>, Hook)
                                                ]),

    {ok, #state{callback_uri = wh_json:get_string_value(<<"callback_uri">>, Hook)
                ,callback_method = wh_json:get_atom_value(<<"callback_method">>, Hook)
                ,retries = wh_json:get_integer_value(<<"retries">>, Hook, 0)
                ,bind_event = wh_json:get_atom_value(<<"hook_type">>, Hook)
                ,acct_id = props:get_value(acct_id, Props)
                ,realm = props:get_value(realm, Props)
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
handle_cast({event, _JObj}, State) ->
    ?LOG("followup event received"),
    {noreply, State};
handle_cast({start_req, JObj}, #state{bind_event=BindEvent}=State) ->
    case is_valid_req(JObj, BindEvent) of
        false ->
            ?LOG("json failed validation for ~s", [BindEvent]),
            {stop, invalid_req, State};
        true ->
            case send_http_req(JObj, State) of
                {ok, Resp} ->
                    ok = send_amqp_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp, BindEvent),
                    {noreply, State};
                {error, E} ->
                    {stop, E, State}
            end
    end.

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
    ?LOG("terminating handler for reason: ~p", [_Reason]).

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

-type ibrowse_errors() :: {'url_parsing_failed', {'invalid_uri_1', string()} |
                           {'EXIT', _} |
                           {invalid_uri_2, _, _, _}
                          }.
-spec send_http_req/2 :: (wh_json:json_object(), #state{}) -> {'ok', ne_binary()} |
                                                              {'error', 'retries_exceeded' | ibrowse_errors()}.
-spec send_http_req/3 :: (wh_json:json_object(), #state{}, non_neg_integer()) -> {'ok', ne_binary()} |
                                                                                 {'error', 'retries_exceeded' | ibrowse_errors()}.
send_http_req(JObj, #state{retries=Retries}=State) ->
    send_http_req(JObj, State, Retries+1).

send_http_req(_, _, 0) ->
    {error, retries_exceeded};
send_http_req(JObj, #state{callback_uri=URI
                           ,callback_method=Method
                           ,bind_event=BindEvent
                          }=State, Retries) ->
    ?LOG("sending req to ~s using http ~s", [URI, Method]),
    JSON = encode_req(BindEvent, JObj),

    case ibrowse:send_req(URI, ?DEFAULT_REQ_HEADERS, Method, JSON, ?DEFAULT_OPTS) of
        {ok, Status, RespHeaders, RespBody} ->
            case process_resp(Status, RespHeaders, RespBody) of
                {ok, _}=OK -> OK;
                {error, retry} -> send_http_req(JObj, State, Retries-1);
                {error, _E}=E ->
                    ?LOG("failed to receive proper response: ~p", [_E]),
                    E
            end;
        {error, _E}=E ->
            ?LOG("failed to send request to ~s", [URI]),
            E
    end.

-spec process_resp/3 :: (nonempty_string(), proplist(), ne_binary()) -> {'ok', ne_binary()} | {'error', 'retry' | term()}.
process_resp([$2|_]=_Status, RespHeaders, RespBody) ->
    ?LOG("successful status ~s received", [_Status]),
    decode_to_json(props:get_value("Content-Type", RespHeaders), RespBody);
process_resp(_Status, _RespHeaders, _RespBody) ->
    ?LOG("failed status ~s received", [_Status]),
    {error, retry}.

%% Should convert from ContentType to a JSON binary (like xml -> Erlang XML -> Erlang JSON -> json)
-spec decode_to_json/2 :: (nonempty_string(), ne_binary()) -> {'ok', ne_binary()} | {'error', 'unsupported_content_type'}.
decode_to_json("application/json" ++ _, JSON) ->
    {ok, JSON};
decode_to_json(_ContentType, _RespBody) ->
    ?LOG("unhandled content type: ~s", [_ContentType]),
    {error, unsupported_content_type}.

-spec encode_req/2 :: (wh_json:json_object(), hook_types()) -> {'ok', iolist() | ne_binary()} | {'error', string()}.
encode_req(JObj, BindEvent) ->
    case api_call(BindEvent, fun(ApiMod) -> ApiMod:req(JObj) end) of
        {ok, _}=OK -> OK;
        {error, try_again} -> encode_req(JObj, BindEvent);
        {error, _}=Err -> Err
    end.

-spec is_valid_req/2 :: (wh_json:json_object(), hook_types()) -> boolean().
is_valid_req(JObj, BindEvent) ->
    is_valid_req(JObj, BindEvent, 1).

is_valid_req(_JObj, _BindEvent, 0) ->
    false;
is_valid_req(JObj, BindEvent, 1) ->
    case api_call(BindEvent, fun(ApiMod) -> ApiMod:req_v(JObj) end) of
        {ok, Resp} -> Resp;
        {error, try_again} -> is_valid_req(JObj, BindEvent, 0);
        {error, _} -> false
    end.

-spec send_amqp_resp/3 :: (ne_binary(), wh_json:json_object(), hook_types()) -> 'ok'.
send_amqp_resp(RespQ, Resp, BindEvent) ->
    {'ok', 'ok'} = api_call(BindEvent, fun(ApiMod) -> ApiMod:publish_resp(RespQ, Resp) end),
    'ok'.

-type api_call_errors() :: 'non_existing' | 'try_again' | 'undefined' | term().

-spec api_call/2 :: (hook_types(), fun((atom()) -> Resp)) -> {'ok', Resp} | {'error', api_call_errors()}.
api_call(BindEvent, ApiFun) when is_function(ApiFun, 1) ->
    Wapi = list_to_binary([<<"wapi_">>, wh_util:to_list(BindEvent)]),
    try
        ApiMod = wh_util:to_atom(Wapi),
        ApiFun(ApiMod)
    catch
        error:badarg ->
            ?LOG_SYS("api module ~s not found", [Wapi]),
            case code:where_is_file(wh_util:to_list(<<Wapi/binary, ".beam">>)) of
                non_existing ->
                    ?LOG_SYS("beam file not found for ~s, fail", [Wapi]),
                    {error, non_existing};
                _Path ->
                    ?LOG_SYS("beam file found: ~s", [_Path]),
                    wh_util:to_atom(Wapi, true), %% put atom into atom table
                    try_again
            end;
        error:undef ->
            ?LOG_SYS("module ~s doesn't exist or fun isn't exported", [Wapi]),
            {error, undefined};
        E:R ->
            ST = erlang:get_stacktrace(),
            ?LOG("exception in executing ApiFun: ~p:~p", [E,R]),
            ?LOG_STACKTRACE(ST),
            {error, R}
    end.
