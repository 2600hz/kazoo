%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(media_tts).

-behaviour(gen_server).

%% API
-export([start_link/2
         ,single/1
         ,continuous/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("media.hrl").

-define(TIMEOUT_LIFETIME, 600000).

-record(state, {
          text :: ne_binary()
         ,contents = <<>> :: binary()
         ,status :: 'streaming' | 'ready'
         ,ibrowse_req_id :: ibrowse_req_id()
         ,reqs :: [{pid(), reference()},...] | []
         ,meta :: wh_json:json_object()
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
-spec start_link/2 :: (ne_binary(), wh_json:json_object()) -> {'ok', pid()}.
start_link(Text, JObj) ->
    gen_server:start_link(?MODULE, [Text, JObj], []).

single(Srv) ->
    gen_server:call(Srv, single).

continuous(Srv) ->
    gen_server:call(Srv, continuous).

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
init([Text, JObj]) ->
    put(callid, ?LOG_SYSTEM_ID),

    Voice = list_to_binary([wh_json:get_value(<<"Voice">>, JObj, <<"female">>), "/"
                            ,wh_json:get_value(<<"Language">>, JObj, <<"en-US">>)
                           ]),

    Format = wh_json:get_value(<<"Format">>, JObj, <<"wav">>),

    {ok, ReqID} = whapps_speech:create(Text, Voice, Format, [{stream_to, self()}]),

    Meta = wh_json:from_list([{<<"content_type">>, media_util:content_type_of(Format)}
                              ,{<<"media_name">>, wh_util:to_hex_binary(Text)}
                             ]),

    {'ok', #state{ibrowse_req_id = ReqID
                  ,status = streaming
                  ,meta = Meta
                  ,contents = <<>>
                  ,reqs = []
                 }
     ,?TIMEOUT_LIFETIME}.

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
handle_call(single, _From, #state{meta=Meta, contents=Contents, status=ready}=State) ->
    %% doesn't currently check whether we're still streaming in from the DB
    lager:debug("returning media contents"),
    {reply, {Meta, Contents}, State, ?TIMEOUT_LIFETIME};
handle_call(single, From, #state{reqs=Reqs, status=streaming}=State) ->
    lager:debug("file not ready for ~p, queueing", [From]),
    {noreply, State#state{reqs=[From | Reqs]}};
handle_call(continuous, _From, #state{}=State) ->
    {reply, ok, State, ?TIMEOUT_LIFETIME}.

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
handle_info(timeout, State) ->
    lager:debug("timeout expired, going down"),
    {stop, normal, State};

handle_info({ibrowse_async_headers, ReqID, "200", Hdrs}, #state{ibrowse_req_id=ReqID}=State) ->
    lager:debug("successfully retrieved audio file for tts"),
    {noreply, State#state{meta=wh_json:normalize(wh_json:from_list(kv_to_bin(Hdrs)))}, hibernate};

handle_info({ibrowse_async_headers, ReqID, "202", Hdrs}, #state{ibrowse_req_id=ReqID}=State) ->
    lager:debug("202 recv for tts request, awaiting further instructions"),
    {noreply, State#state{meta=wh_json:normalize(wh_json:from_list(kv_to_bin(Hdrs)))}, hibernate};

handle_info({ibrowse_async_response, ReqID, Bin}, #state{ibrowse_req_id=ReqID, meta=Meta, contents=Contents}=State) ->
    case wh_json:get_value(<<"content_type">>, Meta) of
        <<"audio/", _/binary>> ->
            lager:debug("recv ~b bytes", [byte_size(Bin)]),
            {noreply, State#state{contents = <<Contents/binary, Bin/binary>>}, hibernate};
        <<"application/json">> ->
            lager:debug("JSON response: ~s", [Bin]),
            {noreply, State}
    end;

handle_info({ibrowse_async_headers, ReqID, Bin}, #state{ibrowse_req_id=ReqID, contents=Contents}=State) ->
    lager:debug("recv ~b bytes", [byte_size(Bin)]),
    {noreply, State#state{contents = <<Contents/binary, Bin/binary>>}, hibernate};

handle_info({ibrowse_async_response_end, ReqID}, #state{ibrowse_req_id=ReqID, contents=Contents, meta=Meta, reqs=Reqs}=State) ->
    case Contents of
        <<>> ->
            lager:debug("no tts contents were received, going down"),
            {stop, normal, State};
        _ ->
            Res = {Meta, Contents},
            _ = [gen_server:reply(From, Res) || From <- Reqs],

            lager:debug("finished receiving file contents"),
            {noreply, State#state{status=ready}, ?TIMEOUT_LIFETIME}
    end;

handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {noreply, State, hibernate}.

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
kv_to_bin(L) ->
    [{wh_util:to_binary(K), wh_util:to_binary(V)} || {K,V} <- L].
