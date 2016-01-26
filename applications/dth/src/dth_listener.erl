%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Listener for authn_req, reg_success, and reg_query AMQP requests
%%% @end
%%% Created : 13 Jan 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(dth_listener).

-behaviour(gen_listener).

%% API
-export([start_link/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("dth.hrl").

-define(SERVER, ?MODULE).

-define(RESPONDERS, [{'dth_cdr_handler'
                      ,[{<<"call_event">>, <<"CHANNEL_DESTROY">>}]
                     }
                     ,{'dth_blacklist_req'
                       ,[{<<"dth">>, <<"blacklist_req">>}]
                      }
                    ]).
-define(BINDINGS, [{'call'
                    ,[{'restrict_to', [<<"CHANNEL_DESTROY">>]}]
                   }
                  ]).

-define(BLACKLIST_REFRESH, 60 * ?MILLISECONDS_IN_SECOND).

-record(state, {wsdl_model = 'undefined' :: 'undefined' | #wsdl{}
                ,dth_cdr_url = <<>> :: binary()
               }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link(?SERVER, [{'responders', ?RESPONDERS}
                                     ,{'bindings', ?BINDINGS}
                                     ], []).

%%%===================================================================
%%% gen_listener callbacks
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
    gen_listener:cast(self(), 'blacklist_refresh'),

    {'ok', Configs} = file:consult([code:priv_dir('dth'), "/startup.config"]),
    URL = props:get_value('dth_cdr_url', Configs),

    {'ok', #state{dth_cdr_url=URL}}.

-spec maybe_init_model() -> 'undefined' | #wsdl{}.
maybe_init_model() ->
    WSDLFile = [code:priv_dir('dth'), "/dthsoap.wsdl"],
    WSDLHrlFile = [code:lib_dir('dth', 'include'), "/dthsoap.hrl"],

    case {filelib:is_regular(WSDLFile), filelib:is_regular(WSDLHrlFile)} of
        {'false', _} ->
            lager:warning("DTH can't startup properly: failed to find WSDL ~p", [WSDLFile]),
            'undefined';
        {'true', 'true'} ->
            detergent:initModel(WSDLFile);
        {'true', 'false'} ->
            'true' = filelib:is_regular(WSDLFile),
            'ok' = detergent:write_hrl(WSDLFile, WSDLHrlFile),
            detergent:initModel(WSDLFile)
    end.

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
handle_call(_Req, _From, State) ->
    {'noreply', State}.

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
handle_cast('blacklist_refresh', #state{wsdl_model='undefined'}=State) ->
    gen_listener:delayed_cast(self(), 'blacklist_refresh', ?BLACKLIST_REFRESH),
    {'noreply', State#state{wsdl_model=maybe_init_model()}};
handle_cast('blacklist_refresh', #state{wsdl_model=WSDL}=State) ->
    gen_listener:delayed_cast(self(), 'blacklist_refresh', ?BLACKLIST_REFRESH),
    _ = wh_util:spawn(fun refresh_blacklist/1, [WSDL]),
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

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
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling AMQP event objects
%%
%% @spec handle_event(JObj, State) -> {reply, Props}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{dth_cdr_url=Url, wsdl_model=WSDL}) ->
    {'reply', [{'cdr_url', Url}, {'wsdl', WSDL}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_listener when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_listener terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), #state{}) -> 'ok'.
terminate(_Reason, _) ->
    lager:debug("dth: ~p termination", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec refresh_blacklist(#wsdl{}) -> 'ok'.
refresh_blacklist(WSDL) ->
    case detergent:call(WSDL, "GetBlockList", []) of
        {'ok', _, [Response]}  -> refresh_blacklist_response(Response);
        {'error', 'req_timedout'} ->
            lager:info("failed to call WSDL, request timed out");
        {'error', _E} -> lager:info("failed to call WSDL: ~p", [_E])
    end.

-spec refresh_blacklist_response(#'p:GetBlockListResponse'{}) -> 'ok'.
refresh_blacklist_response(Response) ->
    BlockListEntries = get_blocklist_entries(Response),
    lager:debug("Entries: ~p", [BlockListEntries]),
    wh_cache:store_local(?DTH_CACHE, dth_util:blacklist_cache_key(), BlockListEntries).

-spec get_blocklist_entries(#'p:GetBlockListResponse'{}) -> wh_json:object().
get_blocklist_entries(#'p:GetBlockListResponse'{
                         'GetBlockListResult'=#'p:ArrayOfBlockListEntry'{
                           'BlockListEntry'='undefined'
                          }}) ->
    wh_json:new();
get_blocklist_entries(#'p:GetBlockListResponse'{
                         'GetBlockListResult'=#'p:ArrayOfBlockListEntry'{
                           'BlockListEntry'=Entries
                          }}) when is_list(Entries) ->
    %% do some formatting of the entries to be [{ID, Reason}]
    wh_json:from_list([{wh_util:to_binary(ID), wh_util:to_binary(Reason)} || #'p:BlockListEntry'{'CustomerID'=ID, 'BlockReason'=Reason} <- Entries]).
