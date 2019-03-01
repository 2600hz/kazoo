%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Listener for authn_req, reg_success, and reg_query AMQP requests.
%%%
%%% @author James Aimonetti <james@2600hz.org>
%%% @end
%%%-----------------------------------------------------------------------------
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

-record(state, {wsdl_model :: 'undefined' | #wsdl{}
               ,dth_cdr_url = <<>> :: binary()
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link(?SERVER, [{'responders', ?RESPONDERS}
                                     ,{'bindings', ?BINDINGS}
                                     ], []).

%%%=============================================================================
%%% gen_listener callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
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

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Req, _From, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('blacklist_refresh', #state{wsdl_model='undefined'}=State) ->
    gen_listener:delayed_cast(self(), 'blacklist_refresh', ?BLACKLIST_REFRESH),
    {'noreply', State#state{wsdl_model=maybe_init_model()}};
handle_cast('blacklist_refresh', #state{wsdl_model=WSDL}=State) ->
    gen_listener:delayed_cast(self(), 'blacklist_refresh', ?BLACKLIST_REFRESH),
    _ = kz_util:spawn(fun refresh_blacklist/1, [WSDL]),
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling AMQP event objects
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, #state{dth_cdr_url=Url
                          ,wsdl_model=WSDL
                          }) ->
    {'reply', [{'cdr_url', Url}, {'wsdl', WSDL}]}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_listener' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_listener' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), #state{}) -> 'ok'.
terminate(_Reason, _) ->
    lager:debug("dth: ~p termination", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
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
    lager:debug("entries: ~p", [BlockListEntries]),
    kz_cache:store_local(?CACHE_NAME, dth_util:blacklist_cache_key(), BlockListEntries).

-spec get_blocklist_entries(#'p:GetBlockListResponse'{}) -> kz_json:object().
get_blocklist_entries(#'p:GetBlockListResponse'{
                         'GetBlockListResult'=#'p:ArrayOfBlockListEntry'{
                                                 'BlockListEntry'='undefined'
                                                }}) ->
    kz_json:new();
get_blocklist_entries(#'p:GetBlockListResponse'{
                         'GetBlockListResult'=#'p:ArrayOfBlockListEntry'{
                                                 'BlockListEntry'=Entries
                                                }}) when is_list(Entries) ->
    %% do some formatting of the entries to be [{ID, Reason}]
    kz_json:from_list([{kz_term:to_binary(ID), kz_term:to_binary(Reason)} || #'p:BlockListEntry'{'CustomerID'=ID, 'BlockReason'=Reason} <- Entries]).
