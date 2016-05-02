%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(fax_shared_listener).

-behaviour(gen_listener).

%% API
-export([start_link/0
         ,new_request/2
        ]).

%% gen_listener callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("fax.hrl").
-include_lib("kazoo/include/kapi_conf.hrl").

-define(SERVER, ?MODULE).

-define(NOTIFY_RESTRICT, ['outbound_fax'
                          ,'outbound_fax_error'
                         ]).

-define(FAXBOX_RESTRICT, [{'db', <<"faxes">>}
                          ,{'doc_type', <<"faxbox">>}
                         ]).

-define(RESPONDERS, [{{'fax_cloud', 'handle_job_notify'}
                      ,[{<<"notification">>, <<"outbound_fax">>}]
                     }
                     ,{{'fax_cloud', 'handle_job_notify'}
                       ,[{<<"notification">>, <<"outbound_fax_error">>}]
                      }
                     ,{{'fax_cloud', 'handle_push'}
                       ,[{<<"xmpp_event">>, <<"push">>}]
                      }
                     ,{{'fax_cloud', 'handle_faxbox_created'}
                       ,[{<<"configuration">>, ?DOC_CREATED}]
                      }
                     ,{{'fax_cloud', 'handle_faxbox_edited'}
                       ,[{<<"configuration">>, ?DOC_EDITED}]
                      }
                     ,{{'fax_cloud', 'handle_faxbox_deleted'}
                       ,[{<<"configuration">>, ?DOC_DELETED}]
                      }
                     ,{{?MODULE, 'new_request'}
                       ,[{<<"dialplan">>, <<"fax_req">>}]
                      }
                    ]).

-define(BINDINGS, [{'notifications', [{'restrict_to', ?NOTIFY_RESTRICT}]}
                   ,{'xmpp',[{'restrict_to',['push']}]}
                   ,{'conf',?FAXBOX_RESTRICT}
                   ,{'fax', [{'restrict_to', ['req']}]}
                   ,{'self', []}
                  ]).
-define(QUEUE_NAME, <<"fax_shared_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link(?SERVER, [{'bindings', ?BINDINGS}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                      ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                      ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                     ], []).

-spec new_request(kz_json:object(), kz_proplist()) -> sup_startchild_ret().
new_request(JObj, _Props) ->
    'true' = kapi_fax:req_v(JObj),
    fax_requests_sup:new(kapps_call:from_json(kz_json:get_value(<<"Call">>, JObj)), JObj).

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
    {'ok', 'ok'}.

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
    {'reply', {'error', 'not_implemented'}, State}.

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
handle_cast({'gen_listener',{'created_queue',_Queue}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
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
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

handle_event(_JObj, _State) ->
    {'reply', []}.

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
    lager:debug("fax shared listener terminating: ~p", [_Reason]).

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
