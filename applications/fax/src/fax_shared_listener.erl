%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(fax_shared_listener).
-behaviour(gen_listener).

%% API
-export([start_link/0]).

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
-include_lib("kazoo_amqp/include/kapi_conf.hrl").

-record(state, {}).
-type state() :: #state{}.

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
                    ,{{'fax_request', 'new_request'}
                     ,[{<<"dialplan">>, <<"fax_req">>}]
                     }
                    ,{{'fax_xmpp', 'handle_printer_start'}
                     ,[{<<"xmpp_event">>, <<"start">>}]
                     }
                    ,{{'fax_xmpp', 'handle_printer_stop'}
                     ,[{<<"xmpp_event">>, <<"stop">>}]
                     }
                    ]).

-define(BINDINGS, [{'notifications', [{'restrict_to', ?NOTIFY_RESTRICT}]}
                  ,{'xmpp',[{'restrict_to',['push']}]}
                  ,{'xmpp', [{'restrict_to', ['start']}, 'federate']}
                  ,{'conf',?FAXBOX_RESTRICT}
                  ,{'fax', [{'restrict_to', ['req']}]}
                  ,{'self', []}
                  ]).
-define(QUEUE_NAME, <<"fax_shared_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link(?SERVER, [{'bindings', ?BINDINGS}
                                     ,{'responders', ?RESPONDERS}
                                     ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                     ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                     ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                     ], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'gen_listener',{'created_queue',_Queue}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'federators_consuming', _AreFederatorsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("fax shared listener terminating: ~p", [_Reason]).

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
