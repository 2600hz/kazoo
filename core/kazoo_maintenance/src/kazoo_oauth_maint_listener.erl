%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_oauth_maint_listener).
-behaviour(gen_listener).

-export([start_link/0
        ,handle_req/2
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("kazoo_maintenance.hrl").
-include_lib("kazoo_amqp/include/kz_amqp.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).
-type state() :: #state{}.

%% By convention, we put the options here in macros, but not required.
%% define what databases or classifications we're interested in
-define(RESTRICTIONS, [kapi_maintenance:restrict_to_db(?KZ_OAUTH_DB)]).
-define(BINDINGS, [{'maintenance', [{'restrict_to', ?RESTRICTIONS}]}]).
-define(RESPONDERS, [{{?MODULE, 'handle_req'}
                     ,[{<<"maintenance">>, <<"req">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<?MODULE_STRING>>).
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
    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}       % optional to include
                            ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                            ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                            ,{'declare_exchanges', [{?EXCHANGE_SYSCONF, ?TYPE_SYSCONF}]}
                            ]
                           ,[]
                           ).

-spec handle_req(kapi_maintenance:req(), kz_term:proplist()) -> 'ok'.
handle_req(MaintJObj, _Props) ->
    'true' = kapi_maintenance:req_v(MaintJObj),
    Created = kz_datamgr:db_create(?KZ_OAUTH_DB),
    _ = kz_util:spawn(fun kazoo_oauth_maintenance:register_common_providers/0),
    send_resp(MaintJObj, Created).

-spec send_resp(kapi_mainteannce:req(), boolean()) -> 'ok'.
send_resp(MaintJObj, Created) ->
    Resp = [{<<"Code">>, code(Created)}
           ,{<<"Message">>, message(Created)}
           ,{<<"Msg-ID">>, kz_api:msg_id(MaintJObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_maintenance:publish_resp(kz_api:server_id(MaintJObj), Resp).

-spec code(boolean()) -> 200 | 500.
code('true') -> 200;
code('false') -> 500.

-spec message(boolean()) -> kz_term:ne_binary().
message('true') -> <<"Created ", (?KZ_OAUTH_DB)/binary>>;
message('false') -> <<"Did not create ", (?KZ_OAUTH_DB)/binary>>.

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
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
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
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_term:proplist()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

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
