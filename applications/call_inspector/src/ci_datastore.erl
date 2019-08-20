%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ci_datastore).
-behaviour(gen_server).

-include("call_inspector.hrl").

-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).
-export([store_chunk/1]).
-export([store_analysis/1]).
-export([lookup_callid/1
        ,lookup_objects/1
        ]).
-export([callid_exists/1]).
-export([flush/0
        ,flush/1
        ]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-record(state, {}).
-type state() :: #state{}.

-record(object, {call_id :: kz_term:ne_binary()
                ,timestamp = kz_time:now_s() :: kz_time:gregorian_seconds()
                ,type :: chunk | analysis
                ,value :: ci_chunk:chunk() | ci_analysis:analysis()
                }).
-type object() :: #object{}.

-type datum() :: {'chunks', [ci_chunk:chunk()]} |
                 {'analysis', [ci_analysis:analysis()]}.
-type data() :: [datum()].

-export_type([data/0]).

-define(CI_DIR, "/var/log/kazoo/call_inspector").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

-spec store_chunk(ci_chunk:chunk()) -> 'ok'.
store_chunk(Chunk) ->
    'true' = ci_chunk:is_chunk(Chunk),
    CallId = ci_chunk:call_id(Chunk),
    gen_server:cast(?SERVER, {'store_chunk', CallId, Chunk}).

-spec store_analysis(ci_analysis:analysis()) -> 'ok'.
store_analysis(Analysis) ->
    'true' = ci_analysis:is_analysis(Analysis),
    CallId = ci_analysis:call_id(Analysis),
    gen_server:cast(?SERVER, {'store_analysis', CallId, Analysis}).

-spec callid_exists(kz_term:ne_binary()) -> boolean().
callid_exists(CallId) ->
    File = make_name(CallId),
    Exists = filelib:is_file(File),
    Exists
        orelse lager:debug("~s not stored here", [CallId]),
    Exists.

-spec lookup_callid(kz_term:ne_binary()) -> data().
lookup_callid(CallId) ->
    Props = lists:foldl(fun lookup_callid_fold/2
                       ,[{'chunks', []}
                        ,{'analysis', []}
                        ]
                       ,lookup_objects(CallId)
                       ),
    Chunks = ci_chunk:reorder_dialog(props:get_value('chunks', Props)),
    props:set_value('chunks', Chunks, Props).

-spec lookup_callid_fold(object(), data()) -> data().
lookup_callid_fold(#object{type='chunk', value=Chunk}, P) ->
    Chunks = props:get_value('chunks', P, []),
    props:set_value('chunks', [Chunk|Chunks], P);
lookup_callid_fold(#object{type='analysis', value=Analysis}, P) ->
    props:set_value('analysis', Analysis, P).

-spec flush() -> 'ok'.
flush() ->
    gen_server:cast(?SERVER, 'flush').

-spec flush(kz_term:ne_binary()) -> 'ok'.
flush(CallId) ->
    gen_server:cast(?SERVER, {'flush', CallId}).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', #state{}}.
init([]) ->
    lager:debug("ensuring directory ~s exists", [?CI_DIR]),
    mkdir(?CI_DIR),
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(atom(), any(), state()) -> kz_types:handle_call_ret().
handle_call(_Request, _From, State) ->
    lager:debug("unhandled handle_call executed ~p~p", [_Request, _From]),
    Reply = 'ok',
    {'reply', Reply, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'store_chunk', CallId, Chunk}, State) ->
    Object = #object{call_id=CallId
                    ,type='chunk'
                    ,value=Chunk
                    },
    insert_object(Object),
    _ = ci_analyzers:new_chunk(CallId, Chunk),
    {'noreply', State};
handle_cast({'store_analysis', CallId, Analysis}, State) ->
    Object = #object{call_id=CallId
                    ,type='analysis'
                    ,value=Analysis
                    },
    insert_object(Object),
    {'noreply', State};
handle_cast('flush', State) ->
    recursive_remove(),
    {'noreply', State};
handle_cast({'flush', CallId}, State) ->
    kz_util:delete_file(make_name(CallId)),
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled handle_cast ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the gen_server terminate
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{}=_State) ->
    lager:debug("call inspector datastore terminated: ~p", [_Reason]),
    'ok'.

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
-spec make_name(kz_term:ne_binary()) -> file:filename().
make_name(CallId) ->
    <<D1:2/binary, D2:2/binary, Rest/binary>> = kz_binary:md5(CallId),
    filename:join([?CI_DIR, D1, D2, Rest]).

-spec ensure_path_exists(file:filename()) -> 'ok'.
ensure_path_exists(CallIdPath) ->
    mkdir(filename:dirname(filename:dirname(CallIdPath))),
    mkdir(filename:dirname(CallIdPath)).

-spec insert_object(object()) -> 'ok'.
insert_object(#object{call_id = CallId} = Object) ->
    Path = make_name(CallId),
    ensure_path_exists(Path),
    IoData = io_lib:fwrite("~p.\n", [Object]),
    kz_util:write_file(Path, IoData, ['append']).

-spec lookup_objects(kz_term:ne_binary()) -> [object()].
lookup_objects(CallId) ->
    Path = make_name(CallId),
    case filelib:is_file(Path) of
        'false' -> [];
        'true' ->
            {'ok', Objects} = file:consult(Path),
            Objects
    end.

-spec recursive_remove() -> 'ok'.
recursive_remove() ->
    F = fun (AbsPath, _Acc) ->
                'ok' = file:delete(AbsPath)
        end,
    filelib:fold_files(?CI_DIR, ".+", 'true', F, 'ok').

-spec mkdir(file:filename()) -> 'ok'.
mkdir(Path) ->
    case file:make_dir(Path) of
        'ok' -> 'ok';
        {'error', 'eexist'} -> 'ok'
    end.

%% End of Module.
