%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_number_crawler).
-behaviour(gen_server).

-export([start_link/0
        ,stop/0
        ,crawl_numbers/0
        ,crawl_number_db/1
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("tasks.hrl").
-include_lib("kazoo_numbers/include/knm_phone_number.hrl").

-define(SERVER, ?MODULE).

-define(NUMBERS_TO_CRAWL
       ,kapps_config:get_pos_integer(?SYSCONFIG_COUCH, <<"default_chunk_size">>, 1000)
       ).

-define(DISCOVERY_EXPIRY
       ,kapps_config:get_non_neg_integer(?CONFIG_CAT, <<"discovery_expiry_d">>, 1)
       ).

-define(AGING_EXPIRY
       ,kapps_config:get_non_neg_integer(?CONFIG_CAT, <<"aging_expiry_d">>, 90)
       ).

-define(TIME_BETWEEN_CRAWLS
       ,kapps_config:get_non_neg_integer(?CONFIG_CAT, <<"crawler_timer_ms">>, ?MILLISECONDS_IN_DAY)
       ).

-record(state, {cleanup_ref :: reference()}).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_server:start_link(?SERVER, [], []).

-spec stop() -> 'ok'.
stop() ->
    gen_server:cast(?SERVER, 'stop').

-spec crawl_numbers() -> 'ok'.
crawl_numbers() ->
    kz_log:put_callid(?SERVER),
    lager:debug("beginning a number crawl"),
    lists:foreach(fun(Num) ->
                          crawl_number_db(Num),
                          timer:sleep(10 * ?MILLISECONDS_IN_SECOND)
                  end
                 ,knm_util:get_all_number_dbs()
                 ),
    lager:debug("finished the number crawl"),
    lager:info("number crawler completed a full crawl").

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_log:put_callid(?SERVER),
    lager:debug("started ~s", [?SERVER]),
    {'ok', #state{cleanup_ref = cleanup_timer()}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('stop', State) ->
    lager:debug("crawler has been stopped"),
    {'stop', 'normal', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'timeout', Ref, _Msg}, #state{cleanup_ref=Ref}=State) ->
    _ = kz_process:spawn(fun crawl_numbers/0),
    {'noreply', State#state{cleanup_ref=cleanup_timer()}};
handle_info(_Msg, State) ->
    lager:debug("unhandled msg: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("~s terminating: ~p", [?SERVER, _Reason]).

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
-spec cleanup_timer() -> reference().
cleanup_timer() ->
    erlang:start_timer(?TIME_BETWEEN_CRAWLS, self(), 'ok').

-spec crawl_number_db(kz_term:ne_binary()) -> ok.
crawl_number_db(Db) ->
    case kz_datamgr:all_docs(Db, [include_docs]) of
        {error, _E} ->
            lager:debug("failed to crawl number db ~s: ~p", [Db, _E]);
        {ok, JObjs} ->
            lager:debug("starting to crawl '~s'", [Db]),
            _ = knm_pipe:pipe(knm_numbers:from_jobjs(JObjs)
                             ,[fun maybe_edit/1
                              ,fun knm_phone_number:save/1
                              ]),
            lager:debug("finished crawling '~s'", [Db])
    end.

-spec maybe_edit(knm_pipe:collection()) -> knm_pipe:collection().
%% FIXME: opaque
maybe_edit(T0=#{'todo' := PNs}) ->
    {ToRemove, ToSave} = lists:foldl(fun maybe_edit_fold/2, {[], []}, PNs),
    _ = knm_phone_number:delete(knm_pipe:set_todo(T0, ToRemove)),
    knm_pipe:set_succeeded(T0, ToSave).

-spec maybe_edit_fold(knm_phone_number:phone_number(), {knm_phone_number:phone_numbers(), knm_phone_number:phone_numbers()}) ->
          {knm_phone_number:phone_numbers(), knm_phone_number:phone_numbers()}.
maybe_edit_fold(PN, {ToRemove, ToSave}=To) ->
    case knm_phone_number:state(PN) of
        ?NUMBER_STATE_DELETED ->
            {[PN|ToRemove], ToSave};
        ?NUMBER_STATE_DISCOVERY ->
            {maybe_remove(PN, ToRemove, ?DISCOVERY_EXPIRY), ToSave};
        ?NUMBER_STATE_AGING ->
            {ToRemove, maybe_transition_aging(PN, ToSave, ?AGING_EXPIRY)};
        _ -> To
    end.

-spec maybe_remove(knm_phone_number:phone_number(), knm_phone_number:phone_numbers(), integer()) ->
          knm_phone_number:phone_numbers().
maybe_remove(PN, ToRemove, Expiry) ->
    case is_old_enough(PN, Expiry) of
        false -> ToRemove;
        true -> [PN|ToRemove]
    end.

-spec maybe_transition_aging(knm_phone_number:phone_number(), knm_phone_number:phone_numbers(), integer()) ->
          knm_phone_number:phone_numbers().
maybe_transition_aging(PN, ToSave, Expiry) ->
    case is_old_enough(PN, Expiry) of
        false -> ToSave;
        true ->
            lager:debug("transitioning number '~s' from ~s to ~s"
                       ,[knm_phone_number:number(PN)
                        ,?NUMBER_STATE_AGING
                        ,?NUMBER_STATE_AVAILABLE
                        ]),
            NewPN = knm_phone_number:set_state(PN, ?NUMBER_STATE_AVAILABLE),
            [NewPN|ToSave]
    end.

-spec is_old_enough(knm_phone_number:record(), pos_integer()) -> boolean().
is_old_enough(PN, Expiry) ->
    knm_phone_number:modified(PN) + Expiry * ?SECONDS_IN_DAY
        < kz_time:now_s().
