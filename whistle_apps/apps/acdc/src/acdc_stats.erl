%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%% Collector of stats
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_stats).

-behaviour(gen_listener).

-export([table_id/0
         ,key_pos/0
        ]).

%% gen_listener functions
-export([start_link/0
         ,init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("acdc.hrl").

-type call_status() :: 'waiting'    % caller is waiting for an agent
                     | 'handling'   % caller is being handled by an agent (still active)
                     | 'finished'   % caller has finished (probably by another node)
                     | 'processed'  % caller has finished, handled by us
                     | 'missed'     % agent missed the call
                     | 'abandoned'. % caller left the queue (hangup or dtmf exit)

-record(call_stat, {
          id = wh_util:rand_hex_binary(8) :: ne_binary()
          ,call_id :: api_binary()
          ,acct_id :: api_binary()
          ,queue_id :: api_binary()
          ,agent_id :: api_binary() % the handling agent
          ,timestamp = wh_util:current_tstamp() :: integer()
          ,wait_time :: integer() % how long was caller on hold
          ,talk_time :: integer() % how long was agent connected to caller
          ,abandon_reason :: abandon_reason()
          ,miss_reason :: api_binary() % why did the agent miss the call
          ,status :: call_status()
          ,caller_id_name :: api_binary()
          ,caller_id_number :: api_binary()
         }).
-type call_stat() :: #call_stat{}.

-record(agent_stat, {
          agent_id :: api_binary()
          ,acct_id :: api_binary()
          ,timestamp = wh_util:current_tstamp() :: integer()
          ,status :: 'ready' | 'logout' | 'paused' | 'wrapup' | 'busy' | 'handling'
          ,wait_time :: integer()
          ,call_id :: api_binary()
         }).
-type agent_stat() :: #agent_stat{}.

-type stat() :: call_stat() | agent_stat().

table_id() -> ?MODULE.
key_pos() -> #call_stat.id.

-define(BINDINGS, [{'self', []}
                   ,{'acdc_stats', []}
                  ]).
-define(RESPONDERS, [{{?MODULE, 'handle_stat'}, [{<<"acdc_stat">>, <<"*">>}]}]).
-define(QUEUE_NAME, <<>>).

start_link() ->
    gen_listener:start_link(?MODULE
                            ,[{'bindings', ?BINDINGS}
                              ,{'responders', ?RESPONDERS}
                              ,{'queue_name', ?QUEUE_NAME}
                             ],
                            []).

init([]) ->
    put('callid', <<"acdc.stats">>),
    Prefix = wh_util:rand_hex_binary(5),
    lager:debug("started new acdc stats collector, prefix: ~s", [Prefix]),
    {'ok', Prefix}.

handle_call(_Req, _From, State) ->
    {'reply', 'ok', State}.

handle_cast({'store', Stat}, State) ->
    store_stat(Stat, State),
    {'noreply', State};
handle_cast(_Req, State) ->
    {'noreply', State}.

handle_info(_Msg, State) ->
    lager:debug("unhandling message: ~p", [_Msg]),
    {'noreply', State}.

handle_event(_JObj, _State) ->
    {'reply', []}.

terminate(_Reason, _) ->
    lager:debug("acdc stats terminating: ~p", [_Reason]).

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

store_stat(#call_stat{acct_id=AcctId}=Stat, Prefix) ->
    JObj = stat_to_jobj(Stat, Prefix),
    store_stat(AcctId, JObj);
store_stat(#agent_stat{acct_id=AcctId}=Stat, Prefix) ->
    JObj = stat_to_jobj(Stat, Prefix),
    store_stat(AcctId, JObj);
store_stat(?NE_BINARY = AcctId, JObj) ->
    case couch_mgr:save_doc(db_name(AcctId), JObj) of
        {'ok', _JObj1} -> lager:debug("saved ~s(~s): c(~s) a(~s) t(~p)", [wh_json:get_value(<<"_id">>, _JObj1)
                                                                          ,wh_json:get_value(<<"status">>, _JObj1)
                                                                          ,wh_json:get_value(<<"call_id">>, _JObj1)
                                                                          ,wh_json:get_value(<<"agent_id">>, _JObj1)
                                                                          ,wh_json:get_value(<<"timestamp">>, _JObj1)
                                                                         ]);
        {'error', 'conflict'} -> lager:debug("conflict ~s(~s): c(~s) a(~s) t(~p)", [wh_json:get_value(<<"_id">>, JObj)
                                                                                    ,wh_json:get_value(<<"status">>, JObj)
                                                                                    ,wh_json:get_value(<<"call_id">>, JObj)
                                                                                    ,wh_json:get_value(<<"agent_id">>, JObj)
                                                                                    ,wh_json:get_value(<<"timestamp">>, JObj)
                                                                                   ]);
        {'error', _E} ->
            lager:debug("error saving: ~p", [_E]),
            timer:sleep(250 + random:uniform(100)),
            store_stat(AcctId, JObj)
    end.

-spec stat_to_jobj(stat(), ne_binary()) -> wh_json:object().
stat_to_jobj(#call_stat{acct_id=AcctId
                        ,queue_id=QueueId
                        ,agent_id=AgentId
                        ,call_id=CallId
                        ,status='handling'=Status
                        ,timestamp=Timestamp
                        ,caller_id_name=CName
                        ,caller_id_number=CNum
                       }, Prefix) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"call_id">>, CallId}
         ,{<<"pvt_account_id">>, AcctId}
         ,{<<"queue_id">>, QueueId}
         ,{<<"agent_id">>, AgentId}
         ,{<<"timestamp">>, Timestamp}
         ,{<<"status">>, Status}
         ,{<<"caller_id_name">>, CName}
         ,{<<"caller_id_number">>, CNum}
         ,{<<"pvt_type">>, <<"call_partial">>}
         ,{<<"_id">>, doc_id(Prefix, Timestamp)}
        ]));
stat_to_jobj(#call_stat{call_id=CallId
                        ,acct_id=AcctId
                        ,queue_id=QueueId
                        ,agent_id=AgentId
                        ,timestamp=TStamp
                        ,wait_time=WaitTime
                        ,talk_time=TalkTime
                        ,abandon_reason=AR
                        ,status=Status
                        ,caller_id_name=CName
                        ,caller_id_number=CNum
                        ,miss_reason=MissReason
                       }, Prefix) ->
    wh_doc:update_pvt_parameters(
      wh_json:from_list(
        props:filter_undefined(
          [{<<"call_id">>, CallId}
           ,{<<"pvt_account_id">>, AcctId}
           ,{<<"queue_id">>, QueueId}
           ,{<<"agent_id">>, AgentId}
           ,{<<"timestamp">>, TStamp}
           ,{<<"wait_time">>, WaitTime}
           ,{<<"talk_time">>, TalkTime}
           ,{<<"abandon_reason">>, AR}
           ,{<<"status">>, Status}
           ,{<<"miss_reason">>, MissReason}
           ,{<<"caller_id_name">>, CName}
           ,{<<"caller_id_number">>, CNum}
           ,{<<"pvt_type">>, <<"call_partial">>}
           ,{<<"_id">>, doc_id(Prefix, TStamp)}
          ])), AcctId, [{'account_id', AcctId}
                        ,{'account_db', db_name(AcctId)}
                       ]);
stat_to_jobj(#agent_stat{agent_id=AgentId
                         ,acct_id=AcctId
                         ,timestamp=TStamp
                         ,status=Status
                         ,wait_time=WaitTime
                         ,call_id=CallId
                        }, Prefix) ->
    wh_doc:update_pvt_parameters(
      wh_json:from_list(
        props:filter_undefined(
          [{<<"pvt_account_id">>, AcctId}
           ,{<<"agent_id">>, AgentId}
           ,{<<"status">>, Status}
           ,{<<"timestamp">>, TStamp}
           ,{<<"wait_time">>, WaitTime}
           ,{<<"pvt_type">>, <<"agent_partial">>}
           ,{<<"call_id">>, CallId}
           ,{<<"_id">>, doc_id(Prefix, TStamp)}
          ])), AcctId, [{'account_id', AcctId}
                        ,{'account_db', db_name(AcctId)}
                       ]).

-spec doc_id(ne_binary(), pos_integer()) -> ne_binary().
doc_id(Prefix, Timestamp) ->
    list_to_binary([Prefix, "-", wh_util:to_binary(Timestamp), "-", couch_mgr:get_uuid(5)]).

init_db(AcctId) ->
    DbName = db_name(AcctId),
    lager:debug("created db ~s: ~s", [DbName, couch_mgr:db_create(DbName)]),
    lager:debug("revised docs in ~s: ~p", [AcctId, couch_mgr:revise_views_from_folder(DbName, 'acdc')]).

db_name(Acct) ->
    <<A:2/binary, B:2/binary, Rest/binary>> = wh_util:format_account_id(Acct, 'raw'),
    <<"acdc%2F",A/binary,"%2F",B/binary,"%2F", Rest/binary>>.
