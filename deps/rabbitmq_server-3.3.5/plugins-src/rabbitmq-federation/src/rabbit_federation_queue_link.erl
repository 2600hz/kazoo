%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ Federation.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2014 GoPivotal, Inc.  All rights reserved.
%%

-module(rabbit_federation_queue_link).

-include_lib("amqp_client/include/amqp_client.hrl").
-include("rabbit_federation.hrl").

-behaviour(gen_server2).

-export([start_link/1, go/0, run/1, pause/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(rabbit_misc, [pget/2]).
-import(rabbit_federation_util, [name/1, pgname/1]).

-record(not_started, {queue, run, upstream, upstream_params}).
-record(state, {queue, run, conn, ch, dconn, dch, upstream, upstream_params,
                unacked}).

start_link(Args) ->
    gen_server2:start_link(?MODULE, Args, [{timeout, infinity}]).

run(QName)   -> cast(QName, run).
pause(QName) -> cast(QName, pause).
go()         -> cast(go).

%%----------------------------------------------------------------------------
%%call(QName, Msg) -> [gen_server2:call(Pid, Msg, infinity) || Pid <- q(QName)].
cast(Msg)        -> [gen_server2:cast(Pid, Msg) || Pid <- all()].
cast(QName, Msg) -> [gen_server2:cast(Pid, Msg) || Pid <- q(QName)].

join(Name) ->
    pg2_fixed:create(pgname(Name)),
    ok = pg2_fixed:join(pgname(Name), self()).

all() ->
    pg2_fixed:create(pgname(rabbit_federation_queues)),
    pg2_fixed:get_members(pgname(rabbit_federation_queues)).

q(QName) ->
    pg2_fixed:create(pgname({rabbit_federation_queue, QName})),
    pg2_fixed:get_members(pgname({rabbit_federation_queue, QName})).

federation_up() ->
    proplists:is_defined(rabbitmq_federation,
                         application:which_applications(infinity)).

%%----------------------------------------------------------------------------

init({Upstream, Queue = #amqqueue{name = QName}}) ->
    case rabbit_amqqueue:lookup(QName) of
        {ok, Q} ->
            UParams = rabbit_federation_upstream:to_params(Upstream, Queue),
            rabbit_federation_status:report(Upstream, UParams, QName, starting),
            join(rabbit_federation_queues),
            join({rabbit_federation_queue, QName}),
            gen_server2:cast(self(), maybe_go),
            rabbit_amqqueue:notify_decorators(Q),
            {ok, #not_started{queue           = Queue,
                              run             = false,
                              upstream        = Upstream,
                              upstream_params = UParams}};
        {error, not_found} ->
            {stop, gone}
    end.

handle_call(Msg, _From, State) ->
    {stop, {unexpected_call, Msg}, State}.

handle_cast(maybe_go, State) ->
    case federation_up() of
        true  -> go(State);
        false -> {noreply, State}
    end;

handle_cast(go, State = #not_started{}) ->
    go(State);

handle_cast(go, State) ->
    {noreply, State};

handle_cast(run, State = #state{upstream        = Upstream,
                                upstream_params = UParams,
                                ch              = Ch,
                                run             = false}) ->
    consume(Ch, Upstream, UParams#upstream_params.x_or_q),
    {noreply, State#state{run = true}};

handle_cast(run, State = #not_started{}) ->
    {noreply, State#not_started{run = true}};

handle_cast(run, State) ->
    %% Already started
    {noreply, State};

handle_cast(pause, State = #state{run = false}) ->
    %% Already paused
    {noreply, State};

handle_cast(pause, State = #not_started{}) ->
    {noreply, State#not_started{run = false}};

handle_cast(pause, State = #state{ch = Ch}) ->
    cancel(Ch),
    {noreply, State#state{run = false}};

handle_cast(Msg, State) ->
    {stop, {unexpected_cast, Msg}, State}.

handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};

handle_info(#'basic.ack'{} = Ack, State = #state{ch      = Ch,
                                                 unacked = Unacked}) ->
    Unacked1 = rabbit_federation_link_util:ack(Ack, Ch, Unacked),
    {noreply, State#state{unacked = Unacked1}};

handle_info(#'basic.nack'{} = Nack, State = #state{ch      = Ch,
                                                   unacked = Unacked}) ->
    Unacked1 = rabbit_federation_link_util:nack(Nack, Ch, Unacked),
    {noreply, State#state{unacked = Unacked1}};

handle_info({#'basic.deliver'{redelivered = Redelivered,
                              exchange    = X,
                              routing_key = K} = DeliverMethod, Msg},
            State = #state{queue           = #amqqueue{name = QName},
                           upstream        = Upstream,
                           upstream_params = UParams,
                           ch              = Ch,
                           dch             = DCh,
                           unacked         = Unacked}) ->
    PublishMethod = #'basic.publish'{exchange    = <<"">>,
                                     routing_key = QName#resource.name},
    HeadersFun = fun (H) -> update_headers(UParams, Redelivered, X, K, H) end,
    ForwardFun = fun (_H) -> true end,
    Unacked1 = rabbit_federation_link_util:forward(
                 Upstream, DeliverMethod, Ch, DCh, PublishMethod,
                 HeadersFun, ForwardFun, Msg, Unacked),
    %% TODO actually we could reject when 'stopped'
    {noreply, State#state{unacked = Unacked1}};

handle_info(#'basic.cancel'{},
            State = #state{queue           = #amqqueue{name = QName},
                           upstream        = Upstream,
                           upstream_params = UParams}) ->
    rabbit_federation_link_util:connection_error(
      local, basic_cancel, Upstream, UParams, QName, State);

handle_info({'DOWN', _Ref, process, Pid, Reason},
            State = #state{dch             = DCh,
                           ch              = Ch,
                           upstream        = Upstream,
                           upstream_params = UParams,
                           queue           = #amqqueue{name = QName}}) ->
    rabbit_federation_link_util:handle_down(
      Pid, Reason, Ch, DCh, {Upstream, UParams, QName}, State);

handle_info(Msg, State) ->
    {stop, {unexpected_info, Msg}, State}.

terminate(Reason, #not_started{upstream        = Upstream,
                               upstream_params = UParams,
                               queue           = #amqqueue{name = QName}}) ->
    rabbit_federation_link_util:log_terminate(Reason, Upstream, UParams, QName),
    ok;

terminate(Reason, #state{dconn           = DConn,
                         conn            = Conn,
                         upstream        = Upstream,
                         upstream_params = UParams,
                         queue           = #amqqueue{name = QName}}) ->
    rabbit_federation_link_util:ensure_connection_closed(DConn),
    rabbit_federation_link_util:ensure_connection_closed(Conn),
    rabbit_federation_link_util:log_terminate(Reason, Upstream, UParams, QName),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------

go(S0 = #not_started{run             = Run,
                     upstream        = Upstream = #upstream{
                                         prefetch_count = Prefetch},
                     upstream_params = UParams,
                     queue           = Queue = #amqqueue{name = QName}}) ->
    #upstream_params{x_or_q = UQueue = #amqqueue{
                                durable     = Durable,
                                auto_delete = AutoDelete,
                                arguments   = Args}} = UParams,
    Unacked = rabbit_federation_link_util:unacked_new(),
    rabbit_federation_link_util:start_conn_ch(
      fun (Conn, Ch, DConn, DCh) ->
              check_upstream_suitable(Conn),
              amqp_channel:call(Ch, #'queue.declare'{queue       = name(UQueue),
                                                     durable     = Durable,
                                                     auto_delete = AutoDelete,
                                                     arguments   = Args}),
              case Upstream#upstream.ack_mode of
                  'no-ack' -> ok;
                  _        -> amqp_channel:call(
                                Ch, #'basic.qos'{prefetch_count = Prefetch})
              end,
              amqp_selective_consumer:register_default_consumer(Ch, self()),
              case Run of
                  true  -> consume(Ch, Upstream, UQueue);
                  false -> ok
              end,
              {noreply, #state{queue           = Queue,
                               run             = Run,
                               conn            = Conn,
                               ch              = Ch,
                               dconn           = DConn,
                               dch             = DCh,
                               upstream        = Upstream,
                               upstream_params = UParams,
                               unacked         = Unacked}}
      end, Upstream, UParams, QName, S0).

check_upstream_suitable(Conn) ->
    Props = pget(server_properties,
                 amqp_connection:info(Conn, [server_properties])),
    {table, Caps} = rabbit_misc:table_lookup(Props, <<"capabilities">>),
    case rabbit_misc:table_lookup(Caps, <<"consumer_priorities">>) of
        {bool, true} -> ok;
        _            -> exit({error, upstream_lacks_consumer_priorities})
    end.

update_headers(UParams, Redelivered, X, K, undefined) ->
    update_headers(UParams, Redelivered, X, K, []);

update_headers(#upstream_params{table = Table}, Redelivered, X, K, Headers) ->
    {Headers1, Count} =
        case rabbit_misc:table_lookup(Headers, ?ROUTING_HEADER) of
            undefined ->
                %% We only want to record the original exchange and
                %% routing key the first time a message gets
                %% forwarded; after that it's known that they were
                %% <<>> and QueueName respectively.
                {rabbit_misc:set_table_value(
                   rabbit_misc:set_table_value(
                     Headers, <<"x-original-exchange">>, longstr, X),
                   <<"x-original-routing-key">>, longstr, K), 0};
            {array, Been} ->
                {Found, Been1} = lists:partition(
                                      fun (I) -> visit_match(I, Table) end,
                                      Been),
                C = case Found of
                        []           -> 0;
                        [{table, T}] -> case rabbit_misc:table_lookup(
                                               T, <<"visit-count">>) of
                                            {_, I} when is_number(I) -> I;
                                            _                        -> 0
                                        end
                    end,
                {rabbit_misc:set_table_value(
                   Headers, ?ROUTING_HEADER, array, Been1), C}
        end,
    rabbit_basic:prepend_table_header(
      ?ROUTING_HEADER, Table ++ [{<<"redelivered">>, bool, Redelivered},
                                 {<<"visit-count">>, long, Count + 1}],
      swap_cc_header(Headers1)).

swap_cc_header(Table) ->
    [{case K of
          <<"CC">> -> <<"x-original-cc">>;
          _        -> K
      end, T, V} || {K, T, V} <- Table].

visit_match({table, T}, Info) ->
    lists:all(fun (K) ->
                      rabbit_misc:table_lookup(T, K) =:=
                          rabbit_misc:table_lookup(Info, K)
              end, [<<"uri">>, <<"virtual_host">>, <<"queue">>]);
visit_match(_ ,_) ->
    false.

consume(Ch, Upstream, UQueue) ->
    NoAck = Upstream#upstream.ack_mode =:= 'no-ack',
    amqp_channel:cast(
      Ch, #'basic.consume'{queue        = name(UQueue),
                           no_ack       = NoAck,
                           nowait       = true,
                           consumer_tag = <<"consumer">>,
                           arguments    = [{<<"x-priority">>, long, -1}]}).

cancel(Ch) ->
    amqp_channel:cast(Ch, #'basic.cancel'{nowait       = true,
                                          consumer_tag = <<"consumer">>}).
