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

-module(rabbit_federation_exchange_link).

-include_lib("amqp_client/include/amqp_client.hrl").
-include("rabbit_federation.hrl").

-behaviour(gen_server2).

-export([go/0, add_binding/3, remove_bindings/3]).
-export([list_routing_keys/1]). %% For testing

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(rabbit_misc, [pget/2]).
-import(rabbit_federation_util, [name/1, vhost/1, pgname/1]).

-record(state, {upstream,
                upstream_params,
                upstream_name,
                connection,
                channel,
                consumer_tag,
                queue,
                internal_exchange,
                waiting_cmds = gb_trees:empty(),
                next_serial,
                bindings = dict:new(),
                downstream_connection,
                downstream_channel,
                downstream_exchange,
                unacked}).

%%----------------------------------------------------------------------------

%% We start off in a state where we do not connect, since we can first
%% start during exchange recovery, when rabbit is not fully started
%% and the Erlang client is not running. This then gets invoked when
%% the federation app is started.
go() -> cast(go).

add_binding(S, XN, B)      -> cast(XN, {enqueue, S, {add_binding, B}}).
remove_bindings(S, XN, Bs) -> cast(XN, {enqueue, S, {remove_bindings, Bs}}).

list_routing_keys(XN) -> call(XN, list_routing_keys).

%%----------------------------------------------------------------------------

start_link(Args) ->
    gen_server2:start_link(?MODULE, Args, [{timeout, infinity}]).

init({Upstream, XName}) ->
    %% If we are starting up due to a policy change then it's possible
    %% for the exchange to have been deleted before we got here, in which
    %% case it's possible that delete callback would also have been called
    %% before we got here. So check if we still exist.
    case rabbit_exchange:lookup(XName) of
        {ok, X} ->
            UParams = rabbit_federation_upstream:to_params(Upstream, X),
            rabbit_federation_status:report(Upstream, UParams, XName, starting),
            join(rabbit_federation_exchanges),
            join({rabbit_federation_exchange, XName}),
            gen_server2:cast(self(), maybe_go),
            {ok, {not_started, {Upstream, UParams, XName}}};
        {error, not_found} ->
            {stop, gone}
    end.

handle_call(list_routing_keys, _From, State = #state{bindings = Bindings}) ->
    {reply, lists:sort([K || {K, _} <- dict:fetch_keys(Bindings)]), State};

handle_call(Msg, _From, State) ->
    {stop, {unexpected_call, Msg}, State}.

handle_cast(maybe_go, S0 = {not_started, _Args}) ->
    case federation_up() of
        true  -> go(S0);
        false -> {noreply, S0}
    end;

handle_cast(go, S0 = {not_started, _Args}) ->
    go(S0);

%% There's a small race - I think we can realise federation is up
%% before 'go' gets invoked. Ignore.
handle_cast(go, State) ->
    {noreply, State};

handle_cast({enqueue, _, _}, State = {not_started, _}) ->
    {noreply, State};

handle_cast({enqueue, Serial, Cmd}, State = #state{waiting_cmds = Waiting}) ->
    Waiting1 = gb_trees:insert(Serial, Cmd, Waiting),
    {noreply, play_back_commands(State#state{waiting_cmds = Waiting1})};

handle_cast(Msg, State) ->
    {stop, {unexpected_cast, Msg}, State}.

handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};

handle_info(#'basic.ack'{} = Ack, State = #state{channel = Ch,
                                                 unacked = Unacked}) ->
    Unacked1 = rabbit_federation_link_util:ack(Ack, Ch, Unacked),
    {noreply, State#state{unacked = Unacked1}};

handle_info(#'basic.nack'{} = Nack, State = #state{channel = Ch,
                                                   unacked = Unacked}) ->
    Unacked1 = rabbit_federation_link_util:nack(Nack, Ch, Unacked),
    {noreply, State#state{unacked = Unacked1}};

handle_info({#'basic.deliver'{routing_key  = Key,
                              redelivered  = Redelivered} = DeliverMethod, Msg},
            State = #state{
              upstream            = Upstream = #upstream{max_hops = MaxH},
              upstream_params     = UParams,
              upstream_name       = UName,
              downstream_exchange = #resource{name = XNameBin},
              downstream_channel  = DCh,
              channel             = Ch,
              unacked             = Unacked}) ->
    PublishMethod = #'basic.publish'{exchange    = XNameBin,
                                     routing_key = Key},
    %% TODO add user information here?
    HeadersFun = fun (H) -> update_headers(UParams, UName, Redelivered, H) end,
    %% We need to check should_forward/2 here in case the upstream
    %% does not have federation and thus is using a fanout exchange.
    ForwardFun = fun (H) ->
                         DName = rabbit_nodes:cluster_name(),
                         rabbit_federation_util:should_forward(H, MaxH, DName)
                 end,
    Unacked1 = rabbit_federation_link_util:forward(
                 Upstream, DeliverMethod, Ch, DCh, PublishMethod,
                 HeadersFun, ForwardFun, Msg, Unacked),
    {noreply, State#state{unacked = Unacked1}};

handle_info(#'basic.cancel'{}, State = #state{upstream            = Upstream,
                                              upstream_params     = UParams,
                                              downstream_exchange = XName}) ->
    rabbit_federation_link_util:connection_error(
      local, basic_cancel, Upstream, UParams, XName, State);

handle_info({'DOWN', _Ref, process, Pid, Reason},
            State = #state{downstream_channel  = DCh,
                           channel             = Ch,
                           upstream            = Upstream,
                           upstream_params     = UParams,
                           downstream_exchange = XName}) ->
    rabbit_federation_link_util:handle_down(
      Pid, Reason, Ch, DCh, {Upstream, UParams, XName}, State);

handle_info(Msg, State) ->
    {stop, {unexpected_info, Msg}, State}.

terminate(_Reason, {not_started, _}) ->
    ok;

terminate(Reason, #state{downstream_connection = DConn,
                         connection            = Conn,
                         upstream              = Upstream,
                         upstream_params       = UParams,
                         downstream_exchange   = XName}) ->
    rabbit_federation_link_util:ensure_connection_closed(DConn),
    rabbit_federation_link_util:ensure_connection_closed(Conn),
    rabbit_federation_link_util:log_terminate(Reason, Upstream, UParams, XName),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------

call(XName, Msg) -> [gen_server2:call(Pid, Msg, infinity) || Pid <- x(XName)].
cast(Msg)        -> [gen_server2:cast(Pid, Msg) || Pid <- all()].
cast(XName, Msg) -> [gen_server2:cast(Pid, Msg) || Pid <- x(XName)].

join(Name) ->
    pg2_fixed:create(pgname(Name)),
    ok = pg2_fixed:join(pgname(Name), self()).

all() ->
    pg2_fixed:create(pgname(rabbit_federation_exchanges)),
    pg2_fixed:get_members(pgname(rabbit_federation_exchanges)).

x(XName) ->
    pg2_fixed:create(pgname({rabbit_federation_exchange, XName})),
    pg2_fixed:get_members(pgname({rabbit_federation_exchange, XName})).

%%----------------------------------------------------------------------------

federation_up() -> is_pid(whereis(rabbit_federation_app)).

handle_command({add_binding, Binding}, State) ->
    add_binding(Binding, State);

handle_command({remove_bindings, Bindings}, State) ->
    lists:foldl(fun remove_binding/2, State, Bindings).

play_back_commands(State = #state{waiting_cmds = Waiting,
                                  next_serial  = Next}) ->
    case gb_trees:is_empty(Waiting) of
        false -> case gb_trees:take_smallest(Waiting) of
                     {Next, Cmd, Waiting1} ->
                         %% The next one. Just execute it.
                         play_back_commands(
                           handle_command(Cmd, State#state{
                                                 waiting_cmds = Waiting1,
                                                 next_serial  = Next + 1}));
                     {Serial, _Cmd, Waiting1} when Serial < Next ->
                         %% This command came from before we executed
                         %% binding:list_for_source. Ignore it.
                         play_back_commands(State#state{
                                              waiting_cmds = Waiting1});
                     _ ->
                         %% Some future command. Don't do anything.
                         State
                 end;
        true  -> State
    end.

add_binding(B, State) ->
    binding_op(fun record_binding/2, bind_cmd(bind, B, State), B, State).

remove_binding(B, State) ->
    binding_op(fun forget_binding/2, bind_cmd(unbind, B, State), B, State).

record_binding(B = #binding{destination = Dest},
               State = #state{bindings = Bs}) ->
    {DoIt, Set} = case dict:find(key(B), Bs) of
                      error       -> {true,  sets:from_list([Dest])};
                      {ok, Dests} -> {false, sets:add_element(
                                               Dest, Dests)}
                  end,
    {DoIt, State#state{bindings = dict:store(key(B), Set, Bs)}}.

forget_binding(B = #binding{destination = Dest},
               State = #state{bindings = Bs}) ->
    Dests = sets:del_element(Dest, dict:fetch(key(B), Bs)),
    {DoIt, Bs1} = case sets:size(Dests) of
                      0 -> {true,  dict:erase(key(B), Bs)};
                      _ -> {false, dict:store(key(B), Dests, Bs)}
                  end,
    {DoIt, State#state{bindings = Bs1}}.

binding_op(UpdateFun, Cmd, B = #binding{args = Args},
           State = #state{channel = Ch}) ->
    {DoIt, State1} =
        case rabbit_misc:table_lookup(Args, ?BINDING_HEADER) of
            undefined  -> UpdateFun(B, State);
            {array, _} -> {Cmd =/= ignore, State}
        end,
    case DoIt of
        true  -> amqp_channel:call(Ch, Cmd);
        false -> ok
    end,
    State1.

bind_cmd(Type, #binding{key = Key, args = Args},
         State = #state{internal_exchange = IntXNameBin,
                        upstream_params   = UpstreamParams}) ->
    #upstream_params{x_or_q = X} = UpstreamParams,
    case update_binding(Args, State) of
        ignore  -> ignore;
        NewArgs -> bind_cmd0(Type, name(X), IntXNameBin, Key, NewArgs)
    end.

bind_cmd0(bind, Source, Destination, RoutingKey, Arguments) ->
    #'exchange.bind'{source      = Source,
                     destination = Destination,
                     routing_key = RoutingKey,
                     arguments   = Arguments};

bind_cmd0(unbind, Source, Destination, RoutingKey, Arguments) ->
    #'exchange.unbind'{source      = Source,
                       destination = Destination,
                       routing_key = RoutingKey,
                       arguments   = Arguments}.

%% This function adds information about the current node to the
%% binding arguments, or returns 'ignore' if it determines the binding
%% should propagate no further. The interesting part is the latter.
%%
%% We want bindings to propagate in the same way as messages
%% w.r.t. max_hops - if we determine that a message can get from node
%% A to B (assuming bindings are in place) then it follows that a
%% binding at B should propagate back to A, and no further. There is
%% no point in propagating bindings past the point where messages
%% would propagate, and we will lose messages if bindings don't
%% propagate as far.
%%
%% Note that we still want to have limits on how far messages can
%% propagate: limiting our bindings is not enough, since other
%% bindings from other nodes can overlap.
%%
%% So in short we want bindings to obey max_hops. However, they can't
%% just obey the max_hops of the current link, since they are
%% travelling in the opposite direction to messages! Consider the
%% following federation:
%%
%%  A -----------> B -----------> C
%%     max_hops=1     max_hops=2
%%
%% where the arrows indicate message flow. A binding created at C
%% should propagate to B, then to A, and no further. Therefore every
%% time we traverse a link, we keep a count of the number of hops that
%% a message could have made so far to reach this point, and still be
%% able to propagate. When this number ("hops" below) reaches 0 we
%% propagate no further.
%%
%% hops(link(N)) is given by:
%%
%%   min(hops(link(N-1))-1, max_hops(link(N)))
%%
%% where link(N) is the link that bindings propagate over after N
%% steps (e.g. link(1) is CB above, link(2) is BA).
%%
%% In other words, we count down to 0 from the link with the most
%% restrictive max_hops we have yet passed through.

update_binding(Args, #state{downstream_exchange = X,
                            upstream            = Upstream,
                            upstream_name       = UName}) ->
    #upstream{max_hops = MaxHops} = Upstream,
    Hops = case rabbit_misc:table_lookup(Args, ?BINDING_HEADER) of
               undefined    -> MaxHops;
               {array, All} -> [{table, Prev} | _] = All,
                               {short, PrevHops} =
                                   rabbit_misc:table_lookup(Prev, <<"hops">>),
                               case rabbit_federation_util:already_seen(
                                      UName, All) of
                                   true  -> 0;
                                   false -> lists:min([PrevHops - 1, MaxHops])
                               end
           end,
    case Hops of
        0 -> ignore;
        _ -> Cluster = rabbit_nodes:cluster_name(),
             ABSuffix = rabbit_federation_db:get_active_suffix(
                          X, Upstream, <<"A">>),
             DVHost = vhost(X),
             DName = name(X),
             Down = <<DVHost/binary,":", DName/binary, " ", ABSuffix/binary>>,
             Info = [{<<"cluster-name">>, longstr, Cluster},
                     {<<"exchange">>,     longstr, Down},
                     {<<"hops">>,         short,   Hops}],
             rabbit_basic:prepend_table_header(?BINDING_HEADER, Info, Args)
    end.

key(#binding{key = Key, args = Args}) -> {Key, Args}.

go(S0 = {not_started, {Upstream, UParams, DownXName}}) ->
    Unacked = rabbit_federation_link_util:unacked_new(),
    rabbit_federation_link_util:start_conn_ch(
      fun (Conn, Ch, DConn, DCh) ->
              Props = pget(server_properties,
                           amqp_connection:info(Conn, [server_properties])),
              UName = case rabbit_misc:table_lookup(
                             Props, <<"cluster_name">>) of
                          {longstr, N} -> N;
                          _            -> unknown
                      end,
              {Serial, Bindings} =
                  rabbit_misc:execute_mnesia_transaction(
                    fun () ->
                            {rabbit_exchange:peek_serial(DownXName),
                             rabbit_binding:list_for_source(DownXName)}
                    end),
              true = is_integer(Serial),
              %% If we are very short lived, Serial can be undefined at
              %% this point (since the deletion of the X could have
              %% overtaken the creation of this process). However, this
              %% is not a big deal - 'undefined' just becomes the next
              %% serial we will process. Since it compares larger than
              %% any number we never process any commands. And we will
              %% soon get told to stop anyway.
              State = ensure_upstream_bindings(
                        consume_from_upstream_queue(
                          #state{upstream              = Upstream,
                                 upstream_params       = UParams,
                                 upstream_name         = UName,
                                 connection            = Conn,
                                 channel               = Ch,
                                 next_serial           = Serial,
                                 downstream_connection = DConn,
                                 downstream_channel    = DCh,
                                 downstream_exchange   = DownXName,
                                 unacked               = Unacked}),
                        Bindings),
              {noreply, State}
      end, Upstream, UParams, DownXName, S0).

consume_from_upstream_queue(
  State = #state{upstream            = Upstream,
                 upstream_params     = UParams,
                 channel             = Ch,
                 downstream_exchange = DownXName}) ->
    #upstream{prefetch_count = Prefetch,
              expires        = Expiry,
              message_ttl    = TTL,
              ha_policy      = HA} = Upstream,
    #upstream_params{x_or_q = X,
                     params = Params} = UParams,
    Q = upstream_queue_name(name(X), vhost(Params), DownXName),
    Args = [A || {_K, _T, V} = A
                     <- [{<<"x-expires">>,          long,    Expiry},
                         {<<"x-message-ttl">>,      long,    TTL},
                         {<<"x-ha-policy">>,        longstr, HA},
                         {<<"x-internal-purpose">>, longstr, <<"federation">>}],
                   V =/= none],
    amqp_channel:call(Ch, #'queue.declare'{queue     = Q,
                                           durable   = true,
                                           arguments = Args}),
    NoAck = Upstream#upstream.ack_mode =:= 'no-ack',
    case NoAck of
        false -> amqp_channel:call(Ch, #'basic.qos'{prefetch_count = Prefetch});
        true  -> ok
    end,
    #'basic.consume_ok'{consumer_tag = CTag} =
        amqp_channel:subscribe(Ch, #'basic.consume'{queue  = Q,
                                                    no_ack = NoAck}, self()),
    State#state{consumer_tag = CTag,
                queue        = Q}.

ensure_upstream_bindings(State = #state{upstream            = Upstream,
                                        upstream_params     = UParams,
                                        connection          = Conn,
                                        channel             = Ch,
                                        downstream_exchange = DownXName,
                                        queue               = Q}, Bindings) ->
    #upstream_params{x_or_q = X, params = Params} = UParams,
    OldSuffix = rabbit_federation_db:get_active_suffix(
                  DownXName, Upstream, <<"A">>),
    Suffix = case OldSuffix of
                 <<"A">> -> <<"B">>;
                 <<"B">> -> <<"A">>
             end,
    IntXNameBin = upstream_exchange_name(name(X), vhost(Params),
                                         DownXName, Suffix),
    ensure_upstream_exchange(State),
    ensure_internal_exchange(IntXNameBin, State),
    amqp_channel:call(Ch, #'queue.bind'{exchange = IntXNameBin, queue = Q}),
    State1 = State#state{internal_exchange = IntXNameBin},
    rabbit_federation_db:set_active_suffix(DownXName, Upstream, Suffix),
    State2 = lists:foldl(fun add_binding/2, State1, Bindings),
    OldIntXNameBin = upstream_exchange_name(
                       name(X), vhost(Params), DownXName, OldSuffix),
    delete_upstream_exchange(Conn, OldIntXNameBin),
    State2.

ensure_upstream_exchange(#state{upstream_params = UParams,
                                connection      = Conn,
                                channel         = Ch}) ->
    #upstream_params{x_or_q = X} = UParams,
    #exchange{type        = Type,
              durable     = Durable,
              auto_delete = AutoDelete,
              internal    = Internal,
              arguments   = Arguments} = X,
    Decl = #'exchange.declare'{exchange    = name(X),
                               type        = list_to_binary(atom_to_list(Type)),
                               durable     = Durable,
                               auto_delete = AutoDelete,
                               internal    = Internal,
                               arguments   = Arguments},
    rabbit_federation_link_util:disposable_channel_call(
      Conn, Decl#'exchange.declare'{passive = true},
      fun(?NOT_FOUND, _Text) ->
              amqp_channel:call(Ch, Decl)
      end).

ensure_internal_exchange(IntXNameBin,
                         #state{upstream        = #upstream{max_hops = MaxHops},
                                upstream_params = UParams,
                                connection      = Conn,
                                channel         = Ch}) ->
    #upstream_params{params = Params} = UParams,
    delete_upstream_exchange(Conn, IntXNameBin),
    Base = #'exchange.declare'{exchange    = IntXNameBin,
                               durable     = true,
                               internal    = true,
                               auto_delete = true},
    Purpose = [{<<"x-internal-purpose">>, longstr, <<"federation">>}],
    XFUArgs = [{?MAX_HOPS_ARG,  long,    MaxHops},
               {?NODE_NAME_ARG, longstr, rabbit_nodes:cluster_name()}
               | Purpose],
    XFU = Base#'exchange.declare'{type      = <<"x-federation-upstream">>,
                                  arguments = XFUArgs},
    Fan = Base#'exchange.declare'{type      = <<"fanout">>,
                                  arguments = Purpose},
    rabbit_federation_link_util:disposable_connection_call(
      Params, XFU, fun(?COMMAND_INVALID, _Text) ->
                           amqp_channel:call(Ch, Fan)
                   end).

upstream_queue_name(XNameBin, VHost, #resource{name         = DownXNameBin,
                                               virtual_host = DownVHost}) ->
    Node = rabbit_nodes:cluster_name(),
    DownPart = case DownVHost of
                   VHost -> case DownXNameBin of
                                XNameBin -> <<"">>;
                                _        -> <<":", DownXNameBin/binary>>
                            end;
                   _     -> <<":", DownVHost/binary,
                              ":", DownXNameBin/binary>>
               end,
    <<"federation: ", XNameBin/binary, " -> ", Node/binary, DownPart/binary>>.

upstream_exchange_name(XNameBin, VHost, DownXName, Suffix) ->
    Name = upstream_queue_name(XNameBin, VHost, DownXName),
    <<Name/binary, " ", Suffix/binary>>.

delete_upstream_exchange(Conn, XNameBin) ->
    rabbit_federation_link_util:disposable_channel_call(
      Conn, #'exchange.delete'{exchange = XNameBin}).

update_headers(#upstream_params{table = Table}, UName, Redelivered, Headers) ->
    rabbit_basic:prepend_table_header(
      ?ROUTING_HEADER, Table ++ [{<<"redelivered">>, bool, Redelivered}] ++
          header_for_name(UName),
      Headers).

header_for_name(unknown) -> [];
header_for_name(Name)    -> [{<<"cluster-name">>, longstr, Name}].
