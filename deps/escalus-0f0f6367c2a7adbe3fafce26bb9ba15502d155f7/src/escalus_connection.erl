%%%===================================================================
%%% @copyright (C) 2011-2012, Erlang Solutions Ltd.
%%% @doc Module providing basic client functionality
%%% @end
%%%===================================================================
-module(escalus_connection).

-include_lib("exml/include/exml_stream.hrl").
-include("escalus.hrl").

%% High-level API
-export([start/1, start/2,
         stop/1]).

%% Low-level API
-export([connect/1,
         send/2,
         get_stanza/2,
         get_stanza/3,
         get_sm_h/1,
         set_sm_h/2,
         set_filter_predicate/2,
         reset_parser/1,
         is_connected/1,
         kill/1]).

%% Behaviour helpers
-export([maybe_forward_to_owner/4]).

%% Public Types
-type client() :: #client{}.
-export_type([client/0]).

-type step_spec() :: atom() | {module(), atom()} | escalus_session:step().
-export_type([step_spec/0]).

-type filter_pred() :: fun((#xmlel{}) -> boolean()) | none.
-export_type([filter_pred/0]).

%% Private
-export([connection_step/2]).

-define(TIMEOUT, 1000).

%%%===================================================================
%%% Behaviour callback
%%%===================================================================
-callback connect([proplists:property()]) -> {ok, client()}.
-callback send(client(), #xmlel{}) -> no_return().
-callback stop(client()) -> ok | already_stopped.

-callback is_connected(client()) -> boolean().
-callback reset_parser(client()) -> no_return().
-callback kill(client()) -> no_return().
-callback set_filter_predicate(client(), filter_pred()) -> ok.


%%%===================================================================
%%% Public API
%%%===================================================================

-spec start(escalus_users:user_spec()) -> {ok, client(), escalus_users:user_spec()}
                                        | {error, any()}.
start(Props) ->
    start(Props,
          [start_stream,
           stream_features,
           maybe_use_ssl,
           maybe_use_compression,
           authenticate,
           bind,
           session,
           maybe_stream_management,
           maybe_use_carbons]).

%% Usage:
%%
%%     start(Props,
%%           [start_stream,
%%            maybe_use_ssl,
%%            maybe_use_compression,
%%            authenticate,
%%            bind,
%%            session])
%%
%%     or
%%
%%     start(Props,
%%           [start_stream,
%%            authenticate,
%%            bind])
%%     or
%%
%%     start(Props, [start_stream])
%%
%% 'maybe_*' will check allowed properties and features to see if it's possible
%% to use a feature.
%% Others will assume a feature is available and fail if it's not.
-spec start(escalus_users:user_spec(),
            [step_spec()]) -> {ok, client(), escalus_users:user_spec()} |
                              {error, any()}.
start(Props0, Steps) ->
    try
        {ok, Conn, Props} = connect(Props0),
        {Conn1, Props1, Features} = lists:foldl(fun connection_step/2,
                                                {Conn, Props, []},
                                                [prepare_step(Step)
                                                 || Step <- Steps]),
        {ok, Conn1, Props1, Features}
    catch
        throw:{connection_step_failed, _Details, _Reason} = Error ->
            {error, Error}
    end.

connection_step(Step, {Conn, Props, Features}) ->
    try
        case Step of
            {Mod, Fun} ->
                apply(Mod, Fun, [Conn, Props, Features]);
            Fun ->
                apply(Fun, [Conn, Props, Features])
        end
    catch
        Error ->
            (Conn#client.module):stop(Conn),
            throw({connection_step_failed, {Step, Conn, Props, Features}, Error})
    end.

%% By default use predefined connection steps from escalus_session.
prepare_step(Step) when is_atom(Step) ->
    {escalus_session, Step};
%% Accept functions defined in other modules.
prepare_step({Mod, Fun}) when is_atom(Mod), is_atom(Fun) ->
    {Mod, Fun};
%% Accept funs of arity 3.
prepare_step(Fun) when is_function(Fun, 3) ->
    Fun.

connect(Props) ->
    Transport = proplists:get_value(transport, Props, tcp),
    Server = proplists:get_value(server, Props, <<"localhost">>),
    Host = proplists:get_value(host, Props, Server),
    NewProps = lists:keystore(host, 1, Props, {host, Host}),
    Mod = get_module(Transport),
    {ok, Conn} = Mod:connect(NewProps),
    {ok, Conn, NewProps}.

send(#client{module = Mod, event_client = EventClient} = Client, Elem) ->
    escalus_event:outgoing_stanza(EventClient, Elem),
    Mod:send(Client, Elem).

-spec get_stanza(client(), any()) -> exml_stream:element().
get_stanza(Conn, Name) ->
    get_stanza(Conn, Name, ?TIMEOUT).

-spec get_stanza(client(), any(), timeout()) -> exml_stream:element().
get_stanza(Conn, Name, Timeout) ->
    receive
        {stanza, Conn, Stanza} ->
            Stanza
    after Timeout ->
            throw({timeout, Name})
    end.

-spec get_sm_h(#client{}) -> non_neg_integer().
get_sm_h(#client{module = escalus_tcp} = Conn) ->
    escalus_tcp:get_sm_h(Conn);
get_sm_h(#client{module = Mod}) ->
    error({get_sm_h, {undefined_for_escalus_module, Mod}}).

-spec set_sm_h(#client{}, non_neg_integer()) -> non_neg_integer().
set_sm_h(#client{module = escalus_tcp} = Conn, H) ->
    escalus_tcp:set_sm_h(Conn, H);
set_sm_h(#client{module = Mod}, _) ->
    error({set_sm_h, {undefined_for_escalus_module, Mod}}).

-spec set_filter_predicate(client(), filter_pred()) -> ok.
set_filter_predicate(#client{module = Module} = Conn, Pred) ->
    Module:set_filter_predicate(Conn, Pred).

reset_parser(#client{module = Mod} = Client) ->
    Mod:reset_parser(Client).

is_connected(#client{module = Mod} = Client) ->
    Mod:is_connected(Client).

stop(#client{module = Mod} = Client) ->
    Mod:stop(Client).

%% Brutally kill the connection without terminating the XMPP stream.
kill(#client{module = Mod} = Client) ->
    Mod:kill(Client).


-spec maybe_forward_to_owner(filter_pred(), term(), [#xmlel{}],
                             fun(([#xmlel{}], term()) -> term()))
        -> term().
maybe_forward_to_owner(none, State, _Stanzas, _Fun) ->
    State;
maybe_forward_to_owner(FilterPred, State, Stanzas, Fun)
    when is_function(FilterPred) ->
    AllowedStanzas = lists:filter(FilterPred, Stanzas),
    case AllowedStanzas of
        [] ->
            State;
        _ ->
            Fun(AllowedStanzas, State)
    end;
maybe_forward_to_owner(_, State, Stanzas, Fun) ->
    Fun(Stanzas, State).


%%%===================================================================
%%% Helpers
%%%===================================================================

get_module(tcp) ->
    escalus_tcp;
get_module(ws) ->
    escalus_ws;
get_module(bosh) ->
    escalus_bosh.
