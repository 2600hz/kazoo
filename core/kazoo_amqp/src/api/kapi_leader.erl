%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_leader).

-export([whoami/0, whoami/1]).
-export([queue/0, queue/1, queue/2]).
-export([route/0, route/1, route/2]).
-export([req/1, req_v/1]).
-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).
-export([publish_req/2, publish_req/3]).

-include_lib("kz_amqp_util.hrl").

-define(LEADER_REQ_HEADERS, []).
-define(LEADER_REQ_VALUES, []).
-define(LEADER_REQ_TYPES, []).
-define(OPTIONAL_LEADER_REQ_HEADERS, [<<"Message">>]).

-spec whoami() -> pid() | {'registered_name', atom()}.
whoami() ->
    whoami(self()).

-spec whoami(pid()) -> pid() | {'registered_name', atom()}.
whoami(Pid) when is_pid(Pid) ->
    case erlang:process_info(Pid, 'registered_name') of
        {'registered_name', Name} -> {Name, node()};
        _ -> Pid
    end.

-spec queue() -> kz_term:ne_binary().
queue() ->
    queue(self()).

-spec queue(pid()) -> kz_term:ne_binary().
queue(Pid) when is_pid(Pid) ->
    case whoami(Pid) of
        Pid -> exit(<<"not registered">>);
        Name ->
            queue(Name)
    end;
queue(Name) when is_atom(Name) ->
    queue(Name, node());
queue({Name, Node}) ->
    queue(Name, Node).

-spec queue(atom(), atom()) -> kz_term:ne_binary().
queue(Name, Node) ->
    iolist_to_binary(io_lib:format("~s-~s", [Node, Name])).

-spec route() -> kz_term:ne_binary().
route() ->
    route(self()).

-spec route(pid() | atom()) -> kz_term:ne_binary().
route(Pid) when is_pid(Pid) ->
    {'registered_name', Name} = erlang:process_info(Pid, 'registered_name'),
    route(Name);
route(Name) when is_atom(Name) ->
    route(Name, node()).

-spec route(atom(), atom()) -> kz_term:ne_binary().
route(Name, Node) ->
    iolist_to_binary(io_lib:format("~s.~s", [Name, Node])).

-spec req(kz_term:api_terms()) -> api_formatter_return().
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?LEADER_REQ_HEADERS, ?OPTIONAL_LEADER_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for leader_req"}
    end;
req(JObj) ->
    req(kz_json:to_proplist(JObj)).

-spec req_v(kz_term:api_terms()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?LEADER_REQ_HEADERS, ?LEADER_REQ_VALUES, ?LEADER_REQ_TYPES);
req_v(JObj) ->
    req_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:leader_exchange().

-spec publish_req(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_req(Routing, JObj) ->
    publish_req(Routing, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_req(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_req(Routing, Req, ContentType) ->
    {ok, Payload} = kz_api:prepare_api_payload(Req, ?LEADER_REQ_VALUES, fun req/1),
    kz_amqp_util:leader_publish(Routing, Payload, ContentType).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, [{'name', Name}]) ->
    Node = node(),
    ProcessQ = iolist_to_binary(io_lib:format("~s.~s", [Name, Node])),
    BroadcastQ = iolist_to_binary(io_lib:format("~s.broadcast", [Name])),
    _ = ['ok' = kz_amqp_util:bind_q_to_leader(Queue, Bind) || Bind <- [ProcessQ, BroadcastQ]],
    'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, _) ->
    'ok' = kz_amqp_util:unbind_q_from_leader(Queue, <<"process">>).
