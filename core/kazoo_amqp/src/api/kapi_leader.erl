%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_leader).

-export([api_definitions/0, api_definition/1]).

-export([whoami/0, whoami/1]).
-export([queue/0, queue/1, queue/2]).
-export([route/0, route/1, route/2]).
-export([req/1
        ,req_v/1
        ,publish_req/2
        ,publish_req/3
        ]).
-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-include_lib("kz_amqp_util.hrl").


%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [req_definition()].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"req">>) ->
    req_definition().

-spec req_definition() -> kapi_definition:api().
req_definition() ->
    Setters = [{fun kapi_definition:set_name/2, <<"req">>}
              ,{fun kapi_definition:set_friendly_name/2, <<"Leader Request">>}
              ,{fun kapi_definition:set_description/2, <<"Leader Request">>}
              ,{fun kapi_definition:set_category/2, <<"leader">>}
              ,{fun kapi_definition:set_build_fun/2, fun req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_req/2}
              ,{fun kapi_definition:set_required_headers/2, []}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Message">>]}
              ,{fun kapi_definition:set_values/2, []}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

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

-spec req(kz_term:api_terms()) -> kz_api:api_formatter_return().
req(Req) ->
    kapi_definition:build_message(Req, req_definition()).

-spec req_v(kz_term:api_terms()) -> boolean().
req_v(Req) ->
    kapi_definition:validate(Req, req_definition()).

-spec publish_req(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_req(Routing, JObj) ->
    publish_req(Routing, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_req(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_req(Routing, Req, ContentType) ->
    Definition = req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:leader_publish(Routing, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:leader_exchange().

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
