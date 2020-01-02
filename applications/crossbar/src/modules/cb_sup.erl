%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc Provides a similar interface to the SUP command-line utility. Maps to SUP
%%% commands most are familiar with already.
%%%
%%% /sup/module/function/arg1/arg2/...
%%%
%%% /sup/hangups [/{hangup_cause}] - stats about hangup causes
%%%   - you can also hit /accounts/{account_id}/sup for specific
%%%     account stats
%%% /sup/compactor - stats about the compactor
%%%
%%% Eventually support the idea of RPC-like AMQP requests to drill down per-node
%%% or per-application for these stats
%%%
%%%
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_sup).

-export([init/0
        ,authorize/1, authorize/2, authorize/3, authorize/4
        ,allowed_methods/1, allowed_methods/2, allowed_methods/3
        ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
        ,validate/1, validate/2, validate/3, validate/4

        ,format_path_tokens/1

         %% IO Server
        ,start_link/0
        ,init_io/1
        ,io_loop/2
        ,system_continue/3
        ,system_terminate/4
        ,system_get_state/1
        ,system_replace_state/2
        ]).

-include("crossbar.hrl").

-define(SERVER, ?MODULE).

-define(CHILDREN, []). %%FIXME: why is this not a supervisor?

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    proc_lib:start_link(?SERVER, 'init_io', [self()]).

-spec init_io(pid()) -> any().
init_io(Parent) ->
    kz_log:put_callid(<<"cb_sup_io_server">>),
    register(?SERVER, self()),
    lager:debug("acking to ~p", [Parent]),
    Debug = sys:debug_options([]),
    proc_lib:init_ack(Parent, {'ok', self()}),
    lager:debug("started io server for cb_sup"),
    io_loop(Parent, Debug).

-spec io_loop(pid(), any()) -> any().
io_loop(Parent, Debug) ->
    receive
        {'EXIT', Parent, Reason} ->
            lager:debug("recv exit from parent ~p: ~p", [Parent, Reason]),
            exit(Reason);
        {'system', From, Request} ->
            lager:debug("system request from ~p: ~p", [From, Request]),
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, 'ok');
        {'io_request', From, ReplyAs, Request} ->
            _ = handle_io_request(From, ReplyAs, Request),
            io_loop(Parent, Debug);
        _Msg ->
            lager:debug("unhandled msg: ~p", [_Msg]),
            io_loop(Parent, Debug)
    end.

handle_io_request(From, ReplyAs, {'put_chars', _Encoding, Characters}) ->
    From ! {'io_result', Characters},
    io_reply(From, ReplyAs, 'ok');
handle_io_request(From, ReplyAs, {'put_chars', _Encoding, M, F, A}) ->
    try apply(M,F,A) of
        Result ->
            From ! {'io_result', Result},
            io_reply(From, ReplyAs, 'ok')
    catch
        _E:R ->
            io_reply(From, ReplyAs, {'error', R})
    end.

io_reply(From, ReplyAs, Msg) ->
    From ! {'io_reply', ReplyAs, Msg}.

-spec system_continue(pid(), any(), any()) -> any().
system_continue(Parent, Debug, _State) ->
    io_loop(Parent, Debug).

-spec system_terminate(any(), pid(), any(), any()) -> no_return().
system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).

-spec system_get_state(any()) -> {ok, ok}.
system_get_state(_State) ->
    {'ok', 'ok'}.

-spec system_replace_state(any(), any()) -> {ok, ok, ok}.
system_replace_state(_StateFun, _State) ->
    {'ok', 'ok', 'ok'}.

-spec format_path_tokens(list()) -> list().
format_path_tokens([]) -> [];
format_path_tokens([_Module]=L) -> L;
format_path_tokens([_Module, _Function]=L) -> L;
format_path_tokens([Module, Function | Args]) -> [Module, Function, Args].

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to..
%% @end
%%------------------------------------------------------------------------------
-spec init() -> supervisor:startchild_ret().
init() ->
    Ret = crossbar_module_sup:start_child(?SERVER),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.sup">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.sup">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.sup">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.authorize.sup">>, ?MODULE, 'authorize'),
    Ret.

%%------------------------------------------------------------------------------
%% @doc Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%------------------------------------------------------------------------------

-spec authorize(cb_context:context()) -> 'false'.
authorize(_Context) ->
    'false'.

-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(Context, _Module) ->
    cb_context:is_superduper_admin(Context).

-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
authorize(Context, _Module, _Function) ->
    cb_context:is_superduper_admin(Context).

-spec authorize(cb_context:context(), path_token(), path_token(), path_token()) -> boolean().
authorize(Context, _Module, _Function, _Args) ->
    cb_context:is_superduper_admin(Context).

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_Module) ->
    [?HTTP_GET].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(_Module, _Function) ->
    [?HTTP_GET].

-spec allowed_methods(path_token(), path_token(), path_token()) -> http_methods().
allowed_methods(_Module, _Function, _Args) ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /sup => []
%%    /sup/foo => [<<"foo">>]
%%    /sup/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'false'.
resource_exists() -> 'false'.

-spec resource_exists(path_token()) -> boolean().
resource_exists(ModuleBin) ->
    does_resource_exist(ModuleBin, <<"status">>, []).

-spec resource_exists(path_token(), path_token()) -> boolean().
resource_exists(ModuleBin, FunctionBin) ->
    does_resource_exist(ModuleBin, FunctionBin, []).

-spec resource_exists(path_token(), path_token(), kz_term:ne_binaries()) -> boolean().
resource_exists(ModuleBin, FunctionBin, Args) ->
    does_resource_exist(ModuleBin, FunctionBin, Args).

-spec does_resource_exist(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binaries()) -> boolean().
does_resource_exist(ModuleBin, FunctionBin, Args) ->
    Arity = erlang:length(Args),
    kz_module:is_exported(maintenance_module_name(ModuleBin), FunctionBin, Arity).

-spec maintenance_module_name(kz_term:ne_binary()) -> module().
maintenance_module_name(ModuleBin) ->
    %% NOTE: the unsafe conversion to an atom is not an issue
    %%   in this module, despite coming from a user, because
    %%   only the system admin has access...
    kz_term:to_atom(<<ModuleBin/binary, "_maintenance">>, 'true').

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /sup might load a list of system_stat objects
%% /sup/123 might load the system_stat object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) -> Context.

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ModuleBin) ->
    validate_sup(Context, maintenance_module_name(ModuleBin), 'status', []).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, ModuleBin, FunctionBin) ->
    validate_sup(Context, maintenance_module_name(ModuleBin), kz_term:to_atom(FunctionBin), []).

-spec validate(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
validate(Context, ModuleBin, FunctionBin, Args) ->
    validate_sup(Context, maintenance_module_name(ModuleBin), kz_term:to_atom(FunctionBin), Args).

validate_sup(Context, Module, Function, Args) ->
    OldGroupLeader = group_leader(),
    group_leader(whereis(?MODULE), self()),
    lager:debug("attempting ~s:~s/~p", [Module, Function, length(Args)]),
    try apply(Module, Function, Args) of
        'no_return' ->
            group_leader(OldGroupLeader, self()),
            aggregate_results(Context);
        'ok' ->
            group_leader(OldGroupLeader, self()),
            aggregate_results(Context);
        {'error', E} ->
            group_leader(OldGroupLeader, self()),
            crossbar_util:response('error', E, Context);
        UnhandledResult ->
            group_leader(OldGroupLeader, self()),
            lager:debug("unhandled result: ~p", [UnhandledResult]),
            Result = iolist_to_binary(io_lib:format("~p", [UnhandledResult])),
            crossbar_util:response(Result, Context)
    catch
        _E:_R ->
            group_leader(OldGroupLeader, self()),
            lager:debug("failed to run ~p:~p/~p: ~s: ~p", [Module, Function, length(Args), _E, _R]),
            Context
    end.

-spec receive_io_results() -> binary().
receive_io_results() ->
    receive
        {'io_result', Result} -> receive_io_results([Result])
    after 50 -> <<>>
    end.

-spec receive_io_results(iolist()) -> binary().
receive_io_results(Acc) ->
    receive
        {'io_result', Result} -> receive_io_results([Result|Acc])
    after 50 -> iolist_to_binary(lists:reverse(Acc))
    end.

-spec aggregate_results(cb_context:context()) -> cb_context:context().
aggregate_results(Context) ->
    Result = receive_io_results(),
    lager:debug("maintenance result: ~p", [Result]),
    crossbar_util:response(Result, Context).
