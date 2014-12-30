%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz INC
%%% @doc
%%%
%%% Provides a similar interface to the SUP command-line utility. Maps to SUP
%%% commands most are familiar with already.
%%%
%%% /sup/module/function/arg1/arg2/...
%%%
%%% /sup/hangups [/{hangup_cause}] - stats about hangup causes
%%%   - you can also hit /accounts/{account_id}/sup for specific
%%%     account stats
%%% /sup/compactor - stats about the compactor
%%%
%%% Eventaully support the idea of RPC-like AMQP requests to drill down per-node
%%% or per-application for these stats
%%%
%%% @end
%%% @contributors:
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_sup).

-export([init/0
         ,authorize/1, authorize/2, authorize/3, authorize/4
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
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

-include("../crossbar.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    proc_lib:start_link(?MODULE, 'init_io', [self()]).

init_io(Parent) ->
    wh_util:put_callid(<<"cb_sup_io_server">>),
    register(?MODULE, self()),
    lager:debug("Acking to ~p", [Parent]),
    Debug = sys:debug_options([]),
    proc_lib:init_ack(Parent, {'ok', self()}),
    lager:debug("started io server for cb_sup"),
    ?MODULE:io_loop(Parent, Debug).

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
            ?MODULE:io_loop(Parent, Debug);
        _Msg ->
            lager:debug("unhandled msg: ~p", [_Msg]),
            ?MODULE:io_loop(Parent, Debug)
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

system_continue(Parent, Debug, _State) ->
    ?MODULE:io_loop(Parent, Debug).

system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).

system_get_state(_State) ->
    {'ok', 'ok'}.

system_replace_state(_StateFun, _State) ->
    {'ok', 'ok', 'ok'}.

-spec format_path_tokens(list()) -> list().
format_path_tokens([]) -> [];
format_path_tokens([_Module]=L) -> L;
format_path_tokens([_Module, _Function]=L) -> L;
format_path_tokens([Module, Function | Args]) -> [Module, Function, Args].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    crossbar_module_sup:start_child(?MODULE),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.sup">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.sup">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.sup">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> 'false'.
-spec authorize(cb_context:context(), path_token()) -> boolean().
-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
-spec authorize(cb_context:context(), path_token(), path_token(), path_token()) -> boolean().

authorize(_Context) ->
    'false'.

authorize(Context, _Module) ->
    cb_modules_util:is_superduper_admin(Context).

authorize(Context, _Module, _Function) ->
    cb_modules_util:is_superduper_admin(Context).

authorize(Context, _Module, _Function, _Args) ->
    cb_modules_util:is_superduper_admin(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [].

allowed_methods(_Module) ->
    [?HTTP_GET].

allowed_methods(_Module, _Function) ->
    [?HTTP_GET].

allowed_methods(_Module, _Function, _Args) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /sup => []
%%    /sup/foo => [<<"foo">>]
%%    /sup/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'false'.
-spec resource_exists(path_token()) -> boolean().
-spec resource_exists(path_token(), path_token()) -> boolean().
-spec resource_exists(path_token(), path_token(), ne_binaries()) -> boolean().
resource_exists() -> 'false'.

resource_exists(ModuleBin) ->
    does_resource_exist(ModuleBin, 'status', []).

resource_exists(ModuleBin, FunctionBin) ->
    does_resource_exist(ModuleBin, FunctionBin, []).

resource_exists(ModuleBin, FunctionBin, Args) ->
    does_resource_exist(ModuleBin, FunctionBin, Args).

does_resource_exist(ModuleBin, FunctionBin, Args) ->
    Arity = erlang:length(Args),

    try {module_name(ModuleBin)
         ,wh_util:to_atom(FunctionBin)
        }
    of
        {Module, Function} ->
            lager:debug("checking existence of ~s:~s/~p", [Module, Function, Arity]),
            erlang:function_exported(Module, Function, Arity)
    catch
        'error':'badarg' ->
            lager:debug("failed to find ~s_maintenance:~s/~p", [ModuleBin, FunctionBin, Arity]),
            'false'
    end.

module_name(ModuleBin) ->
    %% NOTE: the unsafe convertion to an atom is not an issue
    %%   in this module, despite coming from a user, because
    %%   only the system admin has access...
    Module = wh_util:to_atom(<<ModuleBin/binary, "_maintenance">>, 'true'),
    try Module:module_info() of
        _ -> Module
    catch
        _E:R -> exit(R)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /sup mights load a list of system_stat objects
%% /sup/123 might load the system_stat object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
validate(Context) -> Context.

validate(Context, ModuleBin) ->
    validate_sup(Context, module_name(ModuleBin), 'status', []).
validate(Context, ModuleBin, FunctionBin) ->
    validate_sup(Context, module_name(ModuleBin), wh_util:to_atom(FunctionBin), []).
validate(Context, ModuleBin, FunctionBin, Args) ->
    validate_sup(Context, module_name(ModuleBin), wh_util:to_atom(FunctionBin), Args).

validate_sup(Context, Module, Function, Args) ->
    OldGroupLeader = group_leader(),
    group_leader(whereis(?MODULE), self()),
    lager:debug("attempting ~s_maintenance:~s/~p"
                ,[Module, Function, length(Args)]),
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
            lager:debug("failed to run ~p_maintenance:~p/~p: ~s: ~p", [Module, Function, length(Args), _E, _R]),
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
    after 50 -> iolist_to_binary(Acc)
    end.

-spec aggregate_results(cb_context:context()) -> cb_context:context().
aggregate_results(Context) ->
    Result = receive_io_results(),
    lager:debug("maintenance result: ~p", [Result]),
    crossbar_util:response(Result, Context).
