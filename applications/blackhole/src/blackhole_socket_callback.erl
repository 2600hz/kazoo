%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------

-module(blackhole_socket_callback).

-include("blackhole.hrl").

-export([open/3
        ,recv/4
        ,close/3
        ]).

-type cb_return() :: {'ok', bh_context:context()}.

-spec open(pid(), binary(), any()) -> cb_return().
open(Pid, Id, Ipaddr) ->
    IPBin = kz_util:to_binary(inet_parse:ntoa(Ipaddr)),
    lager:debug("opening socket (~p) ~p, peer: ~p", [Pid, Id, IPBin]),

    Context  = bh_context:new(Pid, Id),
    Context1 = bh_context:set_source(Context, IPBin),

    blackhole_tracking:add_socket(Context1),
    {'ok', Context1}.

-spec recv(pid(), binary(), {binary(), kz_json:object()}, bc_context:context()) -> cb_return().
recv(_SessionPid, _SessionId, {<<"subscribe">>, SubscriptionJObj}, Context) ->
    lager:debug("maybe add binding for session: ~p. Data: ~p", [_SessionId, SubscriptionJObj]),
    maybe_subscribe(Context, SubscriptionJObj);

recv(SessionPid, SessionId, {<<"unsubscribe">>, SubscriptionJObj}, Context) ->
    lager:debug("maybe remove binding for session: ~p. Data: ~p", [SessionId, SubscriptionJObj]),
    unsubscribe(Context, SubscriptionJObj, SessionPid, SessionId);

recv(_SessionPid, _SessionId, {_Event, _Data}, Context) ->
    lager:debug("received event: ~p on socket ~p with data payload", [_Event, _SessionId]),
    {'ok', Context};

recv(_SessionPid, SessionId, Message, Context) ->
    lager:info("receive unknown message ~p on socket ~p", [Message, SessionId]),
    {'ok', Context}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec close(ne_binary(), ne_binary(), bh_context:context()) -> 'ok'.
close(SessionPid, SessionId, Context) ->
    lager:debug("closing socket ~p", [SessionId]),
    _ = blackhole_tracking:remove_socket(Context),
    Filter = fun (A, B, C, D) -> filter_bindings(SessionPid, A, B, C, D) end,
    blackhole_bindings:filter(Filter),
    'ok'.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_subscribe(bh_context:context(), kz_json:object()) -> cb_return().
-spec maybe_subscribe(bh_context:context(), kz_json:object(), boolean()) -> cb_return().
maybe_subscribe(Context, JObj) ->
    Context1 = bh_context:from_json(Context, JObj),
    IsAuthorized = blackhole_util:is_authorized(Context1),
    maybe_subscribe(Context1, JObj, IsAuthorized).

maybe_subscribe(Context, _JObj, 'false') ->
    {'ok', blackhole_util:respond_with_authn_failure(Context)};
maybe_subscribe(Context, JObj, 'true') ->
    Bindings = bh_context:bindings_from_json(JObj),
    check_bindings(Context, JObj, Bindings).

-spec check_bindings(bh_context:context(), kz_json:object(), ne_binaries()) -> cb_return().
check_bindings(Context, _JObj, []) ->
    {'ok', Context};
check_bindings(Context, JObj, [Binding|Bds]) ->
    {'ok', Context1} = check_binding(Context, JObj, Binding),
    check_bindings(Context1, JObj, Bds).

-spec check_binding(bh_context:context(), kz_json:object(), ne_binary()) -> cb_return().
check_binding(Context, JObj, Binding) ->
    case bh_context:is_bound(Context, Binding) of
        'true' ->
            blackhole_util:send_error_message(Context, <<"binding already in use">>, Binding),
            {'ok', Context};
        'false' ->
            Module = blackhole_util:get_callback_module(Binding),
            subscribe(Context, JObj, Binding, Module)
    end.

-spec subscribe(bh_context:context(), kz_json:object(), ne_binary(), api_binary()) -> cb_return().
subscribe(Context, _JObj, _Binding, 'undefined') ->
    {'ok', blackhole_util:respond_with_error(Context)};
subscribe(Context, _JObj, Binding, Module) ->
    try Module:subscribe(Context#bh_context{binding=Binding}, Binding) of
        {'ok', Context1} ->
            Context2 = bh_context:add_binding(Context1, Binding),
            _ = blackhole_tracking:update_socket(Context2),
            {'ok', blackhole_util:respond_with_success(Context2, <<"subscribe">>, Binding)};
        {'error', Error} ->
            blackhole_util:send_error_message(Context, Module, Error),
            {'ok', Context}
    catch
        Error:_ ->
            blackhole_util:send_error_message(Context, Module, Error),
            {'ok', Context}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec unsubscribe(bh_context:context(), kz_json:object(), ne_binary(), ne_binary()) -> cb_return().
-spec unsubscribe(bh_context:context(), kz_json:object(), ne_binary(), ne_binary(), boolean()) -> cb_return().
unsubscribe(Context, JObj, SessionPid, SessionId) ->
    Context1 = bh_context:from_json(Context, JObj),
    IsAuthorized = blackhole_util:is_authorized(Context1),
    unsubscribe(Context1, JObj, SessionPid, SessionId, IsAuthorized).

unsubscribe(Context, _JObj, _SessionPid, _SessionId, 'false') ->
    {'ok', blackhole_util:respond_with_authn_failure(Context)};
unsubscribe(Context, JObj, SessionPid, SessionId, 'true') ->
    case kz_json:get_value(<<"account_id">>, JObj) of
        'undefined' ->
            unsubscribe_for_all(Context, JObj, SessionPid, SessionId);
        AccountId ->
            unsubscribe_for_account(Context, JObj, AccountId)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec unsubscribe_for_all(bh_context:context(), kz_json:object(), ne_binary(), ne_binary()) -> cb_return().
unsubscribe_for_all(Context, _JObj, SessionPid, SessionId) ->
    Context1 = bh_context:set_bindings(Context, []),
    lager:debug("remove all bindings for session: ~p", [SessionId]),
    _ = blackhole_tracking:update_socket(Context1),
    Filter = fun (A, B, C, D) -> filter_bindings(SessionPid, A, B, C, D) end,
    blackhole_bindings:filter(Filter),
    {'ok', Context1}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec unsubscribe_for_account(bh_context:context(), kz_json:object(), ne_binary()) -> cb_return().
-spec unsubscribe_for_account(bh_context:context(), kz_json:object(), ne_binary(), ne_binaries() | ne_binary()) -> cb_return().
-spec unsubscribe_for_account(bh_context:context(), kz_json:object(), ne_binary(), ne_binary(), api_binary()) -> cb_return().
unsubscribe_for_account(Context, JObj, AccountId) ->
    Bindings = bh_context:bindings_from_json(JObj),
    unsubscribe_for_account(Context, JObj, AccountId, Bindings).

unsubscribe_for_account(Context, _JObj, _AccountId, []) ->
    {'ok', Context};
unsubscribe_for_account(Context, JObj, AccountId, [Binding|Bds]) ->
    Module = blackhole_util:get_callback_module(Binding),
    {'ok', Context1} = unsubscribe_for_account(Context, JObj, AccountId, Binding, Module),
    unsubscribe_for_account(Context1, JObj, AccountId, Bds).

unsubscribe_for_account(Context, _JObj, _AccountId, _Binding, 'undefined') ->
    {'ok', blackhole_util:respond_with_error(Context)};
unsubscribe_for_account(Context, _JObj, AccountId, Binding, Module) ->
    Context1 = bh_context:remove_binding(Context, Binding),
    _ = blackhole_tracking:update_socket(Context1),
    lager:debug("remove binding for account_id: ~p", [AccountId]),
    try Module:unsubscribe(Context1, Binding) of
        {'ok', Context2} ->
            {'ok', blackhole_util:respond_with_success(Context2, <<"unsubscribe">>, Binding)}
    catch
        Error:_ ->
            blackhole_util:send_error_message(Context1, Module, Error),
            {'ok', Context1}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
filter_bindings(SessionPid, _Binding, _Module, _Function, Context) ->
    case bh_context:is_context(Context) of
        'false' -> 'true';
        'true' -> bh_context:websocket_pid(Context) =/= SessionPid
    end.
