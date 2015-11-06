%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2014, 2600hz Inc
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
        ,handle_info/4
        ]).

-type cb_return() :: {'ok', bh_context:context()}.
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec open(pid(), ne_binary(), any()) -> cb_return().
open(SessionPid, SessionId, _Opts) ->
    lager:debug("opening socket ~p", [SessionId]),
    Context = bh_context:new(SessionPid, SessionId),
    _ = blackhole_tracking:add_socket(Context),
    {'ok', Context}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec recv(ne_binary(), ne_binary(), any(), bh_context:context()) -> cb_return().
recv(_SessionPid, SessionId, {'message', <<>>, Message}, State) ->
    lager:debug("received message ~p on socket ~p", [Message, SessionId]),
    {'ok', State};
recv(_SessionPid, _SessionId, {'event', _Ignore, <<"subscribe">>, SubscriptionJObj}, Context) ->
    lager:debug("maybe add binding for session: ~p. Data: ~p", [_SessionId, SubscriptionJObj]),
    subscribe(Context, SubscriptionJObj);
recv(SessionPid, SessionId, {'event', _Ignore, <<"unsubscribe">>, SubscriptionJObj}, Context) ->
    lager:debug("maybe remove binding for session: ~p. Data: ~p", [SessionId, SubscriptionJObj]),
    unsubscribe(Context, SubscriptionJObj, SessionPid, SessionId);
recv(_SessionPid, _SessionId, {'event', _Ignore, _Event, _Data}, Context) ->
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_info(ne_binary(), ne_binary(), any(), bh_context:context()) -> {'ok', bh_context:context()}.
handle_info(_SessionPid, _SessionId, _Msg, Context) ->
    {'ok', Context}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec subscribe(bh_context:context(), wh_json:object()) -> cb_return().
-spec subscribe(bh_context:context(), wh_json:object(), boolean()) -> cb_return().
-spec subscribe(bh_context:context(), wh_json:object(), ne_binary(), api_binary()) -> cb_return().
subscribe(Context, JObj) ->
    Context1 = bh_context:subscribe(Context, JObj),
    IsAuthorized = blackhole_util:is_authorized(Context1),
    subscribe(Context1, JObj, IsAuthorized).

subscribe(Context, _JObj, 'false') ->
    {'ok', blackhole_util:respond_with_authn_failure(Context)};
subscribe(Context, JObj, 'true') ->
    Binding = wh_json:get_value(<<"binding">>, JObj),
    Module = blackhole_util:get_callback_module(Binding),
    subscribe(Context, JObj, Binding, Module).

subscribe(Context, _JObj, _Binding, 'undefined') ->
    {'ok', blackhole_util:respond_with_error(Context)};
subscribe(Context, _JObj, Binding, Module) ->
    _ = blackhole_tracking:update_socket(Context),
    blackhole_util:maybe_add_binding_to_listener(Module, Binding, Context),
    blackhole_bindings:bind(Binding, Module, 'handle_event', Context),
    {'ok', Context}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec unsubscribe(bh_context:context(), wh_json:object(), ne_binary(), ne_binary()) -> cb_return().
-spec unsubscribe(bh_context:context(), wh_json:object(), ne_binary(), ne_binary(), boolean()) -> cb_return().
unsubscribe(Context, JObj, SessionPid, SessionId) ->
    Context1 = bh_context:from_json(Context, JObj),
    IsAuthorized = blackhole_util:is_authorized(Context1),
    unsubscribe(Context, JObj, SessionPid, SessionId, IsAuthorized).

unsubscribe(Context, _JObj, _SessionPid, _SessionId, 'false') ->
    {'ok', blackhole_util:respond_with_authn_failure(Context)};
unsubscribe(Context, JObj, SessionPid, SessionId, 'true') ->
    case wh_json:get_value(<<"account_id">>, JObj) of
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
-spec unsubscribe_for_all(bh_context:context(), wh_json:object(), ne_binary(), ne_binary()) -> cb_return().
unsubscribe_for_all(Context, JObj, SessionPid, SessionId) ->
    Context1 = bh_context:unsubscribe(Context, JObj),
    lager:debug("remove all bindings for session: ~p", [SessionId]),
    _ = blackhole_tracking:update_socket(Context1),
    Filter = fun (A, B, C, D) -> filter_bindings(SessionPid, A, B, C, D) end,
    blackhole_bindings:filter(Filter),
    {'ok', Context}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec unsubscribe_for_account(bh_context:context(), wh_json:object(), ne_binary()) -> cb_return().
-spec unsubscribe_for_account(bh_context:context(), wh_json:object(), ne_binary(), ne_binary(), api_binary()) -> cb_return().
unsubscribe_for_account(Context, JObj, AccountId) ->
    Binding = wh_json:get_value(<<"binding">>, JObj),
    Module = blackhole_util:get_callback_module(Binding),
    unsubscribe_for_account(Context, JObj, AccountId, Binding, Module).

unsubscribe_for_account(Context, _JObj, _AccountId, _Binding, 'undefined') ->
    {'ok', blackhole_util:respond_with_error(Context)};
unsubscribe_for_account(Context, JObj, AccountId, Binding, Module) ->
    Context1 = bh_context:unsubscribe(Context, JObj),
    _ = blackhole_tracking:update_socket(Context1),
    lager:debug("remove binding for account_id: ~p", [AccountId]),
    blackhole_bindings:unbind(Binding, Module, 'handle_event', Context),
    blackhole_util:maybe_rm_binding_from_listener(Module, Binding, Context),
    {'ok', Context}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
filter_bindings(SessionPid, Binding, _Module, _Function, Context) ->
    case bh_context:is_context(Context) of
        'false' -> 'true';
        'true' ->
            case bh_context:websocket_pid(Context) =:= SessionPid of
                'false' -> 'true';
                'true' ->
                    _ = wh_util:spawn('blackhole_util', 'remove_binding', [Binding, Context]),
                    'false'
            end
    end.
