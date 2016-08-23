-module(bh_cmd).
-export([init/0, subscribe/2, unsubscribe/2, authenticate/2]).
-include("blackhole.hrl").

init() ->
    blackhole_bindings:bind(<<"command.authenticate">>, ?MODULE, 'authenticate'),
    blackhole_bindings:bind(<<"command.subscribe">>, ?MODULE, 'subscribe'),
    blackhole_bindings:bind(<<"command.unsubscribe">>, ?MODULE, 'unsubscribe').

subscribe(Context=#bh_context{websocket_pid=WsPid}, JMsg) ->
    Binding = kz_json:get_value(<<"binding">>, JMsg),
    case bh_context:is_bound(Context, Binding) of
        'true' ->
            blackhole_util:send_error(WsPid, <<"already subscribed">>, Binding),
            Context;
        'false' ->
            execute(<<"subscribe">>, Context, JMsg),
            bh_context:add_binding(Context, Binding)
    end.
unsubscribe(Context=#bh_context{websocket_pid=WsPid}, JMsg) ->
    Binding = kz_json:get_value(<<"binding">>, JMsg),
    case bh_context:is_bound(Context, Binding) of
        'true' ->
            execute(<<"unsubscribe">>, Context, JMsg),
            bh_context:remove_binding(Context, Binding);
        'false' -> 
            blackhole_util:send_error(WsPid, <<"not subscribed">>, Binding),
            Context
    end.

execute(Cmd, Context=#bh_context{}, JMsg) ->
    Binding = blackhole_util:ensure_value(kz_json:get_value(<<"binding">>, JMsg), 'missing_parameter'),
    [Module|Args] = binary:split(Binding, <<".">>, ['global']),
    Ctx1 = blackhole_bindings:fold(<<"command.", Cmd/binary, ".", Module/binary, ".validate">>, [Context, JMsg | Args]),
    _Ctx2 = blackhole_bindings:fold(<<"command.", Cmd/binary, ".", Module/binary, ".execute">>, [Ctx1, Cmd | Args]),
    Context.

authenticate(Context=#bh_context{}, JMsg) ->
    Token = kz_json:get_value(<<"token">>, JMsg),
    AuthAccountId = get_account_id(Token),
    lager:debug("auth_token:~p found, auth_account_id:~p", [Token, AuthAccountId]),
    Context#bh_context{auth_account_id=AuthAccountId, auth_token=Token}.

-spec get_account_id(ne_binary()) -> ne_binary().
get_account_id(AuthToken) ->
    {'ok', JObj} = kz_datamgr:open_doc(?KZ_TOKEN_DB, AuthToken),
    kz_json:get_ne_value(<<"account_id">>, JObj).