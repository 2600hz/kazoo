-module(bh_cmd).
-export([init/0, subscribe/3, authenticate/3]).
-include("blackhole.hrl").

init() ->
    blackhole_bindings:bind(<<"command.authenticate">>, ?MODULE, 'authenticate'),
    blackhole_bindings:bind(<<"command.subscribe">>, ?MODULE, 'subscribe'),
    blackhole_bindings:bind(<<"command.unsubscribe">>, ?MODULE, 'subscribe').

subscribe(Context=#bh_context{websocket_pid=WsPid}, Cmd, JMsg) ->
    Binding = kz_json:get_value(<<"binding">>, JMsg),
    case {Cmd, bh_context:is_bound(Context, Binding)} of
        {<<"subscribe">>, 'true'} ->
            blackhole_util:send_error(WsPid, <<"already subscribed">>, Binding),
            Context;
        {<<"subscribe">>, 'false'} ->
            manage_context(Cmd, execute(Cmd, Context, JMsg), Binding);
        {<<"unsubscribe">>, 'true'} ->
            manage_context(Cmd, execute(Cmd, Context, JMsg), Binding);
        {<<"unsubscribe">>, 'false'} ->
            blackhole_util:send_error(WsPid, <<"not subscribed">>, Binding),
            Context
    end.

execute(Cmd, Context=#bh_context{}, JMsg) ->
    Binding = blackhole_util:ensure_value(kz_json:get_value(<<"binding">>, JMsg), 'missing_parameter'),
    [Module|Args] = binary:split(Binding, <<".">>, ['global']),
    Ctx1 = blackhole_bindings:fold(<<"command.", Cmd/binary, ".", Module/binary, ".validate">>, [Context, JMsg | Args]),
    _Ctx2 = blackhole_bindings:fold(<<"command.", Cmd/binary, ".", Module/binary, ".execute">>, [Ctx1, Cmd | Args]),
    Context.

authenticate(Context=#bh_context{}, <<"authenticate">>, JMsg) ->
    Token = kz_json:get_value(<<"token">>, JMsg),
    AuthAccountId = get_account_id(Token),
    lager:debug("auth_token:~p found, auth_account_id:~p", [Token, AuthAccountId]),
    Context#bh_context{auth_account_id=AuthAccountId, auth_token=Token};
authenticate(_, _, _) ->
    {'error', 'unhandled_authenticate'}.

manage_context(<<"subscribe">>, Context, Binding) -> bh_context:add_binding(Context, Binding);
manage_context(<<"unsubscribe">>, Context, Binding) -> bh_context:remove_binding(Context, Binding).

-spec get_account_id(ne_binary()) -> ne_binary().
get_account_id(AuthToken) ->
    {'ok', JObj} = kz_datamgr:open_doc(?KZ_TOKEN_DB, AuthToken),
    kz_json:get_ne_value(<<"account_id">>, JObj).