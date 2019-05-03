%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Send config commands to FS
%%%
%%% @author Edouard Swiac
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_fetch_configuration_kazoo).

%% API
-export([init/0]).

-export([kazoo/1]).

-include("ecallmgr.hrl").


%%%=============================================================================
%%% API
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Initializes the bindings
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = kazoo_bindings:bind(<<"fetch.configuration.community.*.kazoo.conf">>, ?MODULE, 'kazoo'),
    'ok'.

-spec kazoo(map()) -> fs_sendmsg_ret().
kazoo(#{node := Node, fetch_id := Id, payload := JObj} = Ctx) ->
    kz_util:put_callid(Id),
    lager:debug("received configuration request for kazoo configuration ~p , ~p", [Node, Id]),
    fetch_mod_kazoo_config(kz_api:event_name(JObj), Ctx).


-spec fetch_mod_kazoo_config(kz_term:ne_binary(), map()) -> fs_sendmsg_ret().
fetch_mod_kazoo_config(<<"COMMAND">>, #{payload := _JObj} = Ctx) ->
    lager:debug_unsafe("kazoo conf request : ~s", [kz_json:encode(_JObj, ['pretty'])]),
    kazoo_req_not_handled(Ctx);
fetch_mod_kazoo_config(<<"REQUEST_PARAMS">>, #{payload := JObj} = Ctx) ->
    lager:debug_unsafe("kazoo conf request params: ~s", [kz_json:encode(JObj, ['pretty'])]),
    Action = kz_json:get_ne_binary_value(<<"Action">>, JObj),
    fetch_mod_kazoo_config_action(Action, Ctx);
fetch_mod_kazoo_config(Event, #{node := Node} = Ctx) ->
    lager:debug("unhandled mod kazoo config event : ~p : ~p", [Node, Event]),
    kazoo_req_not_handled(Ctx).

-spec fetch_mod_kazoo_config_action(kz_term:api_ne_binary(), map()) -> fs_sendmsg_ret().
fetch_mod_kazoo_config_action(<<"request-handlers">>, Ctx) ->
    %% TODO get handlers/definitions/events
    kazoo_req_not_handled(Ctx);
fetch_mod_kazoo_config_action('undefined', Ctx) ->
    kazoo_req_not_handled(Ctx);
fetch_mod_kazoo_config_action(Action, #{node := Node} = Ctx) ->
    lager:debug("unhandled mod kazoo config action : ~p : ~p", [Node, Action]),
    kazoo_req_not_handled(Ctx).

-spec kazoo_req_not_handled(map()) -> fs_sendmsg_ret().
kazoo_req_not_handled(#{node := Node, fetch_id := Id} = Ctx) ->
    {'ok', NotHandled} = ecallmgr_fs_xml:not_found(),
    lager:debug("ignoring kazoo conf ~s: ~s", [Node, Id]),
    freeswitch:fetch_reply(Ctx#{reply => iolist_to_binary(NotHandled)}).

