%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
%%% @doc
%%% Send config commands to FS
%%% @end
%%% @contributors
%%%   Edouard Swiac
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_fetch_configuration_kazoo).

%% API
-export([init/0]).

-export([kazoo/1]).

-include("ecallmgr.hrl").


%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the bindings
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    kazoo_bindings:bind(<<"fetch.configuration.community.*.kazoo.conf">>, ?MODULE, 'kazoo'),
    'ok'.

-spec kazoo(map()) -> fs_sendmsg_ret().
kazoo(#{node := Node, fetch_id := Id, payload := JObj}) ->
    kz_util:put_callid(Id),
    lager:debug("received configuration request for kazoo configuration ~p , ~p", [Node, Id]),
    fetch_mod_kazoo_config(Node, Id, kz_api:event_name(JObj), JObj).


-spec fetch_mod_kazoo_config(atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> fs_sendmsg_ret().
fetch_mod_kazoo_config(Node, Id, <<"COMMAND">>, _JObj) ->
    lager:debug_unsafe("kazoo conf request : ~s", [kz_json:encode(_JObj, ['pretty'])]),
    config_req_not_handled(Node, Id, <<"kazoo.conf">>);
fetch_mod_kazoo_config(Node, Id, <<"REQUEST_PARAMS">>, JObj) ->
    lager:debug_unsafe("kazoo conf request params: ~s", [kz_json:encode(JObj, ['pretty'])]),
    Action = kz_json:get_ne_binary_value(<<"Action">>, JObj),
    fetch_mod_kazoo_config_action(Node, Id, Action, JObj);
fetch_mod_kazoo_config(Node, Id, Event, _JObj) ->
    lager:debug("unhandled mod kazoo config event : ~p : ~p", [Node, Event]),
    config_req_not_handled(Node, Id, <<"kazoo.conf">>).

-spec config_req_not_handled(atom(), kz_term:ne_binary(), kz_term:ne_binary()) -> fs_sendmsg_ret().
config_req_not_handled(Node, Id, Conf) ->
    {'ok', NotHandled} = ecallmgr_fs_xml:not_found(),
    lager:debug("ignoring conf ~s: ~s", [Conf, Id]),
    freeswitch:fetch_reply(Node, Id, 'configuration', iolist_to_binary(NotHandled)).

-spec fetch_mod_kazoo_config_action(atom(), kz_term:ne_binary(), kz_term:api_ne_binary(), kz_json:object()) ->
                                           fs_sendmsg_ret().
fetch_mod_kazoo_config_action(Node, Id, <<"request-filter">>, _Data) ->
    {'ok', Xml} = ecallmgr_fs_xml:event_filters_resp_xml(?FS_EVENT_FILTERS),
    lager:debug("replying with xml response for request-filter params request"),
    freeswitch:fetch_reply(Node, Id, 'configuration', iolist_to_binary(Xml));
fetch_mod_kazoo_config_action(Node, Id, <<"request-handlers">>, _Data) ->
    {'ok', Xml} = ecallmgr_fs_xml:event_filters_resp_xml(?FS_EVENT_FILTERS),
    lager:debug("replying with xml response for request-filter params request"),
    freeswitch:fetch_reply(Node, Id, 'configuration', iolist_to_binary(Xml));
fetch_mod_kazoo_config_action(Node, Id, 'undefined', _Data) ->
    config_req_not_handled(Node, Id, <<"kazoo.conf">>);
fetch_mod_kazoo_config_action(Node, Id, Action, _Data) ->
    lager:debug("unhandled mod kazoo config action : ~p : ~p", [Node, Action]),
    config_req_not_handled(Node, Id, <<"kazoo.conf">>).
