%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
%%% @doc
%%% Send config commands to FS
%%% @end
%%% @contributors
%%%   Edouard Swiac
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_fetch_configuration_acl).

%% API
-export([init/0]).

-export([acl/1]).

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
    kazoo_bindings:bind(<<"fetch.configuration.configuration.name.acl.conf">>, ?MODULE, 'acl'),
    'ok'.

-spec acl(tuple()) -> fs_sendmsg_ret().
acl(#{node := Node, fetch_id := Id}) ->
    kz_util:put_callid(Id),

    SysconfResp = ecallmgr_config:fetch(<<"acls">>, kz_json:new(), ecallmgr_fs_node:fetch_timeout(Node)),

    try generate_acl_xml(SysconfResp) of
        'undefined' ->
            lager:warning("failed to query for ACLs; is sysconf running?"),
            {'ok', Resp} = ecallmgr_fs_xml:not_found(),
            freeswitch:fetch_reply(Node, Id, 'configuration', Resp);
        ConfigXml ->
            lager:debug("sending acl XML to ~s: ~s", [Node, ConfigXml]),
            freeswitch:fetch_reply(Node, Id, 'configuration', ConfigXml)
    catch
        _E:_R ->
            lager:info("acl resp failed to convert to XML (~s): ~p", [_E, _R]),
            {'ok', Resp} = ecallmgr_fs_xml:not_found(),
            freeswitch:fetch_reply(Node, Id, 'configuration', iolist_to_binary(Resp))
    end.


-spec generate_acl_xml(api_object()) -> api_binary().
generate_acl_xml('undefined') ->
    'undefined';
generate_acl_xml(SysconfResp) ->
    'false' = kz_json:is_empty(SysconfResp),
    {'ok', ConfigXml} = ecallmgr_fs_xml:acl_xml(SysconfResp),
    erlang:iolist_to_binary(ConfigXml).
