%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc Send config commands to FS
%%%
%%% @author Edouard Swiac
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_fetch_configuration_acl).

%% API
-export([init/0]).

-export([acl/1]).

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
    kazoo_bindings:bind(<<"fetch.configuration.*.*.acl.conf">>, ?MODULE, 'acl'),
    'ok'.

-spec acl(map()) -> fs_sendmsg_ret().
acl(#{node := Node, fetch_id := Id}) ->
    kz_util:put_callid(Id),
    ACLs = ecallmgr_fs_acls:get(),
    ConfigXML = generate_acl_xml(ACLs),
    lager:debug_unsafe("sending acl XML to ~s: ~s", [Node, ConfigXML]),
    freeswitch:fetch_reply(Node, Id, 'configuration', ConfigXML).

-spec generate_acl_xml(kz_json:object()) -> kz_term:ne_binary().
generate_acl_xml(SysconfResp) ->
    'false' = kz_json:is_empty(SysconfResp),
    {'ok', ConfigXml} = ecallmgr_fs_xml:acl_xml(SysconfResp),
    erlang:iolist_to_binary(ConfigXml).
