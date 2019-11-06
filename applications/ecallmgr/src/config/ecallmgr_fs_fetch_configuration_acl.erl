%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Send config commands to FS
%%%
%%% @author Edouard Swiac
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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
    _ = kazoo_bindings:bind(<<"fetch.configuration.*.*.acl.conf">>, ?MODULE, 'acl'),
    'ok'.

-spec acl(map()) -> fs_sendmsg_ret().
acl(#{node := Node, fetch_id := Id}=Ctx) ->
    kz_log:put_callid(Id),
    ACLs = ecallmgr_fs_acls:media_acls(),
    ConfigXML = generate_acl_xml(ACLs),
    lager:debug_unsafe("sending acl XML to ~s: ~s", [Node, ConfigXML]),
    freeswitch:fetch_reply(Ctx#{reply => ConfigXML}).

-spec generate_acl_xml(kz_json:object()) -> kz_term:ne_binary().
generate_acl_xml(SysconfResp) ->
    'false' = kz_json:is_empty(SysconfResp),
    {'ok', ConfigXml} = ecallmgr_fs_xml:acl_xml(SysconfResp),
    erlang:iolist_to_binary(ConfigXml).
