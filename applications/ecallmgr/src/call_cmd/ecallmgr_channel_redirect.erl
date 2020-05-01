%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_channel_redirect).

-export([redirect/2
        ,redirect_remote/2
        ]).

-include("ecallmgr.hrl").

-spec redirect(kz_term:ne_binary(), atom() | kz_term:ne_binary()) -> ecallmgr_util:send_cmd_ret().
redirect(UUID, DestinationNode) ->
    URL = ecallmgr_fs_node:sip_url(DestinationNode),
    case kapps_config:get_boolean(?APP_NAME, <<"redirect_via_proxy">>, 'true') of
        'true' -> redirect_via_proxy(URL, UUID);
        'false' -> redirect_via_endpoint(URL, UUID)
    end.

-spec redirect_remote(kz_term:ne_binary(), kz_json:object()) -> ecallmgr_util:send_cmd_ret().
redirect_remote(UUID, ChannelStatusJObj) ->
    URL = kz_json:get_value(<<"Switch-URL">>, ChannelStatusJObj),

    case kapps_config:get_boolean(?APP_NAME, <<"redirect_via_proxy">>, 'true') of
        'true' -> redirect_via_proxy(URL, UUID);
        'false' -> redirect_via_endpoint(URL, UUID)
    end.

-spec redirect_via_proxy(kz_term:ne_binary(), kz_term:ne_binary()) -> ecallmgr_util:send_cmd_ret().
redirect_via_proxy(DestNodeURL, UUID) ->
    {'ok', #channel{destination=ToUser
                   ,realm=Realm
                   }=Channel
    } = ecallmgr_fs_channel:fetch(UUID, 'record'),
    Contact = <<"sip:", ToUser/binary, "@", Realm/binary>>,

    RedirectURL = binary:replace(DestNodeURL, <<"mod_sofia@">>, <<>>),
    send_redirect(RedirectURL, Contact, Channel).

-spec redirect_via_endpoint(kz_term:ne_binary(), kz_term:ne_binary()) -> ecallmgr_util:send_cmd_ret().
redirect_via_endpoint(DestNodeURL, UUID) ->
    {'ok', #channel{destination=ToUser}=Channel} = ecallmgr_fs_channel:fetch(UUID, 'record'),
    Contact = binary:replace(DestNodeURL, <<"mod_sofia">>, ToUser),
    send_redirect('undefined', Contact, Channel).

-spec send_redirect(kz_term:api_binary(), kz_term:ne_binary(), channel()) ->
          ecallmgr_util:send_cmd_ret().
send_redirect('undefined', Contact, #channel{node=Node
                                            ,uuid=UUID
                                            ,answered=IsAnswered
                                            }) ->
    ecallmgr_util:send_cmd(Node, UUID, redirect_app(IsAnswered), Contact);
send_redirect(RedirectUrl, Contact, #channel{node=Node
                                            ,uuid=UUID
                                            ,answered=IsAnswered
                                            }) ->
    _ = ecallmgr_fs_command:set(Node, UUID, [{<<"sip_rh_X-Redirect-Server">>, RedirectUrl}]),
    ecallmgr_util:send_cmd(Node, UUID, redirect_app(IsAnswered), Contact).

-spec redirect_app(boolean()) -> kz_term:ne_binary().
redirect_app('true') -> <<"deflect">>;
redirect_app('false') -> <<"redirect">>.
