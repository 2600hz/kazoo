%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_channel_redirect).

-export([redirect/2]).

-include("ecallmgr.hrl").

-spec redirect(ne_binary(), atom() | ne_binary()) -> ecallmgr_util:send_cmd_ret().
redirect(UUID, DestinationNode) ->
    DestNodeURL = ecallmgr_fs_node:sip_url(DestinationNode),
    case ecallmgr_config:get_boolean(<<"redirect_via_proxy">>, 'true') of
        'true' -> redirect_via_proxy(DestNodeURL, UUID);
        'false' -> redirect_via_endpoint(DestNodeURL, UUID)
    end.

redirect_via_proxy(DestNodeURL, UUID) ->
    {'ok', #channel{destination=ToUser
                    ,realm=Realm
                   }=Channel} = ecallmgr_fs_channel:fetch(UUID, 'record'),
    Contact = <<"sip:", ToUser/binary, "@", Realm/binary>>,
    RedirectURL = binary:replace(DestNodeURL, <<"mod_sofia@">>, <<>>),
    send_redirect(RedirectURL, Contact, Channel).

redirect_via_endpoint(DestNodeURL, UUID) ->
    {'ok', #channel{destination=ToUser}=Channel} = ecallmgr_fs_channel:fetch(UUID, 'record'),
    Contact = binary:replace(DestNodeURL, <<"mod_sofia">>, ToUser),
    send_redirect('undefined', Contact, Channel).

send_redirect('undefined', Contact, #channel{node=Node
                                            ,uuid=UUID
                                            }) ->
    ecallmgr_util:send_cmd(Node, UUID, <<"redirect">>, Contact);
send_redirect(RedirectUrl, Contact, #channel{node=Node
                                             ,uuid=UUID
                                            }) ->
    ecallmgr_util:set(Node, UUID, [{<<"sip_rh_X-Redirect-Server">>, RedirectUrl}]),
    ecallmgr_util:send_cmd(Node, UUID, <<"redirect">>, Contact).
