%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Alan R Evans
%%%-------------------------------------------------------------------
-module(ecallmgr_message).

-export([process_msg/2
        ]).

-include_lib("ecallmgr.hrl").

process_msg(Props, Node) ->
    lager:debug("process_message received SIP/SIMPLE Msg"),
    From = props:get_value(<<"from">>, Props),
    To = props:get_value(<<"to">>, Props),
    Body = props:get_value(<<"body">>, Props),
    Header = [
           {"profile", ?DEFAULT_FS_PROFILE}
          ,{"content-length", wh_util:to_list(size(Body))}
          ,{"content-type", "text/plain"}
          ,{"to", To}
          ,{"from", From}
          ,{"body", Body}
         ],
    Resp = freeswitch:sendevent(Node, 'SEND_MESSAGE', Header),
    lager:debug("sent SIP/SIMPLE Msg to '~s' via ~s: ~p", [To, Node, Resp]).
