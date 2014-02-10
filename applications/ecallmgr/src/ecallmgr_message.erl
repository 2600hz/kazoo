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
    [User, Realm] = binary:split(To, <<"@">>),
    case  ecallmgr_registrar:lookup_contact(Realm,User) of
        {ok, Contact} ->
            Header = [
               {"profile", ?DEFAULT_FS_PROFILE}
              ,{"content-length", wh_util:to_list(size(Body))}
              ,{"content-type", "text/plain"}
              ,{"to", <<"sip:", To/binary>>}
              ,{"from", <<"sip:", From/binary>>}
              ,{"contact", Contact}
              ,{"body", Body}
            ],
            Resp = freeswitch:sendevent(Node, 'SEND_MESSAGE', Header),
            lager:debug("sent SIP/SIMPLE Msg to '~s' via ~s: ~p", [To, Node, Resp]);
        _Else ->
            lager:debug("drop SIP/SIMPLE Msg '~s' not registered", [To])
    end. 
