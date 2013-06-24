%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% E911 processing
%%% @end
%%% Created : 20 Sep 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------

-module(ts_e911).

-export([process_flags/1]).

-include("ts.hrl").

-spec(process_flags(Flags :: tuple()) -> tuple()).
process_flags(#route_flags{to_user = <<"911">>, caller_id_e911={}}=Flags) ->
    Flags;
process_flags(#route_flags{to_user = <<"911">>, caller_id_e911={_,_}=E911}=Flags) ->
    Flags#route_flags{caller_id=E911};
%% Not a 911 Call
process_flags(Flags) ->
    Flags.
