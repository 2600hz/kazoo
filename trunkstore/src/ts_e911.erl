%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% E911 processing
%%% @end
%%% Created : 20 Sep 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------

-module(ts_e911).

-export([process_flags/1]).

-include("ts.hrl").

-spec(process_flags/1 :: (Flags :: tuple()) -> tuple()).

%% No E911 information set for this DID
%%  - Resort to Server-wide E911
%%  - Resort to DID CallerID
%%  - Resort to Server CallerID
process_flags(#route_flags{to_user = <<"911">>, callerid_e911={}}=Flags) ->
    Flags;
process_flags(#route_flags{to_user = <<"911">>, callerid_e911=E911}=Flags) ->
    Flags#route_flags{callerid=E911};
%% Not a 911 Call
process_flags(Flags) ->
    Flags.
