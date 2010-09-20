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
process_flags(#route_flags{to_user = <<"911">>, e911=[], e911_default=[], callerid={}, callerid_default=Def}=Flags) ->
    Flags#route_flags{e911=Def};
process_flags(#route_flags{to_user = <<"911">>, e911=[], e911_default=[], callerid=Def}=Flags) ->
    Flags#route_flags{e911=Def};
process_flags(#route_flags{to_user = <<"911">>, e911=[], e911_default=Def}=Flags) ->
    Flags#route_flags{e911=Def};
process_flags(#route_flags{to_user = <<"911">>}=Flags) ->
    Flags;
%% Not a 911 Call
process_flags(Flags) ->
    Flags.
