%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created :  5 Apr 2011 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(ts_tollfree).

-export([process_flags/1]).

-include("ts.hrl").

-define(REGEX, begin {ok, M} = re:compile("^\\+18(88|77|66|55|00)\\d{7}$"), M end).

-spec(process_flags(Flags :: tuple()) -> tuple()).
process_flags(#route_flags{direction = <<"inbound">>, to_user = From}=Flags) ->
    case re:run(From, ?REGEX) of
	nomatch -> Flags;
	{match, _} -> Flags#route_flags{flat_rate_enabled=false}
    end;
process_flags(Flags) ->
    Flags.
