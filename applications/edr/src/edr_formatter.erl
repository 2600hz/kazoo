%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017-2018, Conversant Ltd
%%% @doc EDR output formatter.
%%% @author Max Lay
%%% @end
%%%-----------------------------------------------------------------------------
-module(edr_formatter).

-include("edr.hrl").

%%%-----------------------------------------------------------------------------
%%% Callbacks
%%%-----------------------------------------------------------------------------
-callback(format_event(Options :: kz_json:object(), Event :: edr_event()) -> kz_term:ne_binary()).
