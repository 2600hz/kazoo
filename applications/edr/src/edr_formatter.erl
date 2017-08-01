%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Conversant Ltd
%%% @doc
%%% EDR output formatter
%%% @end
%%% @contributors
%%%    Max Lay
%%%-------------------------------------------------------------------
-module(edr_formatter).

-include("edr.hrl").

%%%-------------------------------------------------------------------
%%% Callbacks
%%%-------------------------------------------------------------------
-callback(format_event(Options :: kz_json:object(), Event :: event()) -> ne_binary()).
