%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
%%% @doc
%%% Base module for callflow action
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(cf_skel).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(_Data, Call) ->
    %% Data is the "data" object from the JSON payload
    %% Call is the current kapps_call record

    %% Give control back to cf_exe process
    cf_exe:continue(Call).
