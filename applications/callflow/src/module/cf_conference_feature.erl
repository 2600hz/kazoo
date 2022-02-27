%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2022, 2600Hz
%%% @doc Get conference ID from capture group and put caller into conference
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_conference_feature).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module, creates the parameters and branches
%% to cf_group_pickup.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Id = kapps_call:kvs_fetch('cf_capture_group', Call),
    lager:info("routing to captured conference ~s", [Id]),
    cf_conference:handle(kz_doc:set_id(Data, Id), Call).
