%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(cf_sms_offnet).

-include("doodle.hrl").

-export([handle/2]).

-define(DEFAULT_EVENT_WAIT, 10 * ?MILLISECONDS_IN_SECOND).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    cf_sms_resources:handle(kz_json:set_value(<<"use_local_resources">>, 'false', Data), Call).
