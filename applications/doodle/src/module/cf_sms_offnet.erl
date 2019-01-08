%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author Luis Azedo
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_sms_offnet).

-include("doodle.hrl").

-export([handle/2]).

-define(DEFAULT_EVENT_WAIT, 10 * ?MILLISECONDS_IN_SECOND).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    cf_sms_resources:handle(kz_json:set_value(<<"use_local_resources">>, 'false', Data), Call).
