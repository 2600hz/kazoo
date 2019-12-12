%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author Luis Azedo
%%% @end
%%%-----------------------------------------------------------------------------
-module(tf_offnet).

-include("doodle.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_im:im()) -> 'ok'.
handle(Data, Call) ->
    tf_resources:handle(kz_json:set_value(<<"use_local_resources">>, 'false', Data), Call).
