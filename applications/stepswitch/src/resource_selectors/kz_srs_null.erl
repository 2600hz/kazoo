%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_srs_null).

-export([handle_req/5]).

-include("stepswitch.hrl").

-spec handle_req(stepswitch_resources:resources(), kz_term:ne_binary(), kapi_offnet_resource:req(), kz_term:ne_binary(), kz_json:object()) ->
                        stepswitch_resources:resources().
handle_req(_Resources, _Number, _OffnetJObj, _Db, _Params) ->
    [].
