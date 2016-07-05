%%%-------------------------------------------------------------------
%%% @copyright
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_srs_null).

-export([handle_req/5]).

-include("stepswitch.hrl").

-spec handle_req(stepswitch_resources:resources(), ne_binary(), kapi_offnet_resource:req(), ne_binary(), kz_json:object()) ->
			stepswitch_resources:resources().
handle_req(_Resources, _Number, _OffnetJObj, _Db, _Params) ->
    [].
