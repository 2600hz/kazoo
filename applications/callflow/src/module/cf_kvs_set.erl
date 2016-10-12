%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
-module(cf_kvs_set).

-export([handle/2]).

-include("../callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Call1 = kapps_call:set_custom_kvs(Data, Call),
    cf_exe:set_call(Call1),
    cf_exe:continue(Call1).

