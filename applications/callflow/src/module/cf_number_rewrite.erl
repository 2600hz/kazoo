%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% "data":{
%%%   "number": "+1844xxxxxxx"
%%% }
%%% @end
%%%-------------------------------------------------------------------
-module(cf_number_rewrite).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get the new number that needs to be changed from the data section
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> any().
handle(Data, Call) ->
    Number =  kz_json:get_binary_value(<<"number">>, Data),

    lager:debug("updating called number to ~s", [Number]),

    Updates = [fun(C) -> set_request(C, Number) end
              ,fun(C) -> set_callee_number(C, Number) end
              ],

    {'ok', Call1} = cf_exe:get_call(Call),
    cf_exe:set_call(kapps_call:exec(Updates, Call1)),
    cf_exe:continue(Call1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% modify the number called
%% @end
%%--------------------------------------------------------------------
-spec set_request(kapps_call:call(), ne_binary()) -> kapps_call:call().
set_request(Call, Number) ->
    Realm = kapps_call:request_realm(Call),
    Request = <<Number/binary, "@", Realm/binary>>,
    kapps_call:set_request(Request, Call).

-spec set_callee_number(kapps_call:call(), ne_binary()) -> kapps_call:call().
set_callee_number(Call, Number) ->
    kapps_call:set_callee_id_number(Number, Call).
