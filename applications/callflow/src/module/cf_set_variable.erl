%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz INC
%%% @doc
%%%
%%% "data":{
%%%   "variable":{{var_name}},
%%%   "value:{{value}},
%%%   "channel": "a", "both"
%%% }
%%%
%%% @end
%%% @contributors
%%%   KAZOO-3596: Sponsored by GTNetwork LLC, implemented by SIPLABS LLC
%%%-------------------------------------------------------------------
-module(cf_set_variable).

-include("../callflow.hrl").

-export([handle/2]).

-spec name_mapping() -> wh_proplist().
name_mapping() ->
    [{<<"call_priority">>, <<"Call-Priority">>}].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Value = wh_json:get_binary_value(<<"value">>, Data),
    Name = props:get_value(wh_json:get_value(<<"variable">>, Data), name_mapping()),
    Channel = wh_json:get_value(<<"channel">>, Data, <<"a">>),
    set_variable(Name, Value, Channel, Call),
    Call1 = whapps_call:insert_custom_channel_var(Name, Value, Call),
    cf_exe:set_call(Call1),
    cf_exe:continue(Call1).

-spec set_variable(api_binary(), api_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
set_variable('undefined', _Value, _Channel, _Call) ->
    lager:warning("Cant set variable w/o name!");
set_variable(_Name, 'undefined', _Channel, _Call) ->
    lager:warning("Cant set variable w/o value!");
set_variable(Name, Value, Channel, Call) ->
    lager:debug("Set ~s/~s pair on ~s-leg", [Name, Value, Channel]),
    Var = wh_json:from_list([{Name, Value}]),
    execute_set_var(Var, Channel, Call).

-spec execute_set_var(wh_json:object(), ne_binary(), whapps_call:call()) -> 'ok'.
execute_set_var(Var, <<"a">>, Call) ->
    whapps_call_command:set(Var, 'undefined', Call);
execute_set_var(Var, <<"both">>, Call) ->
    whapps_call_command:set('undefined', Var, Call).
