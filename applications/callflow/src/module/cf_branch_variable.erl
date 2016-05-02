%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz INC
%%% @doc
%%%
%%% Try branch to variable's value
%%%
%%% "data":{
%%%   "variable":{{var_name}}
%%% }
%%%
%%% @end
%%% @contributors
%%%   KAZOO-3596: Sponsored by GTNetwork LLC, implemented by SIPLABS LLC
%%%-------------------------------------------------------------------
-module(cf_branch_variable).

-include("callflow.hrl").

-export([handle/2]).

-spec name_mapping() -> kz_proplist().
name_mapping() ->
    [{<<"call_priority">>, <<"Call-Priority">>}].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Name = props:get_value(kz_json:get_value(<<"variable">>, Data), name_mapping()),
    {'branch_keys', Keys} = cf_exe:get_branch_keys(Call),
    Value = kapps_call:custom_channel_var(Name, Call),
    case lists:member(Value, Keys) of
        'true' ->
            lager:info("trying '~s'", [Value]),
            cf_exe:continue(Value, Call);
        'false' ->
            lager:info("trying '_'"),
            cf_exe:continue(Call)
    end.
