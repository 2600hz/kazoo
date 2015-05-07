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
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cf_branch_variable).

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
    Name = props:get_value(wh_json:get_value(<<"variable">>, Data), name_mapping()),
    {'branch_keys', Keys} = cf_exe:get_branch_keys(Call),
    Value = whapps_call:custom_channel_var(Name, Call),
    case lists:member(Value, Keys) of
        'true' ->
            lager:info("Trying  '~s'", [Value]),
            cf_exe:continue(Value, Call);
        'false' ->
            lager:info("Trying '_'"),
            cf_exe:continue(Call)
    end.
