%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_intercom).

-include("callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    CaptureGroup = kapps_call:kvs_fetch('cf_capture_group', Call),
    AccountId = kapps_call:account_id(Call),
    case is_binary(CaptureGroup) andalso cf_util:lookup_callflow(CaptureGroup, AccountId) of
        {'ok', Flow, 'false'} ->
            JObj = case kz_json:is_true(<<"barge_calls">>, Data) of
                       'false' -> kz_json:from_list([{<<"Auto-Answer-Suppress-Notify">>, <<"true">>}]);
                       'true' -> kz_json:from_list([{<<"Auto-Answer-Suppress-Notify">>, <<"false">>}])
                   end,
            kapps_call_command:set('undefined', JObj, Call),
            cf_exe:branch(kz_json:get_value(<<"flow">>, Flow, kz_json:new()), Call);
        _ -> cf_exe:continue(Call)
    end.
