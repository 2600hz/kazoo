%%%-------------------------------------------------------------------
%%% @author Edouard Swiac <edouard@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created :  4 Aug 2011 by Edouard Swiac <edouard@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_hotdesking).

-include("../callflow.hrl").

-export([handle/2]).

-record(prompts, {
	  good_morning = <<"/system_media/ivr-good_morning">>
	 }).

-import(cf_call_command, [answer/1, play/2, b_play/2, say/3, tones/2, b_record/2
                          ,b_store/3, b_play_and_collect_digits/5, b_play_and_collect_digit/2
                          ,noop/1, b_flush/1, wait_for_dtmf/1, wait_for_application_or_dtmf/2
                          ,audio_macro/2, flush_dtmf/1
			 ]).

-record(hotdesking_profile , {
          hotdesking_id = undefined :: undefined | binary()
	  ,pin = undefined :: binary()
	  ,max_login_attempts = 3 :: non_neg_integer()
	  ,require_pin = false :: boolean()
	  ,check_if_owner = true :: boolean()
	  ,owner_id = <<>> :: binary()
          ,prompts = #prompts{}
	 }).
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> no_return()).
handle(Data, #cf_call{cf_pid=CFPid, call_id=CallId}=Call) ->
    put(callid, CallId),
    answer(Call),
    _ = flush_dtmf(Call),
    P = get_hotdesking_profile(Data, Call),
    ?LOG(" +++ ~p~n", [P]),
    b_play(P#prompts.good_morning, Call),
    CFPid ! {stop}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fetches the hotdesking parameters from the datastore and loads the
%% mailbox record
%% @end
%%--------------------------------------------------------------------
-spec(get_hotdesking_profile/2 :: (Data :: json_object(), Call :: #cf_call{}) -> #hotdesking_profile{}).
get_hotdesking_profile(Data, #cf_call{account_db=Db, request_user=ReqUser, last_action=LastAct}) ->
    Id = wh_json:get_value(<<"id">>, Data, <<"undefined">>),
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            ?LOG("loaded hotdesking profile ~s", [Id]),
            Default = #hotdesking_profile{},
            CheckIfOwner = ((undefined =:= LastAct) orelse (cf_device =:= LastAct)),
            #hotdesking_profile{hotdesking_id = Id
                     ,pin =
                         whistle_util:to_binary(wh_json:get_value(<<"pin">>, JObj, <<>>))
                     ,require_pin =
                         whistle_util:is_true(wh_json:get_value(<<"require_pin">>, JObj, false))
                     ,check_if_owner =
                         whistle_util:is_true(wh_json:get_value(<<"check_if_owner">>, JObj, CheckIfOwner))
                     ,owner_id =
                         wh_json:get_value(<<"owner_id">>, JObj)
                    };
        {error, R} ->
            ?LOG("failed to load hotdesking profile ~s, ~w", [Id, R]),
            #hotdesking_profile{}
    end.

