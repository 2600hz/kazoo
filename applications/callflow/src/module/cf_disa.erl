%%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_disa).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    lager:info("starting DISA handler"),
    whapps_call_command:answer(Call),
    Pin = wh_json:get_value(<<"pin">>, Data),
    Retries = wh_json:get_integer_value(<<"retries">>, Data, 3),
    case try_collect_pin(Call, Pin, Retries) of
        'allow' -> allow_dial(Data, Call, Retries);
        _ -> cf_exe:stop(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec try_collect_pin(whapps_call:call(), binary(), non_neg_integer()) -> 'allow' | 'fail'.
try_collect_pin(_Call, <<>>, _) ->
    lager:info("no pin set on DISA object, permitting"),
    'allow';
try_collect_pin(Call, _, 0) ->
    lager:info("retries for DISA pin exceeded"),
    _ = whapps_call_command:b_prompt(<<"disa-retries_exceeded">>, Call),
    'fail';
try_collect_pin(Call, Pin, Retries) ->
    Prompt = <<"disa-enter_pin">>,
    case whapps_call_command:b_prompt_and_collect_digits(<<"1">>, <<"6">>, Prompt, Call) of
        {'ok', Pin} ->
            lager:info("pin matches, permitting"),
            'allow';
        {'ok', _Digits} ->
            lager:info("caller entered ~s for pin", [_Digits]),
            _ = whapps_call_command:b_prompt(<<"disa-invalid_pin">>, Call),
            try_collect_pin(Call, Pin, Retries - 1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec allow_dial(wh_json:object(), whapps_call:call(), non_neg_integer()) -> 'ok'.
allow_dial(_, Call, 0) ->
    lager:info("retries exceeded for finding a callflow"),
    cf_exe:continue(Call);
allow_dial(Data, Call, Retries) ->
    case wh_json:get_ne_value(<<"preconnect_audio">>, Data, <<"dialtone">>) of
        <<"dialtone">> ->
            lager:debug("playing dialtone..."),
            play_dialtone(Call);
        <<"ringing">> ->
            lager:debug("playing ringing..."),
            play_ringing(Data, Call);
        _Else -> lager:debug("unknown preconnect audio type: ~p", [_Else])
    end,
    {'ok', Digits} = whapps_call_command:collect_digits(15, Call),
    Number = wnm_util:to_e164(Digits),
    lager:info("caller is trying to call ~s", [Number]),
    case wh_json:is_true(<<"use_account_caller_id">>, Data, 'false') of
        'true' -> set_caller_id(Call);
        'false' -> 'ok'
    end,
    case cf_util:lookup_callflow(Number, whapps_call:account_id(Call)) of
        {'ok', Flow, NoMatch} ->
            lager:info("callflow ~s satisfies request", [wh_json:get_value(<<"_id">>, Flow)]),
            Updates = [fun(C) -> whapps_call:set_request(list_to_binary([Number, "@", whapps_call:request_realm(C)]), C) end
                       ,fun(C) -> whapps_call:set_to(list_to_binary([Number, "@", whapps_call:to_realm(C)]), C) end
                       ,fun(C) when NoMatch ->
                                {CIDNum, CIDName} = cf_attributes:caller_id(<<"external">>, C),
                                C1 = whapps_call:set_caller_id_number(CIDNum, C),
                                whapps_call:set_caller_id_name(CIDName, C1);
                           (C) -> C
                        end
                      ],
            {'ok', C} = cf_exe:get_call(Call),
            cf_exe:set_call(whapps_call:exec(Updates, C)),
            cf_exe:branch(wh_json:get_value(<<"flow">>, Flow), Call);
        _ ->
            lager:info("failed to find a callflow to satisfy ~s", [Number]),
            _ = whapps_call_command:b_prompt(<<"disa-invalid_extension">>, Call),
            allow_dial(Data, Call, Retries - 1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec play_dialtone(whapps_call:call()) -> 'ok'.
play_dialtone(Call) ->
    Tone = wh_json:from_list([{<<"Frequencies">>, [<<"350">>, <<"440">>]}
                              ,{<<"Duration-ON">>, <<"10000">>}
                              ,{<<"Duration-OFF">>, <<"0">>}
                              ]),
    whapps_call_command:tones([Tone], Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec play_ringing(wh_json:object(), whapps_call:call()) -> 'ok'.
play_ringing(Data, Call) ->
    RingRepeatCount = wh_json:get_integer_value(<<"ring_repeat_count">>, Data, 1),
    Tone = wh_json:from_list([{<<"Frequencies">>, [<<"440">>, <<"480">>]}
                              ,{<<"Duration-ON">>, <<"2000">>}
                              ,{<<"Duration-OFF">>, <<"4000">>}
                              ,{<<"Repeat">>, RingRepeatCount}
                             ]),
    whapps_call_command:tones([Tone], Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_caller_id(whapps_call:call()) -> 'ok'.
set_caller_id(Call) ->
    CIDNum = whapps_call:caller_id_number(Call),
    case wh_number_manager:lookup_account_by_number(CIDNum) of
        {'ok', AccountId, _} ->
          {Number, Name} = maybe_get_account_cid(AccountId, Call),
          lager:info("setting the caller id number to ~s from account ~s", [Number, AccountId]),
          Updates = [fun(C) -> whapps_call:kvs_store('dynamic_cid', Number, C) end
                     ,fun(C) ->
                        C1 = whapps_call:set_caller_id_number(Number, C),
                        whapps_call:set_caller_id_name(Name, C1)
                      end
                    ],
          {'ok', C} = cf_exe:get_call(Call),
          cf_exe:set_call(whapps_call:exec(Updates, C)),
          'ok';
        _Else ->
          lager:debug("~s is not associated with any account, ~s", [CIDNum]),
          'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_get_account_cid(ne_binary(), whapps_call:call()) ->
                                   {api_binary(), api_binary()}.
maybe_get_account_cid(AccountId, Call) ->
    Name = whapps_call:caller_id_name(Call),
    Number = whapps_call:caller_id_number(Call),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'error', _} -> cf_attributes:maybe_get_assigned_number(Number, Name, Call);
        {'ok', JObj} ->
            maybe_get_account_external_number(Number, Name, JObj, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_get_account_external_number(ne_binary(), ne_binary(), wh_json:object(), whapps_call:call()) ->
                                               {api_binary(), api_binary()}.
maybe_get_account_external_number(Number, Name, Account, Call) ->
    External = wh_json:get_ne_value([<<"caller_id">>, <<"external">>, <<"number">>], Account),
    case is_valid_caller_id(External, Call) of
        'true' ->
            lager:info("valid account external caller id <~s> ~s", [Name, Number]),
            {External, Name};
        'false' ->
            cf_attributes:maybe_get_account_default_number(Number, Name, Account, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_valid_caller_id(api_binary(), whapps_call:call()) -> boolean().
is_valid_caller_id('undefined', _) -> 'false';
is_valid_caller_id(_, _) -> 'true'.
