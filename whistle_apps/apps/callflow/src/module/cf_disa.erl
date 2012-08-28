%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
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
-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    lager:debug("starting DISA handler"),
    whapps_call_command:answer(Call),
    Pin = wh_json:get_value(<<"pin">>, Data),
    Retries = wh_json:get_integer_value(<<"retries">>, Data, 3),
    case try_collect_pin(Call, Pin, Retries) of
        allow -> allow_dial(Call, Retries);
        _ -> cf_exe:stop(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec try_collect_pin/3 :: (whapps_call:call(), binary(), non_neg_integer()) -> allow | fail.
try_collect_pin(_Call, <<>>, _) ->
    lager:debug("no pin set on DISA object, permitting"),
    allow;
try_collect_pin(Call, _, 0) ->
    lager:debug("retries for DISA pin exceeded"),
    _ = whapps_call_command:b_prompt(<<"disa-retries_exceeded">>, Call),
    fail;
try_collect_pin(Call, Pin, Retries) ->
    Prompt = <<"disa-enter_pin">>,
    case whapps_call_command:b_prompt_and_collect_digits(<<"1">>, <<"6">>, Prompt, Call) of
        {ok, Pin} ->
            lager:debug("pin matches, permitting"),
            allow;
        {ok, _Digits} ->
            lager:debug("caller entered ~s for pin", [_Digits]),
            _ = whapps_call_command:b_prompt(<<"disa-invalid_pin">>, Call),
            try_collect_pin(Call, Pin, Retries - 1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec allow_dial/2 :: (whapps_call:call(), non_neg_integer()) -> ok.
allow_dial(Call, 0) ->
    lager:debug("retries exceeded for finding a callflow"),
    cf_exe:continue(Call);
allow_dial(Call, Retries) ->
    _ = play_dialtone(Call),
    {ok, Digits} = whapps_call_command:collect_digits(15, Call),
    Number = wnm_util:to_e164(Digits),
    lager:debug("caller is trying to call ~s", [Number]),
    case cf_util:lookup_callflow(Number, whapps_call:account_id(Call)) of
        {ok, Flow, NoMatch} ->
            lager:debug("callflow ~s satisfies request", [wh_json:get_value(<<"_id">>, Flow)]),
            Updates = [fun(C) -> whapps_call:set_request(list_to_binary([Number, "@", whapps_call:request_realm(C)]), C) end
                       ,fun(C) -> whapps_call:set_to(list_to_binary([Number, "@", whapps_call:to_realm(C)]), C) end
                       ,fun(C) when NoMatch -> 
                                {CIDNum, CIDName} = cf_attributes:caller_id(<<"external">>, C),
                                C1 = whapps_call:set_caller_id_number(CIDNum, C),
                                whapps_call:set_caller_id_name(CIDName, C1);
                           (C) -> C
                        end
                      ],
            {ok, C} = cf_exe:get_call(Call),
            cf_exe:set_call(whapps_call:exec(Updates, C)),
            cf_exe:branch(wh_json:get_value(<<"flow">>, Flow), Call);
        _ ->
            lager:debug("failed to find a callflow to satisfy ~s", [Number]),
            _ = whapps_call_command:b_prompt(<<"disa-invalid_extension">>, Call),
            allow_dial(Call, Retries - 1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec play_dialtone/1 :: (whapps_call:call()) -> ok.
play_dialtone(Call) ->
    Tone = wh_json:from_list([{<<"Frequencies">>, [<<"350">>, <<"440">>]}
                              ,{<<"Duration-ON">>, <<"10000">>}
                              ,{<<"Duration-OFF">>, <<"0">>}
                              ]),
    whapps_call_command:tones([Tone], Call).
