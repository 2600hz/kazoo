%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Allow caller to use the account resource to call out.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`pin'</dt>
%%%   <dd><strong>Optional: </strong>PIN code to allow caller use this feature.</dd>
%%%
%%%   <dt>`max_digits'</dt>
%%%   <dd>Maximum digits allowed when collecting destination number. Default is 15 digits.</dd>
%%%
%%%   <dt>`retries'</dt>
%%%   <dd><strong>Optional: </strong>Maximum number of retries to collect PIN and/or destination number. Default is 3.</dd>
%%%
%%%   <dt>`interdigit'</dt>
%%%   <dd><strong>Optional: </strong>How long to wait for the next DTMF, in milliseconds</dd>
%%% </dl>
%%%
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_disa).

%% some recursion causes loops in cf_data_usage
%% -behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

-define(DEFAULT_USE_ACCOUNT_CALLER_ID, kapps_config:get_is_true(?CF_CONFIG_CAT, <<"default_use_account_caller_id">>, 'true')).
-define(DEFAULT_PIN_LENGTH, kapps_config:get_integer(?CF_CONFIG_CAT, <<"default_pin_length">>, 10)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    lager:info("starting DISA handler"),
    kapps_call_command:answer(Call),

    Pin = kz_json:get_value(<<"pin">>, Data, <<>>),
    Retries = kz_json:get_integer_value(<<"retries">>, Data, 3),
    Interdigit = kz_json:get_integer_value(<<"interdigit">>, Data, kapps_call_command:default_interdigit_timeout()),

    case try_collect_pin(Call, Pin, Retries, Interdigit) of
        'allow' -> allow_dial(Data, Call, Retries, Interdigit);
        'fail' -> cf_exe:stop(Call)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec try_collect_pin(kapps_call:call(), binary(), non_neg_integer(), pos_integer()) -> 'allow' | 'fail'.
try_collect_pin(_Call, <<>>, _Retries, _Interdigit) ->
    lager:warning("no pin set on DISA object, permitting"),
    'allow';
try_collect_pin(Call, _Pin, 0, _Interdigit) ->
    lager:info("retries for DISA pin exceeded"),
    _ = kapps_call_command:b_prompt(<<"disa-retries_exceeded">>, Call),
    'fail';
try_collect_pin(Call, Pin, Retries, Interdigit) ->
    Prompt = <<"disa-enter_pin">>,
    NoopId = kapps_call_command:prompt(Prompt, Call),
    PinLength = erlang:max(?DEFAULT_PIN_LENGTH, byte_size(Pin)),

    lager:debug("collecting up to ~p digits for pin", [PinLength]),
    case kapps_call_command:collect_digits(PinLength
                                          ,kapps_call_command:default_collect_timeout()
                                          ,Interdigit
                                          ,NoopId
                                          ,Call
                                          )
    of
        {'ok', Pin} ->
            lager:info("pin matches, permitting"),
            'allow';
        {'ok', _Digits} ->
            lager:info("caller entered bad pin: '~s'", [_Digits]),
            _ = kapps_call_command:b_prompt(<<"disa-invalid_pin">>, Call),
            try_collect_pin(Call, Pin, Retries - 1, Interdigit);
        {'error', 'channel_hungup'} ->
            lager:info("channel has hungup, we're done"),
            'fail'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec allow_dial(kz_json:object(), kapps_call:call(), non_neg_integer(), pos_integer()) -> 'ok'.
allow_dial(_, Call, 0, _Interdigit) ->
    lager:info("retries exceeded for finding a callflow"),
    cf_exe:continue(Call);
allow_dial(Data, Call, Retries, Interdigit) ->
    _ = start_preconnect_audio(Data, Call),
    Number = collect_destination_number(Call, Data, Interdigit),

    lager:info("caller is trying to call '~s'", [Number]),

    Call1 = maybe_update_caller_id(Call, should_use_account_cid(Data)),
    maybe_route_to_callflow(Data, Call1, Retries, Interdigit, Number).

maybe_route_to_callflow(Data, Call, Retries, Interdigit, Number) ->
    case cf_flow:lookup(Number, kapps_call:account_id(Call)) of
        {'ok', Flow, _NoMatch} ->
            lager:info("callflow ~s satisfies request", [kz_doc:id(Flow)]),
            Updates = [{fun kapps_call:set_request/2
                       ,list_to_binary([Number, "@", kapps_call:request_realm(Call)])
                       }
                      ,{fun kapps_call:set_to/2, list_to_binary([Number, "@", kapps_call:to_realm(Call)])}
                      ],
            cf_exe:set_call(kapps_call:exec(Updates, Call)),
            maybe_restrict_call(Data, Call, Number, Flow);
        _ ->
            lager:info("failed to find a callflow to satisfy ~s", [Number]),
            _ = kapps_call_command:b_prompt(<<"disa-invalid_extension">>, Call),
            allow_dial(Data, Call, Retries - 1, Interdigit)
    end.

%%------------------------------------------------------------------------------
%% @doc Check collect digits to be not empty, if empty collect again
%% (e.g. if previous callflow crashed during collecting digits before receiving pound
%% FreeSWITCH still thinks it's collecting for the previous callflow, and collect digits
%% for this module will be resulted to an empty binary)
%% @end
%%------------------------------------------------------------------------------
-spec collect_destination_number(kapps_call:call(), kz_json:object(), pos_integer()) -> kz_term:ne_binary().
collect_destination_number(Call, Data, Interdigit) ->
    MaxDigits = kz_json:get_integer_value(<<"max_digits">>, Data, 15),
    Timeout = kapps_call_command:default_collect_timeout(),
    lager:debug("collecting max ~p digits for destination number", [MaxDigits]),
    try_collect_destination_number(Call, Interdigit, MaxDigits, Timeout).

-spec try_collect_destination_number(kapps_call:call(), pos_integer(), pos_integer(), pos_integer()) -> kz_term:ne_binary().
try_collect_destination_number(Call, Interdigit, MaxDigits, Timeout) ->
    case kapps_call_command:collect_digits(MaxDigits, Timeout, Interdigit, Call) of
        {'ok', <<>>} -> try_collect_destination_number(Call, Interdigit, MaxDigits, Timeout);
        {'ok', Digits} -> knm_converters:normalize(Digits);
        {'error', _E} ->
            lager:info("caller hungup while collecting destination number"),
            cf_exe:stop(Call)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_restrict_call(kz_json:object(), kapps_call:call(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
maybe_restrict_call(Data, Call, Number, Flow) ->
    case should_restrict_call(Data, Call, Number) of
        'true' ->
            lager:info("disa is restricted from making this call, terminate", []),
            _ = kapps_call_command:answer(Call),
            _ = kapps_call_command:prompt(<<"cf-unauthorized_call">>, Call),
            _ = kapps_call_command:queued_hangup(Call),
            'ok';
        'false' ->
            cf_exe:branch(kz_json:get_json_value(<<"flow">>, Flow), Call)
    end.

-spec should_use_account_cid(kz_json:object()) -> boolean().
should_use_account_cid(Data) ->
    kz_json:is_true(<<"use_account_caller_id">>, Data, ?DEFAULT_USE_ACCOUNT_CALLER_ID).

-spec maybe_update_caller_id(kapps_call:call(), boolean()) -> kapps_call:call().
maybe_update_caller_id(Call, 'true') -> use_account_cid(Call);
maybe_update_caller_id(Call, 'false') -> keep_original_cid(Call).

-spec start_preconnect_audio(kz_json:object(), kapps_call:call()) -> 'ok'.
start_preconnect_audio(Data, Call) ->
    case kz_json:get_ne_binary_value(<<"preconnect_audio">>, Data, <<"dialtone">>) of
        <<"dialtone">> ->
            lager:debug("playing dialtone..."),
            play_dialtone(Call);
        <<"ringing">> ->
            lager:debug("playing ringing..."),
            play_ringing(Data, Call);
        _Else -> lager:debug("unknown preconnect audio type: ~p", [_Else])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec play_dialtone(kapps_call:call()) -> 'ok'.
play_dialtone(Call) ->
    Tone = kz_json:from_list([{<<"Frequencies">>, [<<"350">>, <<"440">>]}
                             ,{<<"Duration-ON">>, <<"10000">>}
                             ,{<<"Duration-OFF">>, <<"0">>}
                             ]),
    kapps_call_command:tones([Tone], Call).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec play_ringing(kz_json:object(), kapps_call:call()) -> 'ok'.
play_ringing(Data, Call) ->
    RingRepeatCount = kz_json:get_integer_value(<<"ring_repeat_count">>, Data, 1),
    Tone = kz_json:from_list([{<<"Frequencies">>, [<<"440">>, <<"480">>]}
                             ,{<<"Duration-ON">>, <<"2000">>}
                             ,{<<"Duration-OFF">>, <<"4000">>}
                             ,{<<"Repeat">>, RingRepeatCount}
                             ]),
    kapps_call_command:tones([Tone], Call).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec use_account_cid(kapps_call:call()) -> kapps_call:call().
use_account_cid(Call) ->
    AccountId = kapps_call:account_id(Call),
    {Number, Name} = kz_attributes:get_account_external_cid(Call),

    lager:info("setting the caller id to <~s> ~s from account ~s", [Name, Number, AccountId]),

    set_cid(Number, Name, Call).

-spec keep_original_cid(kapps_call:call()) -> kapps_call:call().
keep_original_cid(Call) ->
    Number = kapps_call:caller_id_number(Call),
    Name = kapps_call:caller_id_name(Call),

    lager:info("keep the original caller id <~s> ~s", [Name, Number]),

    set_cid(Number, Name, Call).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_cid(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> kapps_call:call().
set_cid(Number, Name, Call) ->
    Props = [{<<"Retain-CID">>, 'true'}
            ,{<<"Caller-ID-Number">>, Number}
            ,{<<"Caller-ID-Name">>, Name}
            ],
    Updates = [fun(C) -> kapps_call:set_caller_id_number(Number, C) end
              ,fun(C) -> kapps_call:set_caller_id_name(Name, C) end
              ,fun(C) -> kapps_call:set_custom_channel_vars(Props, C) end
              ],
    kapps_call:exec(Updates, Call).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_restrict_call(kz_json:object(), kapps_call:call(), kz_term:ne_binary()) -> boolean().
should_restrict_call(Data, Call, Number) ->
    case kz_json:is_true(<<"enforce_call_restriction">>, Data, 'false') of
        'false' -> 'false';
        'true' -> should_restrict_call_by_account(Call, Number)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_restrict_call_by_account(kapps_call:call(), kz_term:ne_binary()) -> boolean().
should_restrict_call_by_account(Call, Number) ->
    case kzd_accounts:fetch(kapps_call:account_id(Call)) of
        {'error', _} -> 'false';
        {'ok', JObj} ->
            Classification = knm_converters:classify(Number),
            lager:info("classified number as ~p", [Classification]),
            <<"deny">> =:= kz_json:get_ne_binary_value([<<"call_restriction">>, Classification, <<"action">>], JObj)
    end.
