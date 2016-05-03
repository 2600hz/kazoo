%%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%% "data":{
%%%   "pin":"1234"
%%%   ,"retries":3
%%%   // optional after here
%%%   ,"interdigit":2000
%%%   ,"max_digits":15
%%% }
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_disa).

-include("callflow.hrl").

-export([handle/2]).

-define(DEFAULT_USE_ACCOUNT_CALLER_ID, kapps_config:get(?CF_CONFIG_CAT, <<"default_use_account_caller_id">>, 'true')).
-define(DEFAULT_PIN_LENGTH, kapps_config:get_integer(?CF_CONFIG_CAT, <<"default_pin_length">>, 10)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    lager:info("starting DISA handler"),
    kapps_call_command:answer(Call),

    Pin = kz_json:get_value(<<"pin">>, Data),
    Retries = kz_json:get_integer_value(<<"retries">>, Data, 3),
    Interdigit = kz_json:get_integer_value(<<"interdigit">>, Data, kapps_call_command:default_interdigit_timeout()),

    case try_collect_pin(Call, Pin, Retries, Interdigit) of
        'allow' -> allow_dial(Data, Call, Retries, Interdigit);
        'fail' -> cf_exe:stop(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec allow_dial(kz_json:object(), kapps_call:call(), non_neg_integer(), pos_integer()) -> 'ok'.
allow_dial(_, Call, 0, _Interdigit) ->
    lager:info("retries exceeded for finding a callflow"),
    cf_exe:continue(Call);
allow_dial(Data, Call, Retries, Interdigit) ->
    _ = start_preconnect_audio(Data, Call),
    MaxDigits = kz_json:get_integer_value(<<"max_digits">>, Data, 15),

    lager:debug("collecting max ~p digits for destination number", [MaxDigits]),
    {'ok', Digits} = kapps_call_command:collect_digits(MaxDigits
                                                        ,kapps_call_command:default_collect_timeout()
                                                        ,Interdigit
                                                        ,Call
                                                       ),

    Number = knm_converters:normalize(Digits),
    lager:info("caller is trying to call '~s'", [Number]),

    Call1 = maybe_update_caller_id(Data, Call),

    maybe_route_to_callflow(Data, Call1, Retries, Interdigit, Number).

maybe_route_to_callflow(Data, Call, Retries, Interdigit, Number) ->
    case cf_util:lookup_callflow(Number, kapps_call:account_id(Call)) of
        {'ok', Flow, NoMatch} ->
            lager:info("callflow ~s satisfies request", [kz_doc:id(Flow)]),
            Updates = [{fun kapps_call:set_request/2
                        ,list_to_binary([Number, "@", kapps_call:request_realm(Call)])
                       }
                       ,{fun kapps_call:set_to/2, list_to_binary([Number, "@", kapps_call:to_realm(Call)])}
                       ,fun(C) when NoMatch ->
                                {CIDNum, CIDName} = cf_attributes:caller_id(<<"external">>, C),
                                C1 = kapps_call:set_caller_id_number(CIDNum, C),
                                kapps_call:set_caller_id_name(CIDName, C1);
                           (C) -> C
                        end
                      ],
            {'ok', C} = cf_exe:get_call(Call),
            cf_exe:set_call(kapps_call:exec(Updates, C)),
            maybe_restrict_call(Data, Call, Number, Flow);
        _ ->
            lager:info("failed to find a callflow to satisfy ~s", [Number]),
            _ = kapps_call_command:b_prompt(<<"disa-invalid_extension">>, Call),
            allow_dial(Data, Call, Retries - 1, Interdigit)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_restrict_call(kz_json:object(), kapps_call:call(), ne_binary(), kz_json:object()) -> 'ok'.
maybe_restrict_call(Data, Call, Number, Flow) ->
    case should_restrict_call(Data, Call, Number) of
        'true' ->
            lager:info("disa is restricted from making this call, terminate", []),
            _ = kapps_call_command:answer(Call),
            _ = kapps_call_command:prompt(<<"cf-unauthorized_call">>, Call),
            _ = kapps_call_command:queued_hangup(Call),
            'ok';
        'false' ->
            cf_exe:branch(kz_json:get_value(<<"flow">>, Flow), Call)
    end.

-spec maybe_update_caller_id(kz_json:object(), kapps_call:call()) -> kapps_call:call().
maybe_update_caller_id(Data, Call) ->
    case kz_json:is_true(<<"use_account_caller_id">>, Data, ?DEFAULT_USE_ACCOUNT_CALLER_ID) of
        'true'  -> set_caller_id(Call);
        'false' -> Call
    end.

-spec start_preconnect_audio(kz_json:object(), kapps_call:call()) -> 'ok'.
start_preconnect_audio(Data, Call) ->
    case kz_json:get_ne_value(<<"preconnect_audio">>, Data, <<"dialtone">>) of
        <<"dialtone">> ->
            lager:debug("playing dialtone..."),
            play_dialtone(Call);
        <<"ringing">> ->
            lager:debug("playing ringing..."),
            play_ringing(Data, Call);
        _Else -> lager:debug("unknown preconnect audio type: ~p", [_Else])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec play_dialtone(kapps_call:call()) -> 'ok'.
play_dialtone(Call) ->
    Tone = kz_json:from_list([{<<"Frequencies">>, [<<"350">>, <<"440">>]}
                              ,{<<"Duration-ON">>, <<"10000">>}
                              ,{<<"Duration-OFF">>, <<"0">>}
                             ]),
    kapps_call_command:tones([Tone], Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec play_ringing(kz_json:object(), kapps_call:call()) -> 'ok'.
play_ringing(Data, Call) ->
    RingRepeatCount = kz_json:get_integer_value(<<"ring_repeat_count">>, Data, 1),
    Tone = kz_json:from_list([{<<"Frequencies">>, [<<"440">>, <<"480">>]}
                              ,{<<"Duration-ON">>, <<"2000">>}
                              ,{<<"Duration-OFF">>, <<"4000">>}
                              ,{<<"Repeat">>, RingRepeatCount}
                             ]),
    kapps_call_command:tones([Tone], Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_caller_id(kapps_call:call()) -> kapps_call:call().
set_caller_id(Call) ->
    AccountId = kapps_call:account_id(Call),
    {Number, Name} = maybe_get_account_cid(AccountId, Call),
    lager:info("setting the caller id number to ~s from account ~s", [Number, AccountId]),
    Updates = [fun(C) -> kapps_call:kvs_store('dynamic_cid', Number, C) end
               ,fun(C) ->
                        C1 = kapps_call:set_caller_id_number(Number, C),
                        kapps_call:set_caller_id_name(Name, C1)
                end
              ],
    UpdatedCall = kapps_call:exec(Updates, Call),
    cf_exe:set_call(UpdatedCall),
    UpdatedCall.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_get_account_cid(ne_binary(), kapps_call:call()) ->
                                   {api(binary()), api(binary())}.
maybe_get_account_cid(AccountId, Call) ->
    Name = kapps_call:caller_id_name(Call),
    Number = kapps_call:caller_id_number(Call),
    case kz_account:fetch(AccountId) of
        {'error', _} -> cf_attributes:maybe_get_assigned_number(Number, Name, Call);
        {'ok', JObj} -> maybe_get_account_external_number(Number, Name, JObj, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_get_account_external_number(ne_binary(), ne_binary(), kz_json:object(), kapps_call:call()) ->
                                               {api(binary()), api(binary())}.
maybe_get_account_external_number(Number, Name, Account, Call) ->
    External = kz_json:get_ne_value([<<"caller_id">>, <<"external">>, <<"number">>], Account),
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
-spec is_valid_caller_id(api(binary()), kapps_call:call()) -> boolean().
is_valid_caller_id('undefined', _) -> 'false';
is_valid_caller_id(_, _) -> 'true'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec should_restrict_call(kz_json:object(), kapps_call:call(), ne_binary()) -> boolean().
should_restrict_call(Data, Call, Number) ->
    case kz_json:is_true(<<"enforce_call_restriction">>, Data, 'false') of
        'false' -> 'false';
        'true' -> should_restrict_call_by_account(Call, Number)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec should_restrict_call_by_account(kapps_call:call(), ne_binary()) -> boolean().
should_restrict_call_by_account(Call, Number) ->
    case kz_account:fetch(kapps_call:account_id(Call)) of
        {'error', _} -> 'false';
        {'ok', JObj} ->
            Classification = knm_converters:classify(Number),
            lager:info("classified number as ~p", [Classification]),
            kz_json:get_value([<<"call_restriction">>, Classification, <<"action">>], JObj) =:= <<"deny">>
    end.
