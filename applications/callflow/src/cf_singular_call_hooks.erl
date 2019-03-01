%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc A singular call is as an entire conversation as dialed by the caller,
%%% and it may comprise of multiple "legs" or "calls".
%%%
%%% This module is called by {@link cf_exe} at the initialization and destroy
%%% points of a call, in order to identify the conversation in a singular manner
%%% and send out hooks to external URLs.
%%%
%%% Hook behavior is disabled by default, and will only be enabled if `singular_call_hook_url'
%%% in the `system_config' or callflow configuration is populated.
%%%
%%% Example of JSON hook sent via `POST' on call init:
%%% ```
%%%    {
%%%      "Event": "init",
%%%      "CallID": "OTdlYzFkMDZlZmRhYWY1YmEzN2RhNmMxZWNiYTQ4NDc",
%%%      "To": "+14088317607",
%%%      "From": "+16505811111",
%%%      "Inception": "onnet"
%%%    }
%%% '''
%%%
%%% Example of JSON hook sent via `POST' on call destroy:
%%% ```
%%%    {
%%%      "Event": "destroy",
%%%      "CallID": "OTdlYzFkMDZlZmRhYWY1YmEzN2RhNmMxZWNiYTQ4NDc",
%%%      "To": "+14088317607",
%%%      "From": "+16505811111",
%%%      "Inception": "onnet",
%%%      "Duration-Seconds": "33",
%%%      "Hangup-Cause": "NORMAL_CLEARING",
%%%      "Disposition":"SUCCESS"
%%%    }
%%% '''
%%%
%%% <div class="notice">Be sure to set the internal and external caller IDs
%%% for the devices. These are used to resolve to/from numbers correctly.</div>
%%%
%%%
%%% @author Benedict Chan <benchan@sendhub.com>
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_singular_call_hooks).
-include("callflow.hrl").

%% API to send hooks
-export([maybe_hook_call/1
        ,send_init_hook/1
        ,send_end_hook/2
        ]).

%% Helper functions
-export([is_enabled/0
        ,get_hook_url/0
        ]).

%%------------------------------------------------------------------------------
%% @doc First we check if this feature is enabled - if not, we return false.
%% Next we check if the call is an indicator for the start of a singular call (A-leg),
%% and if so, then we know the call should be hooked.
%% We then can start the event listener, which will send init and end hooks.
%%
%% @end
%%------------------------------------------------------------------------------
-spec maybe_hook_call(kapps_call:call()) -> boolean().
maybe_hook_call(Call) ->
    %% Never invoke anything if we are disabled
    case should_hook(Call) of
        'true' ->
            %% start event listener, which will be responsible for sending all hooks,
            %% including the init hook (we want to do it this way to make sure we are listening)
            cf_exe:add_event_listener(Call, {'cf_singular_call_hooks_listener', []});
        'false' -> 'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Sends an initial request signifying the start of this entire conversation
%% in a hook to a pre-configured URL.
%%
%% @end
%%------------------------------------------------------------------------------
-spec send_init_hook(kapps_call:call()) -> boolean().
send_init_hook(Call) ->
    lager:debug("===CALL STARTED===", []),
    lager:debug("event: init", []),
    lager:debug("call-ID: ~s", [kapps_call:call_id_direct(Call)]),
    lager:debug("to: ~s", [knm_converters:normalize(kapps_call:to_user(Call))]),
    lager:debug("from: ~s", [knm_converters:normalize(kapps_call:caller_id_number(Call))]),
    lager:debug("inception: ~s", [get_inception(Call)]),
    lager:debug("================", []),

    JObj = kz_json:from_list(
             [{<<"Event">>, <<"init">>}
             ,{<<"Call-ID">>, kapps_call:call_id(Call)}
             ,{<<"To">>, knm_converters:normalize(kapps_call:to_user(Call))}
             ,{<<"From">>, knm_converters:normalize(kapps_call:caller_id_number(Call))}
             ,{<<"Inception">>, get_inception(Call)}
             ]),

    URI = binary_to_list(get_hook_url()),

    case kz_http:post(URI
                     ,[{"Content-Type", "application/json"}]
                     ,kz_json:encode(JObj)
                     ,[{'connect_timeout', 5000}, {'timeout', 5000}]
                     )
    of
        {'error', Reason} ->
            lager:warning("error when sending singular call init hook: ~p", [Reason]),
            'false';
        _ ->
            'true'
    end.

%%------------------------------------------------------------------------------
%% @doc Sends a request signifying the end of this entire conversation in a
%% hook to a pre-configured URL.
%%
%% @end
%%------------------------------------------------------------------------------
-spec send_end_hook(kapps_call:call(), kz_json:object()) -> boolean().
send_end_hook(Call, Event) ->
    lager:debug("===CALL ENDED===", []),
    lager:debug("event: end", []),
    lager:debug("call-ID: ~s", [kapps_call:call_id_direct(Call)]),
    lager:debug("to: ~s", [knm_converters:normalize(kapps_call:to_user(Call))]),
    lager:debug("from: ~s", [knm_converters:normalize(kapps_call:caller_id_number(Call))]),
    lager:debug("inception: ~s", [get_inception(Call)]),
    lager:debug("callDuration: ~s", [kz_json:get_value(<<"Duration-Seconds">>, Event)]),
    lager:debug("hangupReason: ~s", [kz_json:get_value(<<"Hangup-Cause">>, Event)]),
    lager:debug("disposition: ~s", [kz_json:get_value(<<"Disposition">>, Event)]),
    lager:debug("================", []),

    ReferredBy = kapps_call:custom_channel_var(<<"Referred-By">>, Call),
    CallID =
        case ReferredBy of
            'undefined' -> kapps_call:call_id_direct(Call);
                                                % if we were a forwarded call, refer to the original call id (bridge id)
            _ -> kapps_call:custom_channel_var(<<"Bridge-ID">>, Call)
        end,

    JObj = kz_json:from_list(
             [{<<"Event">>, <<"destroy">>}
             ,{<<"Call-ID">>, CallID}
             ,{<<"To">>, knm_converters:normalize(kapps_call:to_user(Call))}
             ,{<<"From">>, knm_converters:normalize(kapps_call:caller_id_number(Call))}
             ,{<<"Inception">>, get_inception(Call)}
             ,{<<"Duration-Seconds">>, kz_json:get_value(<<"Duration-Seconds">>, Event)}
             ,{<<"Hangup-Cause">>, kz_json:get_value(<<"Hangup-Cause">>, Event)}
             ,{<<"Disposition">>, kz_json:get_value(<<"Disposition">>, Event)}
             ]),

    URI = binary_to_list(get_hook_url()),

    case kz_http:post(URI
                     ,[{"Content-Type", "application/json"}]
                     ,kz_json:encode(JObj)
                     ,[{'connect_timeout', 5000}, {'timeout', 5000}]
                     )
    of
        {'error', Reason} ->
            lager:warning("error when sending singular end of call hook: ~p", [Reason]),
            'false';
        _ ->
            'true'
    end.

%%------------------------------------------------------------------------------
%% @doc Checks if there is a non-empty hook URL and that the call is singular (or a transfer)
%% @end
%%------------------------------------------------------------------------------
-spec should_hook(kapps_call:call()) -> boolean().
should_hook(Call) ->
    is_enabled()
        andalso call_is_singular(Call).

%%------------------------------------------------------------------------------
%% @doc Checks if the singular call hook is enabled in the callflow system config.
%% The call hook is enabled if the URL in the `system_config' / callflows / `singular_call_hook_url'
%% field is not set to disabled.
%%
%% @end
%%------------------------------------------------------------------------------
-spec is_enabled() -> boolean().
is_enabled() ->
    (not kz_term:is_empty(get_hook_url())).

%%------------------------------------------------------------------------------
%% @doc This function identifies if a call is the first of the conversation by
%% checking if it has an existing bridge. We also check the presence of referred
%% by and want to send the hook if it is a call transfer
%%
%% @end
%%------------------------------------------------------------------------------
-spec call_is_singular(kapps_call:call()) -> boolean().
call_is_singular(Call) ->
    BridgeID = kapps_call:custom_channel_var(<<"Bridge-ID">>, Call),
    ReferredBy = kapps_call:custom_channel_var(<<"Referred-By">>, Call),
    CallID = kapps_call:call_id_direct(Call),
    (BridgeID =:= 'undefined')
        orelse (BridgeID =:= CallID)
        orelse (ReferredBy =/= 'undefined').

%%------------------------------------------------------------------------------
%% @doc Gets where the call was started. If kapps_call returns undefined, it was on net.
%% @end
%%------------------------------------------------------------------------------
-spec get_inception(kapps_call:call()) -> kz_term:ne_binary().
get_inception(Call) ->
    case kapps_call:inception(Call) of
        'undefined' -> <<"onnet">>;
        _Else -> <<"offnet">>
    end.

%%------------------------------------------------------------------------------
%% @doc Gets the singular call hook URL from the configuration (may be cached).
%% @end
%%------------------------------------------------------------------------------
-spec get_hook_url() -> kz_term:ne_binary().
get_hook_url() ->
    kapps_config:get_binary(?CF_CONFIG_CAT, <<"singular_call_hook_url">>, <<"">>).
