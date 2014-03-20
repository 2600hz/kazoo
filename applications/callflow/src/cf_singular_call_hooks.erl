%%%-------------------------------------------------------------------
%%% @copyright (C) 2014
%%% @doc
%%% A singular call is as an entire conversation as dialed by the caller,
%%% and it may comprise of multiple "legs" or "calls".
%%% This module is called by cf_exe at the init and destroy points of a call, in order to
%%% identify the conversation in a singular manner and send out hooks to external URLs.
%%%
%%% Hook behavior is disabled by default, and will only be enabled if "singular_call_hook_url"
%%% in the system_config / callflow configuration is populated.
%%%
%%% Example of JSON hook sent via post on call init:
%%% {"Event":"init","CallId":"OTdlYzFkMDZlZmRhYWY1YmEzN2RhNmMxZWNiYTQ4NDc",
%%%  "To":"+14088317607","From":"+16505811111","Inception":"onnet"}
%%%
%%% Example of JSON hook sent via post on call destroy:
%%% {"Event":"destroy","CallId":"OTdlYzFkMDZlZmRhYWY1YmEzN2RhNmMxZWNiYTQ4NDc",
%%%  "To":"+14088317607","From":"+16505811111","Inception":"onnet","Duration-Seconds":"33",
%%%  "Hangup-Cause":"NORMAL_CLEARING","Disposition":"SUCCESS"}
%%%
%%% Note: Be sure to set the internal and external caller IDs for the devices.
%%% These are used to resolve to/from numbers correctly.
%%%
%%% @end
%%% @contributors
%%%   Benedict Chan (benchan@sendhub.com)
%%%-------------------------------------------------------------------

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% First we check if this feature is enabled - if not, we return false.
%% Next we check if the call is an indicator for the start of a singular call (A-leg),
%% and if so, then we know the call should be hooked. 
%% We then can start the event listener, which will send init and end hooks.
%%
%% @spec maybe_hook_call(whapps_call:call()) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec maybe_hook_call(whapps_call:call()) -> boolean().
maybe_hook_call(Call) ->
    %% Never invoke anything if we are disabled
    case should_hook(Call) of
        'true' -> 
            %% start event listener, which will be responsible for sending all hooks, 
            %% including the init hook (we want to do it this way to make sure we are listening)
            cf_exe:add_event_listener(Call, {'cf_singular_call_hooks_listener', []});
        'false' -> 'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sends an initial request signifying the start of this entire conversation
%% in a hook to a preconfigured URL.
%%
%% @spec send_init_hook(whapps_call:call()) -> boolean().
%% @end
%%--------------------------------------------------------------------
-spec send_init_hook(whapps_call:call()) -> boolean().
send_init_hook(Call) ->
    lager:debug("===CALL STARTED===", []),
    lager:debug("Event: init", []),
    lager:debug("Call-Id: ~s", [whapps_call:call_id_direct(Call)]),
    lager:debug("To: ~s", [wnm_util:to_e164(whapps_call:to_user(Call))]),
    lager:debug("From: ~s", [wnm_util:to_e164(whapps_call:caller_id_number(Call))]),
    lager:debug("Inception: ~s", [get_inception(Call)]),
    lager:debug("================", []),

    Prop = [{<<"Event">>, <<"init">>}
            ,{<<"Call-Id">>, whapps_call:call_id(Call)}
            ,{<<"Bridge-ID">>, whapps_call:custom_channel_var(<<"Bridge-ID">>, Call)}
            ,{<<"To">>, wnm_util:to_e164(whapps_call:to_user(Call))}
            ,{<<"From">>, wnm_util:to_e164(whapps_call:caller_id_number(Call))}
            ,{<<"Inception">>, get_inception(Call)}
            ],

    JObj = wh_json:from_list(props:filter_undefined(Prop)),
    URI = binary_to_list(get_hook_url()),

    case ibrowse:send_req(URI
                         ,[{"Content-Type", "application/json"}]
                         ,'post'
                         ,wh_json:encode(JObj)
                         ,[{'connect_timeout', 5000}]
                         ,5000
                         )
    of
        {'error', Reason} ->
            lager:warning("Ibrowse error when sending singular call init hook: ~p", [Reason]),
            'false';
        _ -> 
            'true'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sends a request signifying the end of this entire conversation in a
%% hook to a preconfigured URL.
%%
%% @spec send_end_hook(whapps_call:call(), wh_json:object()) -> boolean().
%% @end
%%--------------------------------------------------------------------
-spec send_end_hook(whapps_call:call(), wh_json:object()) -> boolean().
send_end_hook(Call, Event) ->
    lager:debug("===CALL ENDED===", []),
    lager:debug("Event: end", []),
    lager:debug("CallId: ~s", [whapps_call:call_id_direct(Call)]),
    lager:debug("To: ~s", [wnm_util:to_e164(whapps_call:to_user(Call))]),
    lager:debug("From: ~s", [wnm_util:to_e164(whapps_call:caller_id_number(Call))]),
    lager:debug("Inception: ~s", [get_inception(Call)]),
    lager:debug("CallDuration: ~s", [wh_json:get_value(<<"Duration-Seconds">>, Event)]),
    lager:debug("HangupReason: ~s", [wh_json:get_value(<<"Hangup-Cause">>, Event)]),
    lager:debug("Disposition: ~s", [wh_json:get_value(<<"Disposition">>, Event)]),
    lager:debug("================", []),

    Prop = [{<<"Event">>, <<"destroy">>}
            ,{<<"Call-Id">>, whapps_call:call_id_direct(Call)}
            ,{<<"Bridge-Id">>, whapps_call:custom_channel_var(<<"Bridge-ID">>, Call)}
            ,{<<"To">>, wnm_util:to_e164(whapps_call:to_user(Call))}
            ,{<<"From">>, wnm_util:to_e164(whapps_call:caller_id_number(Call))}
            ,{<<"Inception">>, get_inception(Call)}
            ,{<<"Duration-Seconds">>, wh_json:get_value(<<"Duration-Seconds">>, Event)}
            ,{<<"Hangup-Cause">>, wh_json:get_value(<<"Hangup-Cause">>, Event)}
            ,{<<"Disposition">>, wh_json:get_value(<<"Disposition">>, Event)}
           ],

    JObj = wh_json:from_list(props:filter_undefined(Prop)),
    URI = binary_to_list(get_hook_url()),

    case ibrowse:send_req(URI
                         ,[{"Content-Type", "application/json"}]
                         ,'post'
                         ,wh_json:encode(JObj)
                         ,[{'connect_timeout', 5000}]
                         ,5000
                         )
    of
        {'error', Reason} ->
            lager:warning("Ibrowse error when sending singular end of call hook: ~p", [Reason]),
            'false';
        _ -> 'true'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks if there is a non-empty hook url and that the call is singular (or a transfer)
%% @end
%%--------------------------------------------------------------------
-spec should_hook(whapps_call:call()) -> boolean().
should_hook(Call) ->
    is_enabled() andalso call_is_singular(Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks if the singular call hook is enabled in the callflow system config.
%% The call hook is enabled if the URL in the system_config / callflows / singular_call_hook_url
%% field is not set to disabled.
%%
%% @spec is_enabled() -> boolean().
%% @end
%%--------------------------------------------------------------------
-spec is_enabled() -> boolean().
is_enabled() ->
    (not wh_util:is_empty(get_hook_url())).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function identifies if a call is the first of the conversation by checking if it
%% has an existing bridge. We also check the presence of referredby and 
%% want to send the hook if it is a call transfer 
%%
%% @spec call_is_singular(whapps_call:call()) -> boolean().
%% @end
%%--------------------------------------------------------------------
-spec call_is_singular(whapps_call:call()) -> boolean().
call_is_singular(Call) ->
    BridgeID = whapps_call:custom_channel_var(<<"Bridge-ID">>, Call),
    ReferredBy = whapps_call:custom_channel_var(<<"Referred-By">>, Call),
    CallId = whapps_call:call_id_direct(Call),
    (BridgeID =:= 'undefined')
        orelse (BridgeID =:= CallId)
        orelse (ReferredBy =/= 'undefined').

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Gets where the call was started. If whapps_call returns undefined, it was on net.
%%
%% @spec get_inception(whapps_call:call()) -> ne_binary().
%% @end
%%--------------------------------------------------------------------
-spec get_inception(whapps_call:call()) -> ne_binary().
get_inception(Call) ->
    case whapps_call:inception(Call) of
        'undefined' -> <<"onnet">>;
        _Else -> <<"offnet">>
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Gets the singular call hook URL from the configuration (may be cached).
%%
%% @spec get_hook_url() -> ne_binary().
%% @end
%%--------------------------------------------------------------------
-spec get_hook_url() -> ne_binary().
get_hook_url() ->
    whapps_config:get_binary(?CF_CONFIG_CAT, <<"singular_call_hook_url">>, <<"">>).
