%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, Umojo
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Jon Blanton
%%%-------------------------------------------------------------------
-module(cf_speech_ivr).

-include("../callflow.hrl").

-export([handle/2]).

-define(CONFIG_CAT, <<"speech_ivr">>).
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call the speech IVR as defined
%% Returns continue if fails to connect or stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case bridge_to_speech_ivr(Data, Call) of
        {ok, HangupData} ->
            %% success - branch/transfer to correct child
            CustomSipHeaders = wh_json:get_value(<<"Custom-SIP-Headers">>, HangupData), 
            Instructions = {wh_json:get_ne_value(<<"X-Branch">>, CustomSipHeaders)
                            ,wh_json:get_ne_value(<<"X-Transfer">>, CustomSipHeaders)
                           },

            case Instructions of
                {undefined, undefined} ->
                    lager:debug("no instructions found"),
                    cf_exe:continue(Call);
                {undefined, CallflowId} ->
                    lager:debug("transfer instruction found"),
                    case couch_mgr:open_doc(whapps_call:account_db(Call), CallflowId) of
                        {ok, Doc} ->
                            lager:debug("branching to callflow ~s", [CallflowId]),
                            Flow = wh_json:get_value(<<"flow">>, Doc, wh_json:new()),
                            cf_exe:branch(Flow, Call);
                        {error, R} ->
                            lager:debug("could not branch to callflow ~s, ~p", [CallflowId, R]),
                            cf_exe:continue(Call)
                    end;
                {BranchKey, _} ->
                    lager:debug("branch instruction found"),
                    cf_exe:attempt(BranchKey, Call)
            end;
        {_, _} ->
            %% failure - attempt to fallback to non-speech enabled ivr
            case wh_json:get_ne_value(<<"menu_id">>, Data, undefined) of
                undefined ->
                    cf_exe:continue(Call);
                MenuId ->
                    %% possibly say "due to high call volume, speech is currently disabled"
                    cf_menu:handle(wh_json:set_value(<<"id">>, MenuId, wh_json:new()), Call)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to bridge to the the speech IVR
%% @end
%%--------------------------------------------------------------------
-spec bridge_to_speech_ivr(wh_json:json_object(), whapps_call:call()) -> 'ok'.
bridge_to_speech_ivr(Data, Call) ->
    SpeechIvrEndpoint = build_speech_ivr_endpoint(whapps_call:request_user(Call)
                                                  ,whapps_call:account_id(Call)
                                                  ,wh_json:get_value(<<"id">>, Data)
                                                  ,<<"PLACEHOLDER">>
                                                 ),
    Timeout = whapps_config:get(?CONFIG_CAT, <<"bridge_timeout">>),
    whapps_call_command:b_bridge([SpeechIvrEndpoint], Timeout, <<"simultaneous">>, true, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds the speech IVR endpoint
%% @end
%%--------------------------------------------------------------------
build_speech_ivr_endpoint(Destination, AccountId, IvrId, CacheId) ->
    Route = wh_util:to_binary([<<"sip:">>, Destination, <<"@">>, whapps_config:get(?CONFIG_CAT, <<"sip_uri_host">>)]),
    SipHeaders = wh_json:from_list([{<<"X-Account-Id">>, AccountId}
                                    ,{<<"X-IVR-Id">>, IvrId}
                                    ,{<<"X-Cache-Id">>, CacheId}
                                   ]),
    wh_json:from_list([{<<"Invite-Format">>, <<"route">>}
                       ,{<<"Route">>, Route}
                       ,{<<"SIP-Headers">>, SipHeaders}
                      ]).
