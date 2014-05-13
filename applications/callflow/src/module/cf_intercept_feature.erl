%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz INC
%%% @doc
%%% Intercept a call in the specified device/user/extension
%%%
%%% data: {
%%%   "type" : "user | device | extension"
%%% }
%%%
%%% uses cf_capture_group and type to build parameters to branch to cf_intercept
%%%
%%% user -> lookup groups by number  (WIP)
%%% device -> lookup device by sip username
%%% extension -> lookup callflows
%%%
%%% usage example for BLF on spa504g
%%% the sip user of device we want to monitor for this example is 55578547
%%% on "Phone" Tab, go to "Line Key 2" and set
%%%   Extension : disabled
%%%   Share Call Appearance : private
%%%   Extended Function :fnc=blf+cp;sub=55578547@sip.domain.com;ext=55578547@sip.domain.com
%%%
%%% on "Attendant Console" Tab, set "Attendant Console Call Pickup Code:" to *98# instead of *98
%%% this way the username part of the subscription is passed along (*9855578547)
%%%
%%% create a "pattern callflow" with "patterns": ["^\\*98([0-9]*)$"]
%%% set the parameter "type" to "device"
%%%
%%%
%%% usage example for extension pickup
%%%
%%% 1) create a "pattern callflow" with "patterns": ["^\\*7([0-9]*)$"] and set the parameter "type" to "extension"
%%% 2) create simple callflow with number = 401 and set the target to a "ring group" or "page group"
%%% 3) dial 401 to start ringing the phones in group, in another phone dial *7401 to pickup the call
%%%
%%%
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo  <luis.azedo@factorlusitano.com>
%%%-------------------------------------------------------------------
%%% @author Luis Azedo
%%% @copyright (C) 2013, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 10 Oct 2013
%%%-------------------------------------------------------------------
-module(cf_intercept_feature).

-include("../callflow.hrl").

-export([handle/2]).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, creates the parameters and branches
%% to cf_intercept.
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Number = whapps_call:kvs_fetch('cf_capture_group', Call),
    InterceptType = wh_json:get_value(<<"type">>, Data),
    case build_intercept_params(Number, InterceptType, Call) of
        {'ok', Params} ->
            case make_secure_params(Data) of
                [] ->
                    case maybe_allowed_to_intercept(Call, Params) of
                        'true' ->
                            cf_intercept:handle(wh_json:from_list(Params), Call);
                        'false' ->
                            no_permission_to_intercept(Call),
                            cf_exe:stop(Call)
                    end;
                SParams ->
                    cf_intercept:handle(wh_json:from_list(SParams ++ Params), Call)
            end;
        {'error', _E} ->
            lager:info("Error <<~s>> processing intercept '~s' for number ~s"
                       ,[_E, InterceptType, Number]),
            _ = whapps_call_command:b_play(<<"park-no_caller">>, Call),
            cf_exe:stop(Call)
    end.

-spec make_secure_params(wh_json:object()) -> wh_proplist().
make_secure_params(Data) ->
    SecureParams = [
      {<<"approved_device_id">>, wh_json:get_value(<<"approved_device_id">>, Data)},
      {<<"approved_user_id">>, wh_json:get_value(<<"approved_user_id">>, Data)},
      {<<"approved_group_id">>, wh_json:get_value(<<"approved_group_id">>, Data)}
    ],
    props:filter_undefined(SecureParams).

-spec maybe_allowed_to_intercept(whapps_call:call(), wh_proplist()) -> boolean().
maybe_allowed_to_intercept(Call, Props) ->
  Id = whapps_call:authorizing_id(Call),
  case couch_mgr:open_cache_doc(whapps_call:account_db(Call), Id) of
      {'ok', DeviceDoc} ->
          Caller = wh_json:get_value(<<"owner_id">>, DeviceDoc),
          case wh_json:get_value(<<"user_id">>, Props) of
              'undefined' ->
                  case wh_json:get_value(<<"device_id">>, Props) of
                      'undefined' -> 'false';
                      DevId ->
                          case couch_mgr:open_cache_doc(whapps_call:account_db(Call), DevId) of
                              {ok, Target} ->
                                  Caller =:= wh_json:get_value(<<"owner_id">>, Target);
                              Err ->
                                  lager:info("Error while opening couch document: ~p", [Err]),
                                  'false'
                          end
                  end;
              UserId ->
                  UserId =:= Caller
          end;
      Err ->
          lager:info("Error while opening couch document: ~p", [Err]),
          'false'
  end.

-spec build_intercept_params(ne_binary(), ne_binary(), whapps_call:call()) ->
                                 {'ok', wh_proplist()} |
                                 {'error', ne_binary()}.
build_intercept_params(Number, <<"device">>, Call) ->
    AccountDb = whapps_call:account_db(Call),
    case cf_util:endpoint_id_by_sip_username(AccountDb, Number) of
        {'ok', EndpointId} -> {'ok', [{<<"device_id">>, EndpointId}]};
        {'error', _}=E -> E
    end;
build_intercept_params(_Number, <<"user">>, _Call) ->
    {'error', <<"work in progress">>};
build_intercept_params(Number, <<"extension">>, Call) ->
    AccountId = whapps_call:account_id(Call),
    case cf_util:lookup_callflow(Number, AccountId) of
        {'ok', FlowDoc, 'false'} ->
            Data = wh_json:get_value([<<"flow">>, <<"data">>], FlowDoc),
            Module = wh_json:get_value([<<"flow">>, <<"module">>], FlowDoc),
            params_from_data(Module, Data,Call);
        {'ok', _FlowDoc, 'true'} ->
            {'error', <<"no callflow with extension ", Number/binary>>};
        {'error', _} = E -> E
    end;
build_intercept_params(_ ,'undefined', _) ->
    {'error', <<"parameter 'type' not defined">>};
build_intercept_params(_, Other, _) ->
    {'error', <<Other/binary," not implemented">>}.

-spec params_from_data(ne_binary(), wh_json:object(), whapps_call:call()) ->
                              {'ok', wh_proplist()} |
                              {'error', ne_binary()}.
params_from_data(<<"user">>, Data, _Call) ->
    EndpointId = wh_json:get_value(<<"id">>,Data),
    {'ok', [{<<"user_id">>, EndpointId}]};
params_from_data(<<"device">>, Data, _Call) ->
    EndpointId = wh_json:get_value(<<"id">>, Data),
    {'ok', [{<<"device_id">>, EndpointId}]};

params_from_data('undefined', _, _) ->
    {'error',<<"module not defined in callflow">>};
params_from_data(Other, _, _) ->
    {'error',<<"module ",Other/binary," not implemented">>}.

-spec no_permission_to_intercept(whapps_call:call()) -> any().
%% TODO: please convert to system_media file (say is not consistent on deployments)
no_permission_to_intercept(Call) ->
    whapps_call_command:answer(Call),
    whapps_call_command:b_say(<<"you have no permission to intercept this call">>, Call).
