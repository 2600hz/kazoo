%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz INC
%%% @doc
%%% Pickup a call in the specified group/device/user/extension
%%% 
%%% data: {
%%%   "type" : "group | user | device | extension"
%%% }
%%%
%%% uses cf_capture_group and type to build parameters to branch to cf_group_pickup
%%%
%%% group -> lookup groups by number (WIP)  
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
-module(cf_group_pickup_feature).

-include("../callflow.hrl").

-export([handle/2]).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, creates the parameters and branches
%% to cf_group_pickup. 
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:json_object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Number = whapps_call:kvs_fetch(cf_capture_group, Call),
	PickupType = wh_json:get_value(<<"type">>, Data),
	case build_pickup_params(Number, PickupType, Call) of
		{'ok', {Param, Value}} ->
			Flow = wh_json:from_list([{<<"module">>,<<"group_pickup">>},{<<"data">>,wh_json:from_list([{Param,Value}])}]),
			cf_exe:branch(Flow, Call);
		{'error', E} -> 
			lager:info("Error <<~s>> processing pickup '~s' for number ~s",[E,PickupType,Number]),
			_ = whapps_call_command:b_play(<<"park-no_caller">>, Call),
			cf_exe:stop(Call)
	end.

-spec build_pickup_params(ne_binary(), ne_binary(), whapps_call:call()) -> {'ok',{ne_binary(),ne_binary()}} | {'error', ne_binary()}.

build_pickup_params(Number,<<"device">>, Call) ->
	AccountDb = whapps_call:account_db(Call),
	case cf_util:endpoint_id_by_sip_username(AccountDb, Number) of
		{'ok',EndpointId} -> {'ok',{<<"device_id">>,EndpointId}};
		{'error', _ }=E -> E
	end;

build_pickup_params(_Number,<<"user">>, _Call) ->
	{'error', <<"work in progress">>};

build_pickup_params(_Number,<<"group">>, _Call) ->
	{'error', <<"work in progress">>};

build_pickup_params(Number,<<"extension">>, Call) ->
	AccountId = whapps_call:account_id(Call),
	case cf_util:lookup_callflow(Number, AccountId) of
		{'ok', FlowDoc, 'false' } ->
			Data = wh_json:get_value([<<"flow">>,<<"data">>],FlowDoc),
			Module = wh_json:get_value([<<"flow">>,<<"module">>],FlowDoc),
			params_from_data(Module, Data,Call);
		{'ok', _FlowDoc, 'true' } ->	
			{'error', <<"no callflow with extension ",Number/binary>>};
		{'error', _} = E -> E
	end;

build_pickup_params(_ ,'undefined', _ ) ->
	{'error',<<"parameter 'type' not defined">>};
build_pickup_params(_, Other, _) ->
	{'error', <<Other/binary," not implemented">>}.



-spec params_from_data(ne_binary(), wh_json:object(), whapps_call:call()) -> {'ok',{ne_binary(),ne_binary()}} | {'error', ne_binary()}.
params_from_data(<<"user">>, Data, _Call) ->
	EndpointId = wh_json:get_value(<<"id">>,Data),
	{'ok',{<<"user_id">>,EndpointId}};

params_from_data(<<"device">>, Data, _Call) ->
	EndpointId = wh_json:get_value(<<"id">>,Data),
	{'ok',{<<"device_id">>,EndpointId}};			

params_from_data(<<"ring_group">>, Data, _Call) ->
	lager:info("ring_group"),
	[Endpoint|_Endpoints] = wh_json:get_value(<<"endpoints">>,Data,[]),
	EndpointType = wh_json:get_value(<<"endpoint_type">>,Endpoint),
	{'ok',{<<EndpointType/binary,"_id">>,wh_json:get_value(<<"id">>,Endpoint)}};

params_from_data(<<"page_group">>, Data, _Call) ->
	params_from_data(<<"ring_group">>, Data, _Call);

params_from_data('undefined', _, _) ->
	{'error',<<"module not defined in callflow">>};

params_from_data(Other, _, _) ->
	{'error',<<"module ",Other/binary," not implemented">>}.

