%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_device).

-include("../callflow.hrl").

-export([handle/2]).

-import(cf_call_command, [b_bridge/4, wait_for_bridge/1, wait_for_unbridge/0, get_caller_id_option/3, get_caller_id/3]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or 
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> tuple(stop | continue)).
handle(Data, #cf_call{cf_pid=CFPid}=Call) ->    
    {ok, Endpoint} = get_endpoint(Data, Call),
    Timeout = wh_json:get_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT),  
    case b_bridge([Endpoint], Timeout, {undefined, undefined}, Call) of
        {ok, _} ->
            _ = wait_for_unbridge(),
            CFPid ! { stop };
        {error, _} ->
            CFPid ! { continue }
    end.   

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fetches a endpoint defintion from the database and returns the
%% whistle_api endpoint json
%% @end
%%--------------------------------------------------------------------
-spec(get_endpoint/2 :: (JObj :: json_object(), Db :: binary()) -> tuple(ok, json_object()) | tuple(error, atom())).
get_endpoint({struct, _}=EP, #cf_call{account_db=Db}=Call) ->
    Id = wh_json:get_value(<<"id">>, EP),
    case couch_mgr:get_results(Db, <<"devices/listing_with_owner">>, [{<<"include_docs">>, true}, {<<"key">>, Id}]) of
        {ok, [JObj]} ->
            Device = wh_json:get_value([<<"value">>, <<"device">>], JObj, ?EMPTY_JSON_OBJECT),
            Owner = wh_json:get_value([<<"value">>, <<"doc">>], JObj, ?EMPTY_JSON_OBJECT),
            %% Get the internal caller id for this devices
            {CallerNumber, CallerName} = get_caller_id(Id, Call),            
            {CalleeNumber, CalleeName} = get_callee_id(Id, Call),            
            Endpoint = [
                         {<<"Invite-Format">>, wh_json:get_value([<<"sip">>, <<"invite_format">>], Device)}
                        ,{<<"To-User">>, wh_json:get_value([<<"sip">>, <<"username">>], Device)}
                        ,{<<"To-Realm">>, wh_json:get_value([<<"sip">>, <<"realm">>], Device)}
                        ,{<<"To-DID">>, wh_json:get_value([<<"sip">>, <<"number">>], Device)}
                        ,{<<"Route">>, wh_json:get_value([<<"sip">>, <<"route">>], Device)}
                        ,{<<"Caller-ID-Number">>, CallerNumber}
                        ,{<<"Caller-ID-Name">>, CallerName}
                        ,{<<"Callee-ID-Number">>, CalleeNumber}
                        ,{<<"Callee-ID-Name">>, CalleeName}
                        ,{<<"Ignore-Early-Media">>, wh_json:get_value([<<"media">>, <<"ignore_early_media">>], Device)}
                        ,{<<"Bypass-Media">>, wh_json:get_value([<<"media">>, <<"bypass_media">>], Device)}
                        ,{<<"Endpoint-Progress-Timeout">>, wh_json:get_value([<<"media">>, <<"progress_timeout">>], EP, <<"6">>)}
                        ,{<<"Codecs">>, wh_json:get_value([<<"media">>, <<"codecs">>], Device)}
                        ,{<<"Custom-Channel-Vars">>, get_custom_channel_vars(Device, Owner)}
                    ],
            {ok, {struct, [ KV || {_, V}=KV <- Endpoint, V =/= undefined ]} };
        {error, _}=E ->
            logger:format_log(error, "CF_DEVICES(~p): Could not locate endpoint ~p in ~p (~p)~n", [self(), Id, Db, E]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will collect and set the appropriate custom channel
%% vars.  It is very important that Custom-Channel-Vars never be
%% set to an empty json object! 
%% @end
%%--------------------------------------------------------------------
-spec(get_custom_channel_vars/2 :: (Device :: json_object(), Owner :: json_object()) -> proplist() | undefined).
get_custom_channel_vars(Device, Owner) ->
    case wh_json:get_value([<<"sip">>, <<"realm">>], Device) of 
        undefined ->
            undefined;
        Realm ->
            {struct, [{<<"Realm">>, Realm}]}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If the call is from a carrier (ie PSTN) then the callee is the same
%% as the caller id.  If the call is from a local endpoint, then the
%% callee of the bridge (the b-leg) is the devices being bridged to.
%% @end
%%--------------------------------------------------------------------
-spec(get_callee_id/2 :: (Id :: binary(), Call :: #cf_call{}) -> tuple(binary(), binary())).
get_callee_id(Id, #cf_call{authorizing_id=undefined}=Call) ->
    get_caller_id(Id, Call);
get_callee_id(Id, Call) ->
    cf_call_command:get_caller_id(Id, <<"internal">>, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If the call is from a carrier (ie PSTN) then the caller id is 
%% the reformated caller id recieved from the carrier.  Otherwise,
%% the caller id of the bridge (the b-leg) is the device that started
%% the call.
%%
%% NOTE:
%% If there was no authorizing_id then this call originated externally
%% (ie carrier) at least for now... Eventually we will be doing the 
%% ACL internally then we will be given an id for external auth.
%% @end
%%--------------------------------------------------------------------
-spec(get_caller_id/2 :: (Id :: binary(), Call :: #cf_call{}) -> tuple(binary(), binary())).
get_caller_id(Id, #cf_call{cid_number=Number, cid_name=Name, authorizing_id=undefined}=Call) ->
    try        
        Reformat = get_caller_id_option(Id, <<"reformat">>, Call),
        {match, Positions} = re:run(Number, Reformat),
        %% find the largest matching group if present by sorting the position of the 
        %% matching groups by list, reverse so head is largest, then take the head of the list
        {Start, End} = hd(lists:reverse(lists:keysort(2, tl(Positions)))),
        {binary:part(Number, Start, End), Name}
    catch
        _:_ ->
            {Number, Name}
    end;
get_caller_id(_, #cf_call{authorizing_id=Id}=Call) ->
    cf_call_command:get_caller_id(Id, <<"internal">>, Call).
