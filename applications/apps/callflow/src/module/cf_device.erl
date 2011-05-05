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
    case b_bridge([Endpoint], Timeout, format_caller_id(Data, Call), Call) of
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
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            {CalleeNumber, CalleeName} = get_caller_id(Id, <<"internal">>, Call),            
            Endpoint = [
                         {<<"Invite-Format">>, wh_json:get_value([<<"sip">>, <<"invite_format">>], JObj)}
                        ,{<<"To-User">>, wh_json:get_value([<<"sip">>, <<"username">>], JObj)}
                        ,{<<"To-Realm">>, wh_json:get_value([<<"sip">>, <<"realm">>], JObj)}
                        ,{<<"To-DID">>, wh_json:get_value([<<"sip">>, <<"number">>], JObj)}
                        ,{<<"Route">>, wh_json:get_value([<<"sip">>, <<"route">>], JObj)}
                        ,{<<"Callee-ID-Number">>, CalleeNumber}
                        ,{<<"Callee-ID-Name">>, CalleeName}
                        ,{<<"Ignore-Early-Media">>, wh_json:get_value([<<"media">>, <<"ignore_early_media">>], JObj)}
                        ,{<<"Bypass-Media">>, wh_json:get_value([<<"media">>, <<"bypass_media">>], JObj)}
                        ,{<<"Endpoint-Progress-Timeout">>, wh_json:get_value([<<"media">>, <<"progress_timeout">>], EP, <<"6">>)}
                        ,{<<"Codecs">>, wh_json:get_value([<<"media">>, <<"codecs">>], JObj)}
                    ],
            {ok, {struct, [ KV || {_, V}=KV <- Endpoint, V =/= undefined ]} };
        {error, _}=E ->
            logger:format_log(error, "CF_DEVICES(~p): Could not locate endpoint ~p in ~p (~p)~n", [self(), Id, Db, E]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will either look up the internal caller id or
%% optionally reformat the provided.
%%
%% NOTE:
%% If there was no authorizing_id then this call originated externally
%% (ie carrier) at least for now... Eventually we will be doing the 
%% ACL internally then we will be given an id for external auth.
%% @end
%%--------------------------------------------------------------------
format_caller_id(Data, #cf_call{cid_name=Name, cid_number=Number, authorizing_id=undefined}=Call) ->
    try        
        Id = wh_json:get_value(<<"id">>, Data),
        Reformat = get_caller_id_option(Id, <<"reformat">>, Call),
        {match, Positions} = re:run(Number, Reformat),
        %% find the largest matching group if present by sorting the position of the 
        %% matching groups by list, reverse so head is largest, then take the head of the list
        {Start, End} = hd(lists:reverse(lists:keysort(2, tl(Positions)))),
        {binary:part(Number, Start, End), Name}
    catch
        _:_ ->
            raw
    end;
format_caller_id(_, _) ->
    <<"internal">>.
