%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_call_forward).

-include("../callflow.hrl").

-export([handle/2]).

-import(cf_call_command, [b_bridge/6, wait_for_bridge/1, wait_for_unbridge/0, set/3, b_fetch/2]).
 
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
    case wh_json:get_value(<<"action">>, Data) of 
        <<"activate">> ->
           cf_activation(Call),
            cf_call_command:b_play(<<"/system_media/ivr-call_forwarding_has_been_set">>, Call),
            CFPid ! {stop};
        <<"deactivate">> ->
            cf_deactivation(Call),
            cf_call_command:b_play(<<"/system_media/ivr-call_forwarding_has_been_cancelled">>, Call),
            CFPid ! {stop}
    end.

-spec(cf_activation/1 :: (Call :: #cf_call{}) -> no_return()).
cf_activation(#cf_call{authorizing_id=Id, account_db=Db}) ->
    {ok, [JObj]} = couch_mgr:get_results(Db, <<"devices/listing_with_owner">>, [{<<"include_docs">>, true}, {<<"key">>, Id}]),
    User = wh_json:set_value([<<"call_forward">>, <<"enabled">>], true, wh_json:get_value(<<"doc">>, JObj)),
    ok = update_user_doc(Db, User).    

-spec(cf_deactivation/1 :: (Call :: #cf_call{}) -> no_return()).
cf_deactivation(#cf_call{authorizing_id=Id, account_db=Db}) ->
    {ok, [JObj]} = couch_mgr:get_results(Db, <<"devices/listing_with_owner">>, [{<<"include_docs">>, true}, {<<"key">>, Id}]),
    User = wh_json:set_value([<<"call_forward">>, <<"enabled">>], false, wh_json:get_value(<<"doc">>, JObj)),
    ok = update_user_doc(Db, User).
    
update_user_doc(Db, User) ->
    case couch_mgr:save_doc(Db, User) of 
        {error, conflict} ->
            {ok, Rev} = couch_mgr:lookup_doc_rev(Db, wh_json:get_value(<<"_id">>, User)),
            update_user_doc(Db, wh_json:set_value(<<"_rev">>, Rev));
        {ok, _} ->
            ok;
        {error, _} ->
            error
    end.
