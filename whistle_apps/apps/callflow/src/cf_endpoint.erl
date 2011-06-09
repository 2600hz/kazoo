%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 6 May 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_endpoint).

-include("callflow.hrl").

-export([build/2]).
-export([get_invite_format/2]).
-export([get_to_user/2]).
-export([get_to_realm/2]).
-export([get_to_did/2]).
-export([get_route/2]).
-export([get_ignore_early_media/2]).
-export([get_bypass_media/2]).
-export([get_endpoint_progress_timeout/2]).
-export([get_codecs/2]).
-export([get_custom_channel_vars/2]).
-export([get_caller_id/2, get_callee_id/2]).
-export([get_endpoint_caller_ids/2, get_endpoint_caller_id/3]).
-export([get_endpoint_caller_id_options/2, get_endpoint_caller_id_option/3]).

-define(VIEW_WITH_OWNER, <<"devices/listing_with_owner">>).
-define(VIEW_CALLER_ID, <<"caller_id/find_caller_id">>).
-define(VIEW_CALLER_ID_OPTIONS, <<"caller_id/find_caller_id_options">>).
-define(CONFIRM_FILE, <<"/opt/freeswitch/sounds/en/us/callie/ivr/8000/ivr-accept_reject_voicemail.wav">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetches a endpoint defintion from the database and returns the
%% whistle_api endpoint json
%% @end
%%--------------------------------------------------------------------
-spec(build/2 :: (Id :: binary(), Call :: #cf_call{}) -> tuple(ok, json_object()) | tuple(error, atom())).
build(Id, #cf_call{account_db=Db}=Call) ->
    case couch_mgr:get_results(Db, ?VIEW_WITH_OWNER, [{<<"include_docs">>, true}, {<<"key">>, Id}]) of
        {ok, [JObj]} ->
            Endpoint = wh_json:get_value([<<"value">>, <<"device">>], JObj, ?EMPTY_JSON_OBJECT),
            Owner = wh_json:get_value(<<"doc">>, JObj, ?EMPTY_JSON_OBJECT),
            {CallerNumber, CallerName} = get_caller_id(Id, Call),            
            {CalleeNumber, CalleeName} = get_callee_id(Id, Call),            
            Prop = [
                     {<<"Invite-Format">>, get_invite_format(Endpoint, Owner)}
                    ,{<<"To-User">>, get_to_user(Endpoint, Owner)}
                    ,{<<"To-Realm">>, get_to_realm(Endpoint, Owner)}
                    ,{<<"To-DID">>, get_to_did(Endpoint, Owner)}
                    ,{<<"Route">>, get_route(Endpoint, Owner)}
                    ,{<<"Caller-ID-Number">>, CallerNumber}
                    ,{<<"Caller-ID-Name">>, CallerName}
                    ,{<<"Callee-ID-Number">>, CalleeNumber}
                    ,{<<"Callee-ID-Name">>, CalleeName}
                    ,{<<"Ignore-Early-Media">>, get_ignore_early_media(Endpoint, Owner)}
                    ,{<<"Bypass-Media">>, get_bypass_media(Endpoint, Owner)}
                    ,{<<"Endpoint-Progress-Timeout">>, get_endpoint_progress_timeout(Endpoint, Owner)}
                    ,{<<"Codecs">>, get_codecs(Endpoint, Owner)}
                    ,{<<"Custom-Channel-Vars">>, get_custom_channel_vars(Endpoint, Owner)}
                   ],            
            {ok, {struct, [ KV || {_, V}=KV <- Prop, V =/= undefined ]} };
        {error, _}=E ->
            logger:format_log(error, "CF_CALL_UTILS(~p): Could not locate endpoint ~p in ~p (~p)~n", [self(), Id, Db, E]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get the endpoint format but override it with 'route' if the owner
%% of the endpoint has call forwarding enabled
%% @end
%%--------------------------------------------------------------------
-spec(get_invite_format/2 :: (Endpoint :: json_object(), Owner :: json_object()) -> binary() | undefined).
get_invite_format(Endpoint, Owner) ->
    case whistle_util:is_true(wh_json:get_value([<<"call_forward">>, <<"enabled">>], Owner)) of
        false -> wh_json:get_value([<<"sip">>, <<"invite_format">>], Endpoint, <<"username">>);
        true -> <<"route">>
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get the endpoint to user
%% @end
%%--------------------------------------------------------------------
-spec(get_to_user/2 :: (Endpoint :: json_object(), Owner :: json_object()) -> binary() | undefined).
get_to_user(Endpoint, _) ->
    wh_json:get_value([<<"sip">>, <<"username">>], Endpoint).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get the endpoint to user
%% @end
%%--------------------------------------------------------------------
-spec(get_to_realm/2 :: (Endpoint :: json_object(), Owner :: json_object()) -> binary() | undefined).
get_to_realm(Endpoint, _) ->
    wh_json:get_value([<<"sip">>, <<"realm">>], Endpoint).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get the endpoint to user
%% @end
%%--------------------------------------------------------------------
-spec(get_to_did/2 :: (Endpoint :: json_object(), Owner :: json_object()) -> binary() | undefined).
get_to_did(Endpoint, _) ->
    wh_json:get_value([<<"sip">>, <<"number">>], Endpoint).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get the endpoint route but override it if the owner has call 
%% forwarding enabled
%% @end
%%--------------------------------------------------------------------
-spec(get_route/2 :: (Endpoint :: json_object(), Owner :: json_object()) -> binary() | undefined).
get_route(Endpoint, Owner) ->
    case whistle_util:is_true(wh_json:get_value([<<"call_forward">>, <<"enabled">>], Owner)) of
        false -> wh_json:get_value([<<"sip">>, <<"route">>], Endpoint);
        true -> <<"loopback/", (wh_json:get_value([<<"call_forward">>, <<"number">>], Owner, <<"unknown">>))/binary>>
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get the ignore_early_media option but override it if the owner of
%% the endpoint has call forwarding has is enabled
%% @end
%%--------------------------------------------------------------------
-spec(get_ignore_early_media/2 :: (Endpoint :: json_object(), Owner :: json_object()) -> binary() | undefined).
get_ignore_early_media(Endpoint, Owner) ->
    case whistle_util:is_true(wh_json:get_value([<<"call_forward">>, <<"enabled">>], Owner)) of
        false -> wh_json:get_value([<<"media">>, <<"ignore_early_media">>], Endpoint);
        true ->
            whistle_util:to_binary(
              whistle_util:is_true(
                wh_json:get_value([<<"call_forward">>, <<"require_keypress">>], Owner)
               )
             )
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get the endpoint setting for bypass media mode
%% @end
%%--------------------------------------------------------------------
-spec(get_bypass_media/2 :: (Endpoint :: json_object(), Owner :: json_object()) -> binary() | undefined).
get_bypass_media(Endpoint, _) ->
    wh_json:get_value([<<"media">>, <<"bypass_media">>], Endpoint).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get the endpoint setting for progress timeout
%% @end
%%--------------------------------------------------------------------
-spec(get_endpoint_progress_timeout/2 :: (Endpoint :: json_object(), Owner :: json_object()) -> binary() | undefined).
get_endpoint_progress_timeout(Endpoint, _) ->
    wh_json:get_value([<<"media">>, <<"progress_timeout">>], Endpoint, <<"6">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get the endpoint setting for codecs
%% @end
%%--------------------------------------------------------------------
-spec(get_codecs/2 :: (Endpoint :: json_object(), Owner :: json_object()) -> binary() | undefined).
get_codecs(Endpoint, _) ->    
    wh_json:get_value([<<"media">>, <<"codecs">>], Endpoint).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will collect and set the appropriate custom channel
%% vars.  It is very important that Custom-Channel-Vars never be
%% set to an empty json object! 
%% @end
%%--------------------------------------------------------------------
-spec(get_custom_channel_vars/2 :: (Endpoint :: json_object(), Owner :: json_object()) -> proplist() | undefined).
get_custom_channel_vars(Endpoint, Owner) ->
    Realm = wh_json:get_value([<<"sip">>, <<"realm">>], Endpoint),
    case { whistle_util:is_true(wh_json:get_value([<<"call_forward">>, <<"enabled">>], Owner))
          ,whistle_util:is_true(wh_json:get_value([<<"call_forward">>, <<"require_keypress">>], Owner)) } of
        {false, _} when Realm =:= undefined ->
            undefined;
        {false, _} ->
            {struct, [{<<"Realm">>, Realm}]};
        {true, false} when Realm =:= undefined ->
            {struct, [
                       {<<"Call-Forward">>, <<"true">>}
                      ,{<<"CF-Keep-Caller-ID">>, wh_json:get_value([<<"call_forward">>, <<"keep_caller_id">>], Owner)}
                     ]};
        {true, false} ->
            {struct, [
                       {<<"Realm">>, Realm}
                      ,{<<"Call-Forward">>, <<"true">>}
                      ,{<<"CF-Keep-Caller-ID">>, wh_json:get_value([<<"call_forward">>, <<"keep_caller_id">>], Owner)}
                     ]};
        {true, true} when Realm =:= undefined ->
            {struct, [
                       {<<"Call-Forward">>, <<"true">>}
                      ,{<<"CF-Keep-Caller-ID">>, wh_json:get_value([<<"call_forward">>, <<"keep_caller_id">>], Owner)}
                      ,{<<"Confirm-Key">>, <<"1">>}
                      ,{<<"Confirm-Cancel-Timeout">>, <<"2">>}
                      ,{<<"Confirm-File">>, ?CONFIRM_FILE}
                     ]};
        {true, true} ->
            {struct, [
                       {<<"Realm">>, Realm}
                      ,{<<"Call-Forward">>, <<"true">>}
                      ,{<<"CF-Keep-Caller-ID">>, wh_json:get_value([<<"call_forward">>, <<"keep_caller_id">>], Owner)}
                      ,{<<"Confirm-Key">>, <<"1">>}
                      ,{<<"Confirm-Cancel-Timeout">>, <<"2">>}
                      ,{<<"Confirm-File">>, ?CONFIRM_FILE}
                     ]}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the call is from a carrier (ie PSTN) then the callee is the same
%% as the caller id.  If the call is from a local endpoint, then the
%% callee of the bridge (the b-leg) is the endpoint being bridged to.
%% @end
%%--------------------------------------------------------------------
-spec(get_callee_id/2 :: (Id :: binary(), Call :: #cf_call{}) -> tuple(binary(), binary())).
get_callee_id(Id, #cf_call{authorizing_id=undefined}=Call) ->
    get_caller_id(Id, Call);
get_callee_id(Id, Call) ->
    get_endpoint_caller_id(Id, <<"internal">>, Call).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the call is from a carrier (ie PSTN) then the caller id is 
%% the reformated caller id recieved from the carrier.  Otherwise,
%% the caller id of the bridge (the b-leg) is the endpoint that
%% started the call.
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
        Reformat = get_endpoint_caller_id_option(Id, <<"reformat">>, Call),
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
    get_endpoint_caller_id(Id, <<"internal">>, Call).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Finds the specific type of caller id beloning to the endpoint or the 
%% account default if no match is found
%% @end
%%--------------------------------------------------------------------
-spec(get_endpoint_caller_id/3 :: (For :: binary(), Type :: binary(), Call :: #cf_call{}) -> 
                                   tuple(binary()|undefined, binary()|undefined)).
get_endpoint_caller_id(_, raw, _) ->
    {undefined, undefined};
get_endpoint_caller_id(For, Type, #cf_call{dest_number=DestNum}=Call) ->
    case props:get_value(Type, get_endpoint_caller_ids(For, Call)) of
        undefined when Type =/= <<"default">> ->             
            get_endpoint_caller_id(For, <<"default">>, Call);
        undefined ->
            {undefined, undefined};
        CID ->
            {wh_json:get_value(<<"number">>, CID, DestNum), wh_json:get_value(<<"name">>, CID)}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Gets a proplist of all the caller ids that belong to the endpoint
%% @end
%%--------------------------------------------------------------------    
-spec(get_endpoint_caller_ids/2 :: (For :: binary(), Call :: #cf_call{}) -> proplist()).
get_endpoint_caller_ids(For, #cf_call{account_db=Db}) ->
    try
        {ok, C1} = wh_cache:fetch({caller_ids, For}), C1        
    catch
        _:_ ->
            case couch_mgr:get_all_results(Db, ?VIEW_CALLER_ID) of 
                {ok, JObj} ->
                    AllTypes = [{wh_json:get_value(<<"key">>, Result), wh_json:get_value(<<"value">>, Result)}
                                || Result <- JObj
                               ],
                    C2 = merge_endpoint_caller_id_types(For, AllTypes, []),
                    wh_cache:store({caller_ids, For}, C2), C2;
                {error, _} ->
                    []
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Follows the caller id tree and merges the caller id types 
%% @end
%%--------------------------------------------------------------------
-spec(merge_endpoint_caller_id_types/3 :: (NodeId :: binary(), AllTypes :: proplist(), 
                                    Types :: proplist()) -> proplist()).
merge_endpoint_caller_id_types(NodeId, AllTypes, Types) when NodeId =:= <<"account">> ->
    Node = props:get_value(NodeId, AllTypes),
    {struct, NodeOpts} = wh_json:get_value(<<"caller_id">>, Node, ?EMPTY_JSON_OBJECT),
    propmerge(Types, NodeOpts);
merge_endpoint_caller_id_types(NodeId, AllTypes, Types) ->
    Node = props:get_value(NodeId, AllTypes),
    {struct, NodeTypes} = wh_json:get_value(<<"caller_id">>, Node, ?EMPTY_JSON_OBJECT),
    merge_endpoint_caller_id_types(
       wh_json:get_value(<<"next">>, Node, <<"account">>)
      ,AllTypes
      ,propmerge(Types, NodeTypes)
     ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Gets the value of a specific caller id option that belong to 'For'
%% @end
%%--------------------------------------------------------------------    
-spec(get_endpoint_caller_id_option/3 :: (For :: binary(), Key :: binary(), Call :: #cf_call{}) -> undefined|binary()).
get_endpoint_caller_id_option(For, Key, Call) ->
    props:get_value(Key, get_endpoint_caller_id_options(For, Call)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Gets a proplist of all caller id options that belong to 'For'
%% @end
%%--------------------------------------------------------------------
-spec(get_endpoint_caller_id_options/2 :: (For :: binary(), Call :: #cf_call{}) -> proplist()).
get_endpoint_caller_id_options(For, #cf_call{account_db=Db}) ->
    try
        {ok, O1} = wh_cache:fetch({caller_id_options, For}), O1
    catch
        _:_ ->
            case couch_mgr:get_all_results(Db, ?VIEW_CALLER_ID_OPTIONS) of 
                {ok, JObj} ->
                    AllOptions = [{wh_json:get_value(<<"key">>, Result), wh_json:get_value(<<"value">>, Result)}
                                  || Result <- JObj
                                 ],
                    O2 = merge_endpoint_caller_id_options(For, AllOptions, []),
                    logger:format_log(info, "Loading caller-id options for ~p with ~p", [For, O2]),
                    wh_cache:store({caller_id_options, For}, O2), O2;
                {error, _} ->
                    []
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Follows the caller id options tree and merges them into one proplist
%% @end
%%--------------------------------------------------------------------
-spec(merge_endpoint_caller_id_options/3 :: (NodeId :: binary(), AllOptions :: proplist(), 
                                    Options :: proplist()) -> proplist()).
merge_endpoint_caller_id_options(NodeId, AllOptions, Options) when NodeId =:= <<"account">> ->
    Node = props:get_value(NodeId, AllOptions),
    {struct, NodeOpts} = wh_json:get_value(<<"caller_id_options">>, Node, ?EMPTY_JSON_OBJECT),
    propmerge(Options, NodeOpts);
merge_endpoint_caller_id_options(NodeId, AllOptions, Options) ->
    Node = props:get_value(NodeId, AllOptions),
    {struct, NodeOpts} = wh_json:get_value(<<"caller_id_options">>, Node, ?EMPTY_JSON_OBJECT),
    merge_endpoint_caller_id_options(
       wh_json:get_value(<<"next">>, Node, <<"account">>)
      ,AllOptions
      ,propmerge(Options, NodeOpts)
     ).

%% TAKEN FOR COUCHBEAM_UTIL.ERL (5/3/2011)
%% released under the MIT license

%% @doc merge 2 proplists. All the Key - Value pairs from both proplists
%% are included in the new proplists. If a key occurs in both dictionaries 
%% then Fun is called with the key and both values to return a new
%% value. This a wreapper around dict:merge
propmerge(F, L1, L2) ->    
    dict:to_list(dict:merge(F, dict:from_list(L1), dict:from_list(L2))).

%% @doc Update a proplist with values of the second. In case the same
%% key is in 2 proplists, the value from the first are kept.
propmerge(L1, L2) ->   
    propmerge(fun(_, V1, _) ->
                       V1 end, L1, L2).
