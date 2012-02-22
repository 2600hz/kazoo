%%%============================================================================
%%% @copyright (C) 2011 VoIP Inc
%%% @doc
%%%
%%% @end
%%% Created : 28 Jun 2011 by Karl Anderson <karl@2600hz.org>
%%%============================================================================
-module(whapps_call).

-include_lib("whistle/include/wh_types.hrl").

-export([new/0]).
-export([flush/0, cache/1, cache/2, retrieve/1]).

-export([exec/2]).
-export([to_proplist/1]).

-export([from_route_req/1, from_route_req/2]).
-export([from_route_win/1, from_route_win/2]).

-export([set_application_name/2, application_name/1]).
-export([set_application_version/2, application_version/1]).
-export([set_call_id/2, call_id/1]).
-export([set_control_queue/2, control_queue/1]).
-export([set_controller_queue/2, controller_queue/1]).

-export([set_caller_id_name/2, caller_id_name/1]).
-export([set_caller_id_number/2, caller_id_number/1]).
-export([set_callee_id_name/2, callee_id_name/1]).
-export([set_callee_id_number/2, callee_id_number/1]).

-export([set_request/2, request/1, request_user/1, request_realm/1]).
-export([set_from/2, from/1, from_user/1, from_realm/1]).
-export([set_to/2, to/1, to_user/1, to_realm/1]).

-export([set_account_db/2, account_db/1]).
-export([set_account_id/2, account_id/1]).

-export([set_inception/2, inception/1]).

-export([set_authorizing_id/2, authorizing_id/1]).
-export([set_authorizing_type/2, authorizing_type/1]).

-export([set_custom_channel_var/3, update_custom_channel_vars/2
         ,custom_channel_var/3, custom_channel_var/2
         ,custom_channel_vars/1
        ]).

-export([kvs_append/3
         ,kvs_append_list/3
         ,kvs_erase/2
         ,kvs_fetch/2
         ,kvs_fetch_keys/2
         ,kvs_filter/2
         ,kvs_find/2
         ,kvs_fold/3
         ,kvs_from_proplist/2
         ,kvs_is_key/2
         ,kvs_map/2
         ,kvs_store/3
         ,kvs_store_proplist/2
         ,kvs_to_proplist/1
         ,kvs_update/3
         ,kvs_update/4
         ,kvs_update_counter/3
        ]).

-record (whapps_call, {call_id = undefined :: 'undefined' | ne_binary()                                %% The UUID of the call
                       ,control_q = 'undefined' :: 'undefined' | ne_binary()                           %% The control queue provided on route win
                       ,controller_q = 'undefined' :: 'undefined' | ne_binary()                        %%  
                       ,caller_id_name = <<"Unknown">> :: binary()                                     %% The caller name
                       ,caller_id_number = <<"0000000000">> :: binary()                                %% The caller number
                       ,callee_id_name = <<>> :: binary()                                              %% The callee name
                       ,callee_id_number = <<>> :: binary()                                            %% The callee number
                       ,request = <<"nouser@norealm">> :: ne_binary()                                  %% The request of sip_request_user + @ + sip_request_host
                       ,request_user = <<"nouser">> :: ne_binary()                                     %% SIP request user
                       ,request_realm = <<"norealm">> :: ne_binary()                                   %% SIP request host
                       ,from = <<"nouser@norealm">> :: ne_binary()                                     %% Result of sip_from_user + @ + sip_from_host
                       ,from_user = <<"nouser">> :: ne_binary()                                        %% SIP from user
                       ,from_realm = <<"norealm">> :: ne_binary()                                      %% SIP from host
                       ,to = <<"nouser@norealm">> :: ne_binary()                                       %% Result of sip_to_user + @ + sip_to_host
                       ,to_user = <<"nouser">> :: ne_binary()                                          %% SIP to user
                       ,to_realm = <<"norealm">> :: ne_binary()                                        %% SIP to host
                       ,inception = 'undefined' :: 'undefined' | ne_binary()                           %% Origin of the call <<"on-net">> | <<"off-net">>
                       ,account_db = 'undefined' :: 'undefined'| ne_binary()                           %% The database name of the account that authorized this call
                       ,account_id = 'undefined' :: 'undefined' | ne_binary()                          %% The account id that authorized this call
                       ,authorizing_id = 'undefined' :: 'undefined' | ne_binary()                      %% The ID of the record that authorized this call
                       ,authorizing_type = 'undefined' :: 'undefined' | ne_binary()                    %% The pvt_type of the record that authorized this call
                       ,app_name = <<"whapps_call">> :: ne_binary()                                    %% The application name used during whapps_call_command
                       ,app_version = <<"1.0.0">> :: ne_binary()                                       %% The application version used during whapps_call_command
                       ,ccvs = wh_json:new() :: wh_json:json_object()                                  %% Any custom channel vars that where provided with the route request
                       ,kvs = orddict:new() :: orddict:orddict()                                       %% allows callflows to set values that propogate to children
                      }).

-opaque call() :: call().
-export_type([call/0]).

-define(APP_NAME, <<"whapps_call">>).
-define(APP_VERSION, <<"1.0.0">>).

-spec new/0 :: () -> call().
new() ->
    #whapps_call{}.

-spec flush/0 :: () -> 'ok'.
flush() ->
    {ok, Cache} = whistle_apps_sup:whapps_call_cache_proc(),
    wh_cache:local_flush(Cache).

-spec cache/1 :: (call()) -> 'ok'.
-spec cache/2 :: (call(), pos_integer()) -> 'ok'.

cache(#whapps_call{}=Call) ->
    cache(Call, 300000).
    
cache(#whapps_call{call_id=CallId}=Call, Expires) ->
    {ok, Cache} = whistle_apps_sup:whapps_call_cache_proc(),
    wh_cache:store_local(Cache, {?MODULE, call, CallId}, Call, Expires).

-spec retrieve/1 :: (ne_binary()) -> {'ok', call()} | {'error', 'not_found'}.
retrieve(CallId) ->
    {ok, Cache} = whistle_apps_sup:whapps_call_cache_proc(),
    wh_cache:fetch_local(Cache, {?MODULE, call, CallId}).

-spec exec/2 :: ([fun((call()) -> call()),...], call()) -> call().
exec(Funs, #whapps_call{}=Call) ->
    lists:foldr(fun(F, C) -> F(C) end, Call, Funs).

-spec to_proplist/1 :: (#whapps_call{}) -> proplist().
to_proplist(#whapps_call{}=Call) ->
    [{wh_util:to_binary(K), V}
     || {K, V} <- lists:zip(record_info(fields, whapps_call), tl(tuple_to_list(Call)))
            ,K =/= kvs
    ].    
  
-spec from_route_req/1 :: (wh_json:json_object()) -> call().
from_route_req(RouteReq) ->
    from_route_req(RouteReq, #whapps_call{}).

-spec from_route_req/2 :: (wh_json:json_object(), call()) -> call().
from_route_req(RouteReq, #whapps_call{}=Call) ->
    CallId = wh_json:get_value(<<"Call-ID">>, RouteReq, Call#whapps_call.call_id),
    put(callid, CallId),

    CCVs = wh_json:merge_recursive(Call#whapps_call.ccvs, wh_json:get_value(<<"Custom-Channel-Vars">>, RouteReq, wh_json:new())),
    Request = wh_json:get_value(<<"Request">>, RouteReq, Call#whapps_call.request),
    From = wh_json:get_value(<<"From">>, RouteReq, Call#whapps_call.from),
    To = wh_json:get_value(<<"To">>, RouteReq, Call#whapps_call.to),
    Inception = case wh_json:get_value(<<"Inception">>, CCVs) of
                    <<"on-net">> -> <<"on-net">>;
                    <<"off-net">> -> <<"off-net">>;
                    _Else -> Call#whapps_call.inception
                end,
    AccountId = wh_json:get_value(<<"Account-ID">>, CCVs, Call#whapps_call.account_id),
    AccountDb = case is_binary(AccountId) of
                    false -> Call#whapps_call.account_db;
                    true ->  wh_util:format_account_id(AccountId, encoded)
                end,
    [ToUser, ToRealm] = binary:split(To, <<"@">>),
    [FromUser, FromRealm] = binary:split(From, <<"@">>),
    [RequestUser, RequestRealm] = binary:split(Request, <<"@">>),
    
    Call#whapps_call{call_id=CallId
                     ,request=Request
                     ,request_user=wnm_util:to_e164(RequestUser)
                     ,request_realm=RequestRealm
                     ,from=From
                     ,from_user=FromUser
                     ,from_realm=FromRealm
                     ,to=To
                     ,to_user=ToUser
                     ,to_realm=ToRealm
                     ,inception=Inception
                     ,account_id=AccountId
                     ,account_db=AccountDb
                     ,authorizing_id = wh_json:get_ne_value(<<"Authorizing-ID">>, CCVs, Call#whapps_call.authorizing_id)
                     ,authorizing_type = wh_json:get_ne_value(<<"Authorizing-Type">>, CCVs, Call#whapps_call.authorizing_type)
                     ,caller_id_name = wh_json:get_value(<<"Caller-ID-Name">>, RouteReq, Call#whapps_call.caller_id_name)
                     ,caller_id_number = wh_json:get_value(<<"Caller-ID-Number">>, RouteReq, Call#whapps_call.caller_id_number)
                     ,ccvs = CCVs
                    }.

from_route_win(RouteWin) ->
    from_route_win(RouteWin, #whapps_call{}). 

from_route_win(RouteWin, #whapps_call{}=Call) ->
    CallId = wh_json:get_value(<<"Call-ID">>, RouteWin, Call#whapps_call.call_id),
    put(callid, CallId),

    CCVs = wh_json:merge_recursive(Call#whapps_call.ccvs, wh_json:get_value(<<"Custom-Channel-Vars">>, RouteWin, wh_json:new())),
    Inception = case wh_json:get_value(<<"Inception">>, CCVs) of
                    <<"on-net">> -> <<"on-net">>;
                    <<"off-net">> -> <<"off-net">>;
                    _Else -> Call#whapps_call.inception
                end,
    AccountId = wh_json:get_value(<<"Account-ID">>, CCVs, Call#whapps_call.account_id),
    AccountDb = case is_binary(AccountId) of
                    false -> Call#whapps_call.account_db;
                    true ->  wh_util:format_account_id(AccountId, encoded)
                end,

    Call#whapps_call{call_id=CallId
                     ,control_q=wh_json:get_value(<<"Control-Queue">>, RouteWin)
                     ,inception=Inception
                     ,account_id=AccountId
                     ,account_db=AccountDb
                     ,authorizing_id=wh_json:get_ne_value(<<"Authorizing-ID">>, CCVs, Call#whapps_call.authorizing_id)
                     ,authorizing_type=wh_json:get_ne_value(<<"Authorizing-Type">>, CCVs, Call#whapps_call.authorizing_type)
                     ,ccvs = CCVs
                    }.    

-spec set_application_name/2 :: (ne_binary(), call()) -> call().
set_application_name(AppName, #whapps_call{}=Call) when is_binary(AppName) ->
    Call#whapps_call{app_name=AppName}.

-spec application_name/1 :: (call()) -> ne_binary().
application_name(#whapps_call{app_name=AppName}) ->
    AppName.

-spec set_application_version/2 :: (ne_binary(), call()) -> call().
set_application_version(AppVersion, #whapps_call{}=Call) when is_binary(AppVersion) ->
    Call#whapps_call{app_version=AppVersion}.

-spec application_version/1 :: (call()) -> ne_binary().
application_version(#whapps_call{app_version=AppVersion}) ->
    AppVersion.

-spec set_call_id/2 :: (ne_binary(), call()) -> call().
set_call_id(CallId, #whapps_call{}=Call) when is_binary(CallId) ->
    Call#whapps_call{call_id=CallId}.

-spec call_id/1 :: (call()) -> 'undefined' | binary().
call_id(#whapps_call{call_id=CallId}) ->
    CallId.

-spec set_control_queue/2 :: (ne_binary(), call()) -> call().
set_control_queue(ControlQ, #whapps_call{}=Call) when is_binary(ControlQ) ->
    Call#whapps_call{control_q=ControlQ}.

-spec control_queue/1 :: (call()) -> 'undefined' | binary().
control_queue(#whapps_call{control_q=ControlQ}) ->
    ControlQ.

-spec set_controller_queue/2 :: (ne_binary(), call()) -> call().
set_controller_queue(ControllerQ, #whapps_call{call_id=CallId, control_q=CtrlQ}=Call) when is_binary(ControllerQ) ->
    spawn(fun() when is_binary(CtrlQ) ->
                  Props = [{<<"Call-ID">>, CallId}
                           ,{<<"Controller-Queue">>, ControllerQ}
                           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                          ],
                  wapi_call:publish_controller_queue(CtrlQ, Props);
             () ->
                  ok
          end),
    Call#whapps_call{controller_q=ControllerQ}.

-spec controller_queue/1 :: (call()) -> 'undefined' | binary().
controller_queue(#whapps_call{controller_q=ControllerQ}) ->
    ControllerQ.

-spec set_caller_id_name/2 :: (ne_binary(), call()) -> call().
set_caller_id_name(CIDName, #whapps_call{}=Call) when is_binary(CIDName) ->
    io:format("set caller id name: ~p~n", [CIDName]),
    Call#whapps_call{caller_id_name=CIDName}.

-spec caller_id_name/1 :: (call()) -> binary().
caller_id_name(#whapps_call{caller_id_name=CIDName}) ->
    CIDName.

-spec set_caller_id_number/2 :: (ne_binary(), call()) -> call().
set_caller_id_number(CIDNumber, #whapps_call{}=Call) when is_binary(CIDNumber) ->
    io:format("set caller id number: ~p~n", [CIDNumber]),
    Call#whapps_call{caller_id_number=CIDNumber}.

-spec caller_id_number/1 :: (call()) -> binary().
caller_id_number(#whapps_call{caller_id_number=CIDNumber}) ->
    CIDNumber.

-spec set_callee_id_name/2 :: (ne_binary(), call()) -> call().
set_callee_id_name(CIDName, #whapps_call{}=Call) when is_binary(CIDName) ->
    io:format("set callee id name: ~p~n", [CIDName]),
    Call#whapps_call{callee_id_name=CIDName}.

-spec callee_id_name/1 :: (call()) -> binary().
callee_id_name(#whapps_call{callee_id_name=CIDName}) ->
    CIDName.

-spec set_callee_id_number/2 :: (ne_binary(), call()) -> call().
set_callee_id_number(CIDNumber, #whapps_call{}=Call) when is_binary(CIDNumber) ->
    io:format("set callee id number: ~p~n", [CIDNumber]),
    Call#whapps_call{callee_id_number=CIDNumber}.

-spec callee_id_number/1 :: (call()) -> binary().
callee_id_number(#whapps_call{callee_id_number=CIDNumber}) ->
    CIDNumber.

-spec set_request/2 :: (ne_binary(), call()) -> call().
set_request(Request, #whapps_call{}=Call) when is_binary(Request) ->
    [RequestUser, RequestRealm] = binary:split(Request, <<"@">>),
    Call#whapps_call{request=Request, request_user=wnm_util:to_e164(RequestUser)
                     ,request_realm=RequestRealm}.

-spec request/1 :: (call()) -> ne_binary().
request(#whapps_call{request=Request}) ->
    Request.

-spec request_user/1 :: (call()) -> ne_binary().
request_user(#whapps_call{request_user=RequestUser}) ->
    RequestUser.

-spec request_realm/1 :: (call()) -> ne_binary().
request_realm(#whapps_call{request_realm=RequestRealm}) ->
    RequestRealm.

-spec set_from/2 :: (ne_binary(), call()) -> call().
set_from(From, #whapps_call{}=Call) when is_binary(From) ->
    [FromUser, FromRealm] = binary:split(From, <<"@">>),
    Call#whapps_call{from=From, from_user=FromUser, from_realm=FromRealm}.

-spec from/1 :: (call()) -> ne_binary().
from(#whapps_call{from=From}) ->
    From.

-spec from_user/1 :: (call()) -> ne_binary().
from_user(#whapps_call{from_user=FromUser}) ->
    FromUser.

-spec from_realm/1 :: (call()) -> ne_binary().
from_realm(#whapps_call{from_realm=FromRealm}) ->
    FromRealm.

-spec set_to/2 :: (ne_binary(), call()) -> call().
set_to(To, #whapps_call{}=Call) when is_binary(To) ->
    [ToUser, ToRealm] = binary:split(To, <<"@">>),
    Call#whapps_call{to=To, to_user=ToUser, to_realm=ToRealm}.

-spec to/1 :: (call()) -> ne_binary().
to(#whapps_call{to=To}) ->
    To.

-spec to_user/1 :: (call()) -> ne_binary().
to_user(#whapps_call{to_user=ToUser}) ->
    ToUser.

-spec to_realm/1 :: (call()) -> ne_binary().
to_realm(#whapps_call{to_realm=ToRealm}) ->
    ToRealm.

-spec set_inception/2 :: (ne_binary(), call()) -> call().
set_inception(<<"on-net">>, #whapps_call{}=Call) ->
    set_custom_channel_var(<<"Inception">>, <<"on-net">>, Call#whapps_call{inception = <<"on-net">>});
set_inception(<<"off-net">>, #whapps_call{}=Call) ->
    set_custom_channel_var(<<"Inception">>, <<"off-net">>, Call#whapps_call{inception = <<"off-net">>}).

-spec inception/1 :: (call()) -> 'undefined' | binary().
inception(#whapps_call{inception=Inception}) ->
    Inception.

-spec set_account_db/2 :: (ne_binary(), call()) -> call().
set_account_db(AccountDb, #whapps_call{}=Call) when is_binary(AccountDb) ->
    AccountId = wh_util:format_account_id(AccountDb, raw),
    set_custom_channel_var(<<"Account-ID">>, AccountId, Call#whapps_call{account_db=AccountDb, account_id=AccountId}).

-spec account_db/1 :: (call()) -> 'undefined' | binary().
account_db(#whapps_call{account_db=AccountDb}) ->
    AccountDb.

-spec set_account_id/2 :: (ne_binary(), call()) -> call().
set_account_id(AccountId, #whapps_call{}=Call) when is_binary(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    set_custom_channel_var(<<"Account-ID">>, AccountId, Call#whapps_call{account_db=AccountDb, account_id=AccountId}).

-spec account_id/1 :: (call()) -> 'undefined' | binary().
account_id(#whapps_call{account_id=AccountId}) ->
    AccountId.

-spec set_authorizing_id/2 :: (ne_binary(), call()) -> call().
set_authorizing_id(AuthorizingId, #whapps_call{}=Call) when is_binary(AuthorizingId) ->
    set_custom_channel_var(<<"Authorizing-Id">>, AuthorizingId, Call#whapps_call{authorizing_id=AuthorizingId}).

-spec authorizing_id/1 :: (call()) -> 'undefined' | binary().
authorizing_id(#whapps_call{authorizing_id=AuthorizingId}) ->
    AuthorizingId.

-spec set_authorizing_type/2 :: (ne_binary(), call()) -> call().
set_authorizing_type(AuthorizingType, #whapps_call{}=Call) when is_binary(AuthorizingType) ->
    set_custom_channel_var(<<"Authorizing-Type">>, AuthorizingType, Call#whapps_call{authorizing_type=AuthorizingType}).

-spec authorizing_type/1 :: (call()) -> 'undefined' | binary().
authorizing_type(#whapps_call{authorizing_type=AuthorizingType}) ->
    AuthorizingType.

-spec set_custom_channel_var/3 :: (term(), term(), call()) -> call().
set_custom_channel_var(Key, Value, #whapps_call{ccvs=CCVs}=Call) ->
    whapps_call_command:set(wh_json:from_list([{Key, Value}]), undefined, Call),
    Call#whapps_call{ccvs=wh_json:set_value(Key, Value, CCVs)}.

-spec update_custom_channel_vars/2 :: (fun((wh_json:json_object()) -> wh_json:json_object()), call()) -> call().
update_custom_channel_vars(Updaters, #whapps_call{ccvs=CCVs}=Call) ->
    NewCCVs = lists:foldr(fun(F, J) -> F(J) end, CCVs, Updaters),
    whapps_call_command:set(NewCCVs, undefined, Call),
    Call#whapps_call{ccvs=NewCCVs}.

-spec custom_channel_var/3 :: (term(), Default, call()) -> Default | term().
custom_channel_var(Key, Default, #whapps_call{ccvs=CCVs}) ->
    wh_json:get_value(Key, CCVs, Default).

-spec custom_channel_var/2 :: (term(), call()) -> term().
custom_channel_var(Key, #whapps_call{ccvs=CCVs}) ->
    wh_json:get_value(Key, CCVs).

-spec custom_channel_vars/1 :: (call()) -> wh_json:json_object().
custom_channel_vars(#whapps_call{ccvs=CCVs}) ->
    CCVs.

-spec kvs_append/3 :: (term(), term(), call()) -> call().
kvs_append(Key, Value, #whapps_call{kvs=Dict}=Call) ->
    Call#whapps_call{kvs=orddict:append(Key, Value, Dict)}.

-spec kvs_append_list/3 :: (term(), [term(),...], call()) -> call().
kvs_append_list(Key, ValList, #whapps_call{kvs=Dict}=Call) ->
    Call#whapps_call{kvs=orddict:append_list(Key, ValList, Dict)}.

-spec kvs_erase/2 :: (term(), call()) -> call().
kvs_erase(Key, #whapps_call{kvs=Dict}=Call) ->
    Call#whapps_call{kvs=orddict:erase(Key, Dict)}.

-spec kvs_fetch/2 :: (term(), call()) -> term().
kvs_fetch(Key, #whapps_call{kvs=Dict}) ->
    try orddict:fetch(Key, Dict) of
        Ok -> Ok
    catch
        error:function_clause -> undefined
    end.     

-spec kvs_fetch_keys/2 :: (term(), call()) -> [term(),...].
kvs_fetch_keys(Key, #whapps_call{kvs=Dict}) ->
    orddict:fetch_keys(Key, Dict).

-spec kvs_filter/2 :: (fun((term(), term()) -> boolean()), call()) -> call().
kvs_filter(Pred, #whapps_call{kvs=Dict}=Call) ->
    Call#whapps_call{kvs=orddict:filter(Pred, Dict)}.

-spec kvs_find/2 :: (term(), call()) -> {ok, term()} | error.
kvs_find(Key, #whapps_call{kvs=Dict}) ->
    orddict:find(Key, Dict).
 
-spec kvs_fold/3 :: (fun((term(), term(), term()) -> term()), term(), call()) -> call().
kvs_fold(Fun, Acc0, #whapps_call{kvs=Dict}) ->
    orddict:fold(Fun, Acc0, Dict).

-spec kvs_from_proplist/2 :: (proplist(), call()) -> call().
kvs_from_proplist(List, #whapps_call{kvs=Dict}=Call) ->
    Call#whapps_call{kvs=orddict:from_list(List, Dict)}.

-spec kvs_is_key/2 :: (term(), call()) -> boolean().
kvs_is_key(Key, #whapps_call{kvs=Dict}) ->
    orddict:is_key(Key, Dict).

-spec kvs_map/2 :: (fun((term(), term()) -> term()), call()) -> call().
kvs_map(Pred, #whapps_call{kvs=Dict}=Call) ->
    Call#whapps_call{kvs=orddict:map(Pred, Dict)}.

-spec kvs_store/3 :: (term(), term(), call()) -> call().
kvs_store(Key, Value, #whapps_call{kvs=Dict}=Call) ->
    Call#whapps_call{kvs=orddict:store(Key, Value, Dict)}.

-spec kvs_store_proplist/2 :: (proplist(), call()) -> call().
kvs_store_proplist(List, #whapps_call{kvs=Dict}=Call) ->
    Call#whapps_call{kvs=lists:foldr(fun({K, V}, D) -> 
                                             orddict:store(K, V, D) 
                                     end, Dict, List)}.

-spec kvs_to_proplist/1 :: (call()) -> proplist().
kvs_to_proplist(#whapps_call{kvs=Dict}) ->
    orddict:to_list(Dict).

-spec kvs_update/3 :: (term(), fun((term()) -> term()), call()) -> call().
kvs_update(Key, Fun, #whapps_call{kvs=Dict}=Call) ->
    Call#whapps_call{kvs=orddict:update(Key, Fun, Dict)}.

-spec kvs_update/4 :: (term(), fun((term()) -> term()), term(), call()) -> call().
kvs_update(Key, Fun, Initial, #whapps_call{kvs=Dict}=Call) ->
    Call#whapps_call{kvs=orddict:update(Key, Fun, Initial, Dict)}.

-spec kvs_update_counter/3 :: (term(), number(), call()) -> call().
kvs_update_counter(Key, Number, #whapps_call{kvs=Dict}=Call) ->
    Call#whapps_call{kvs=orddict:update_counter(Key,Number, Dict)}.
