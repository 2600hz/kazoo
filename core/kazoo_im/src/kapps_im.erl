%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @author Sponsored by GTNetwork LLC, Implemented by SIPLABS LLC
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapps_im).

-export([is_im/1]).
-export([new/0]).
-export([from_payload/1
        ,to_payload/1
        ]).

-export([put_message_id/1]).

-export([exec/2]).
-export_type([exec_funs/0]).

-export([set_application_name/2, application_name/1]).
-export([set_application_version/2, application_version/1]).
-export([set_message_id/2, message_id/1]).

-export([context/1, context/2, set_context/2]).

-export([set_controller_queue/2, controller_queue/1]).

-export([set_from/2, from/1]).
-export([set_to/2, to/1]).

-export([set_account_id/2, account_id/1]).
-export([account_db/1, account_realm/1]).
-export([set_reseller_id/2, reseller_id/1]).
-export([set_application_id/2, application_id/1]).

-export([set_inception/2, inception/1, inception_type/1]).
-export([is_inter_account/1, inter_account_id/1]).

-export([set_authorizing_id/2, authorizing_id/1]).
-export([set_authorizing_type/2, authorizing_type/1]).
-export([set_authorization/3]).
-export([set_owner_id/2, owner_id/1]).
-export([set_fetch_id/2, fetch_id/1]).
-export([set_direction/2, direction/1]).
-export([set_route_type/2, route_type/1]).
-export([set_route_id/2, route_id/1]).
-export([set_body/2, body/1]).
-export([set_type/2, type/1]).
-export([set_endpoint/2, endpoint/1]).

-export([set_custom_var/3
        ,insert_custom_var/3
        ,set_custom_vars/2
        ,remove_custom_vars/2
        ,update_custom_vars/2
        ,custom_var/3
        ,custom_var/2
        ,custom_vars/1
        ]).

-export([kvs_append/3
        ,kvs_append_list/3
        ,kvs_erase/2
        ,kvs_fetch/2, kvs_fetch/3
        ,kvs_fetch_keys/1
        ,kvs_filter/2
        ,kvs_find/2
        ,kvs_flush/1
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

-include("kazoo_im.hrl").

-type im_type() :: 'sms' | 'mms'.
-type direction() :: 'inbound' | 'outbound'.
-type route_type() :: 'onnet' | 'offnet'.

-record(kapps_im, {message_id :: kz_term:api_binary()
                  ,context :: kz_term:api_ne_binary()
                  ,controller_q :: kz_term:api_binary()
                  ,from :: kz_term:api_ne_binary()
                  ,to :: kz_term:api_ne_binary()
                  ,inception :: kz_term:api_binary()
                  ,account_id :: kz_term:api_binary()
                  ,reseller_id :: kz_term:api_binary()
                  ,authorizing_id :: kz_term:api_binary()
                  ,authorizing_type :: kz_term:api_binary()
                  ,owner_id :: kz_term:api_binary()
                  ,fetch_id :: kz_term:api_binary()
                  ,application_id :: kz_term:api_binary()
                  ,app_name = ?APP_NAME :: kz_term:ne_binary()
                  ,app_version = ?APP_VERSION :: kz_term:ne_binary()
                  ,cvs = kz_json:new() :: kz_json:object()
                  ,kvs = orddict:new() :: orddict:orddict()
                  ,direction = 'inbound' :: direction()
                  ,route_type = 'onnet' :: route_type()
                  ,route_id :: kz_term:api_binary()
                  ,body :: kz_term:api_binary()
                  ,type = 'sms' :: im_type()
                  ,endpoint = kz_json:new() :: kz_json:object()
                  }).

-type im() :: #kapps_im{}.

-export_type([im/0
             ,im_type/0
             ,route_type/0
             ,direction/0
             ]).

-define(SPECIAL_VARS, [{<<"Account-ID">>, #kapps_im.account_id}
                      ,{<<"Reseller-ID">>, #kapps_im.reseller_id}
                      ,{<<"Authorizing-ID">>, #kapps_im.authorizing_id}
                      ,{<<"Authorizing-Type">>, #kapps_im.authorizing_type}
                      ,{<<"Fetch-ID">>, #kapps_im.fetch_id}
                      ,{<<"Owner-ID">>, #kapps_im.owner_id}
                      ,{<<"Inception">>, #kapps_im.inception}
                      ]).

-spec new() -> im().
new() -> #kapps_im{}.

-spec put_message_id(im()) -> kz_term:api_binary().
put_message_id(#kapps_im{message_id='undefined'}) -> 'undefined';
put_message_id(#kapps_im{message_id=MsgId}) ->
    kz_log:put_callid(MsgId).

-spec is_im(any()) -> boolean().
is_im(#kapps_im{}) -> 'true';
is_im(_) -> 'false'.

-type exec_fun_1() :: fun((im()) -> im()).
-type exec_fun_2() :: {fun((_, im()) -> im()), _}.
-type exec_fun_3() :: {fun((_, _, im()) -> im()), _, _}.
-type exec_fun() :: exec_fun_1() | exec_fun_2() | exec_fun_3().
-type exec_funs() :: [exec_fun(),...].

-spec exec(exec_funs(), im()) -> im().
exec(Funs, #kapps_im{}=Im) ->
    lists:foldl(fun exec_fold/2, Im, Funs).

-spec exec_fold(exec_fun(), im()) -> im().
exec_fold({F, _K, 'undefined'}, C) when is_function(F, 3) -> C;
exec_fold({F, K, V}, C) when is_function(F, 3) -> F(K, V, C);
exec_fold({F, 'undefined'}, C) when is_function(F, 2) -> C;
exec_fold({F, V}, C) when is_function(F, 2) -> F(V, C);
exec_fold(F, C) when is_function(F, 1) -> F(C).

-spec set_application_name(kz_term:ne_binary(), im()) -> im().
set_application_name(AppName, #kapps_im{}=Im) when is_binary(AppName) ->
    Im#kapps_im{app_name=AppName}.

-spec application_name(im()) -> kz_term:ne_binary().
application_name(#kapps_im{app_name=AppName}) ->
    AppName.

-spec set_application_version(kz_term:ne_binary(), im()) -> im().
set_application_version(AppVersion, #kapps_im{}=Im) when is_binary(AppVersion) ->
    Im#kapps_im{app_version=AppVersion}.

-spec application_version(im()) -> kz_term:ne_binary().
application_version(#kapps_im{app_version=AppVersion}) ->
    AppVersion.

-spec set_message_id(kz_term:api_binary(), im()) -> im().
set_message_id(MsgId, #kapps_im{}=Msg) ->
    Msg#kapps_im{message_id=MsgId}.

-spec message_id(im()) -> kz_term:api_binary().
message_id(#kapps_im{message_id=MsgId}) -> MsgId.

-spec context(im()) -> kz_term:api_ne_binary().
context(Im) ->
    context(Im, 'undefined').

-spec context(im(), Default) -> kz_term:ne_binary() | Default.
context(#kapps_im{context='undefined'}, Default) -> Default;
context(#kapps_im{context=Context}, _Default) -> Context.

-spec set_context(im(), kz_term:ne_binary()) -> im().
set_context(#kapps_im{}=Im, Context) ->
    Im#kapps_im{context=Context}.

-spec set_controller_queue(kz_term:ne_binary(), im()) -> im().
set_controller_queue(ControllerQ, #kapps_im{}=Im) when is_binary(ControllerQ) ->
    Im#kapps_im{controller_q=ControllerQ}.

-spec controller_queue(im()) -> binary().
controller_queue(#kapps_im{controller_q=ControllerQ}) ->
    ControllerQ.

-spec set_from(kz_term:ne_binary(), im()) -> im().
set_from(From, #kapps_im{}=Im)
  when is_binary(From) ->
    Im#kapps_im{from=From}.

-spec from(im()) -> kz_term:ne_binary().
from(#kapps_im{from=From}) ->
    From.

-spec set_to(kz_term:ne_binary(), im()) -> im().
set_to(To, #kapps_im{}=Im)
  when is_binary(To) ->
    Im#kapps_im{to=To}.

-spec to(im()) -> kz_term:ne_binary().
to(#kapps_im{to=To}) ->
    To.

-spec set_inception(kz_term:api_binary(), im()) -> im().
set_inception('undefined', #kapps_im{}=Im) ->
    Im#kapps_im{inception='undefined'};
set_inception(Inception, #kapps_im{}=Im) ->
    set_custom_var(<<"Inception">>, Inception, Im#kapps_im{inception=Inception}).

-spec inception(im()) -> kz_term:api_binary().
inception(#kapps_im{inception=Inception}) ->
    Inception.

-spec set_application_id(kz_term:api_ne_binary(), im()) -> im().
set_application_id(<<_/binary>> = ApplicationId, #kapps_im{}=Im) ->
    Props = [{<<"Account-ID">>, null}
            ,{<<"Reseller-ID">>, null}
            ],
    set_custom_vars(Props, Im#kapps_im{application_id=ApplicationId});
set_application_id(_, #kapps_im{}=Im) -> Im.

-spec application_id(im()) -> kz_term:api_binary().
application_id(#kapps_im{application_id=ApplicationId}) ->
    ApplicationId.

-spec account_db(im()) -> kz_term:api_ne_binary().
account_db(#kapps_im{account_id='undefined'}) -> 'undefined';
account_db(#kapps_im{account_id=AccountId}) -> kzs_util:format_account_db(AccountId).

-spec set_account_id(kz_term:ne_binary(), im()) -> im().
set_account_id(<<_/binary>> = AccountId, #kapps_im{}=Im) ->
    Props = [{<<"Account-ID">>, AccountId}
            ,{<<"Realm">>, kzd_accounts:fetch_realm(AccountId)}
            ,{<<"Reseller-ID">>, kz_services_reseller:get_id(AccountId)}
            ],
    fetch_endpoint(set_custom_vars(Props, Im)).

-spec account_id(im()) -> kz_term:api_binary().
account_id(#kapps_im{account_id=AccountId}) ->
    AccountId.

-spec set_reseller_id(kz_term:ne_binary(), im()) -> im().
set_reseller_id(<<_/binary>> = ResellerId, #kapps_im{}=Im) ->
    Props = [{<<"Reseller-ID">>, ResellerId}],
    set_custom_vars(Props, Im).

-spec reseller_id(im()) -> kz_term:api_binary().
reseller_id(#kapps_im{reseller_id=ResellerId}) ->
    ResellerId.

-spec account_realm(im()) -> kz_term:ne_binary().
account_realm(#kapps_im{account_id=AccountId}) ->
    kzd_accounts:fetch_realm(AccountId).

-spec set_authorizing_id(kz_term:ne_binary(), im()) -> im().
set_authorizing_id(AuthorizingId, #kapps_im{}=Im)
  when is_binary(AuthorizingId) ->
    set_custom_var(<<"Authorizing-ID">>, AuthorizingId, Im#kapps_im{authorizing_id=AuthorizingId}).

-spec authorizing_id(im()) -> kz_term:api_binary().
authorizing_id(#kapps_im{authorizing_id=AuthorizingId}) ->
    AuthorizingId.

-spec set_authorizing_type(kz_term:ne_binary(), im()) -> im().
set_authorizing_type(AuthorizingType, #kapps_im{}=Im) when is_binary(AuthorizingType) ->
    set_custom_var(<<"Authorizing-Type">>, AuthorizingType, Im#kapps_im{authorizing_type=AuthorizingType}).

-spec authorizing_type(im()) -> kz_term:api_binary().
authorizing_type(#kapps_im{authorizing_type=AuthorizingType}) ->
    AuthorizingType.

-spec set_authorization(kz_term:ne_binary(), kz_term:ne_binary(), im()) -> im().
set_authorization(AuthorizingType, AuthorizingId, #kapps_im{}=Im)
  when is_binary(AuthorizingType)
       andalso is_binary(AuthorizingId) ->
    set_custom_vars([{<<"Authorizing-Type">>, AuthorizingType}
                            ,{<<"Authorizing-ID">>, AuthorizingId}
                            ]
                           ,Im#kapps_im{authorizing_type=AuthorizingType
                                         ,authorizing_id=AuthorizingId
                                         }
                           ).

-spec set_owner_id(kz_term:ne_binary(), im()) -> im().
set_owner_id(OwnerId, #kapps_im{}=Im) when is_binary(OwnerId) ->
    set_custom_var(<<"Owner-ID">>, OwnerId, Im#kapps_im{owner_id=OwnerId});
set_owner_id(_, #kapps_im{}=Im) -> Im.

-spec owner_id(im()) -> kz_term:api_binary().
owner_id(#kapps_im{owner_id=OwnerId}) -> OwnerId.

-spec set_fetch_id(kz_term:ne_binary(), im()) -> im().
set_fetch_id(FetchId, #kapps_im{}=Im) when is_binary(FetchId) ->
    set_custom_var(<<"Fetch-Id">>, FetchId, Im#kapps_im{fetch_id=FetchId}).

-spec fetch_id(im()) -> kz_term:api_binary().
fetch_id(#kapps_im{fetch_id=FetchId}) -> FetchId.

-spec direction(im()) -> direction().
direction(#kapps_im{direction=Direction}) ->
    Direction.

-spec set_direction(direction() | binary(), im()) -> im().
set_direction(Direction, #kapps_im{}=Im)
  when is_binary(Direction) ->
    set_direction(kz_term:to_atom(Direction, 'true'), Im);
set_direction(Direction, #kapps_im{}=Im)
  when Direction =:= 'inbound';
       Direction =:= 'outbound' ->
    Im#kapps_im{direction=Direction};
set_direction(_Direction, #kapps_im{}=Im) -> Im.

-spec route_type(im()) -> route_type().
route_type(#kapps_im{route_type=Type}) ->
    Type.

-spec set_route_type(route_type() | binary(), im()) -> im().
set_route_type(Type, #kapps_im{}=Im)
  when is_binary(Type)->
    set_route_type(kz_term:to_atom(Type, 'true'), Im);
set_route_type(Type, #kapps_im{}=Im)
  when Type =:= 'onnet';
       Type =:= 'offnet' ->
    Im#kapps_im{route_type=Type};
set_route_type(_Type, #kapps_im{}=Im) -> Im.

-spec route_id(im()) -> kz_term:api_binary().
route_id(#kapps_im{route_id=RouteId}) ->
    RouteId.

-spec set_route_id(binary(), im()) -> im().
set_route_id(RouteId, #kapps_im{}=Im)
  when is_binary(RouteId)->
    Im#kapps_im{route_id=RouteId};
set_route_id(_RouteId, #kapps_im{}=Im) -> Im.

-spec body(im()) -> kz_term:ne_binary().
body(#kapps_im{body=Body}) ->
    Body.

-spec set_body(kz_term:ne_binary(), im()) -> im().
set_body(Body, #kapps_im{}=Im) when is_binary(Body) ->
    Im#kapps_im{body=Body};
set_body(_Body, #kapps_im{}=Im) -> Im.

-spec set_endpoint(kz_json:object(), im()) -> im().
set_endpoint(EP, #kapps_im{}=Im) ->
    set_owner_id(kzd_devices:owner_id(EP), Im#kapps_im{endpoint=EP}).

-spec endpoint(im()) -> kz_json:object().
endpoint(#kapps_im{endpoint=EP}) ->
    EP.

-spec type(im()) -> im_type().
type(#kapps_im{type=Type}) ->
    Type.

-spec set_type(im_type() | binary(), im()) -> im().
set_type(Type, #kapps_im{}=Im)
  when is_binary(Type) ->
    set_type(kz_term:to_atom(Type, 'true'), Im);
set_type(Type, #kapps_im{}=Im)
  when Type =:= 'sms';
       Type =:= 'mms' ->
    Im#kapps_im{type=Type};
set_type(_Type, #kapps_im{}=Im) -> Im.

-spec remove_custom_vars(kz_json:keys(), im()) -> im().
remove_custom_vars(Keys, #kapps_im{}=Im) ->
    handle_cvs_remove(Keys, Im).

-spec handle_cvs_remove(kz_json:keys(), im()) -> im().
handle_cvs_remove(Keys, #kapps_im{cvs=CCVs}=Im) ->
    lists:foldl(fun ccv_remove_fold/2
               ,Im#kapps_im{cvs=kz_json:delete_keys(Keys, CCVs)}
               ,Keys
               ).

-spec ccv_remove_fold(kz_json:key(), im()) -> im().
ccv_remove_fold(Key, Im) ->
    case props:get_value(Key, ?SPECIAL_VARS) of
        'undefined' -> Im;
        Index -> setelement(Index, Im, 'undefined')
    end.

-spec set_custom_var(kz_json:key(), kz_json:json_term(), im()) -> im().
set_custom_var(Key, Value, Im) ->
    insert_custom_var(Key, Value, Im).

-spec insert_custom_var(kz_json:key(), kz_json:json_term(), im()) -> im().
insert_custom_var(Key, Value, #kapps_im{cvs=CCVs}=Im) ->
    handle_cvs_update(kz_json:set_value(Key, Value, CCVs), Im).

-spec set_custom_vars(kz_term:proplist(), im()) -> im().
set_custom_vars(Props, #kapps_im{cvs=CCVs}=Im) ->
    NewCCVs = kz_json:set_values(Props, CCVs),
    handle_cvs_update(NewCCVs, Im).

-spec update_custom_vars([fun((kz_json:object()) -> kz_json:object()),...], im()) -> im().
update_custom_vars(Updaters, #kapps_im{cvs=CCVs}=Im) ->
    NewCCVs = lists:foldr(fun(F, J) -> F(J) end, CCVs, Updaters),
    handle_cvs_update(NewCCVs, Im).

-spec custom_var(any(), Default, im()) -> Default | _.
custom_var(Key, Default, #kapps_im{cvs=CCVs}) ->
    kz_json:get_value(Key, CCVs, Default).

-spec custom_var(any(), im()) -> any().
custom_var(Key, #kapps_im{cvs=CCVs}) ->
    kz_json:get_value(Key, CCVs).

-spec custom_vars(im()) -> kz_json:object().
custom_vars(#kapps_im{cvs=CCVs}) ->
    CCVs.

-spec handle_cvs_update(kz_json:object(), im()) -> im().
handle_cvs_update(CCVs, #kapps_im{}=Im) ->
    lists:foldl(fun({Var, Index}, C) ->
                        case kz_json:get_ne_value(Var, CCVs) of
                            'undefined' -> C;
                            Value -> setelement(Index, C, Value)
                        end
                end
               ,Im#kapps_im{cvs=CCVs}
               ,?SPECIAL_VARS
               ).

-spec kvs_append(any(), any(), im()) -> im().
kvs_append(Key, Value, #kapps_im{kvs=Dict}=Im) ->
    Im#kapps_im{kvs=orddict:append(kz_term:to_binary(Key), Value, Dict)}.

-spec kvs_append_list(any(), [any(),...], im()) -> im().
kvs_append_list(Key, ValList, #kapps_im{kvs=Dict}=Im) ->
    Im#kapps_im{kvs=orddict:append_list(kz_term:to_binary(Key), ValList, Dict)}.

-spec kvs_erase(any() | [any(),...], im()) -> im().
kvs_erase(Keys, #kapps_im{kvs=Dict}=Im) when is_list(Keys)->
    Im#kapps_im{kvs=erase_keys(Keys, Dict)};
kvs_erase(Key, #kapps_im{kvs=Dict}=Im) ->
    Im#kapps_im{kvs=erase_key(Key, Dict)}.

-spec erase_keys(list(), orddict:orddict()) -> orddict:orddict().
erase_keys(Keys, Dict) ->
    lists:foldl(fun erase_key/2, Dict, Keys).

-spec erase_key(any(), orddict:orddict()) -> orddict:orddict().
erase_key(K, D) -> orddict:erase(kz_term:to_binary(K), D).

-spec kvs_flush(im()) -> im().
kvs_flush(#kapps_im{}=Im) -> Im#kapps_im{kvs=orddict:new()}.

-spec kvs_fetch(any(), im()) -> any().
kvs_fetch(Key, Im) -> kvs_fetch(Key, 'undefined', Im).

-spec kvs_fetch(any(), Default, im()) -> any() | Default.
kvs_fetch(Key, Default, #kapps_im{kvs=Dict}) ->
    try orddict:fetch(kz_term:to_binary(Key), Dict)
    catch
        'error':'function_clause' -> Default
    end.

-spec kvs_fetch_keys(im()) -> [any(),...].
kvs_fetch_keys(#kapps_im{kvs=Dict}) -> orddict:fetch_keys(Dict).

-spec kvs_filter(fun((any(), any()) -> boolean()), im()) -> im().
kvs_filter(Pred, #kapps_im{kvs=Dict}=Im) ->
    Im#kapps_im{kvs=orddict:filter(Pred, Dict)}.

-spec kvs_find(any(), im()) -> {'ok', any()} | 'error'.
kvs_find(Key, #kapps_im{kvs=Dict}) ->
    orddict:find(kz_term:to_binary(Key), Dict).

-spec kvs_fold(fun((any(), any(), any()) -> any()), any(), im()) -> im().
kvs_fold(Fun, Acc0, #kapps_im{kvs=Dict}) -> orddict:fold(Fun, Acc0, Dict).

-spec kvs_from_proplist(kz_term:proplist(), im()) -> im().
kvs_from_proplist(List, #kapps_im{kvs=Dict}=Im) ->
    L = orddict:from_list([{kz_term:to_binary(K), V} || {K, V} <- List]),
    Im#kapps_im{kvs=orddict:merge(fun(_, V1, _) -> V1 end, L, Dict)}.

-spec kvs_is_key(any(), im()) -> boolean().
kvs_is_key(Key, #kapps_im{kvs=Dict}) ->
    orddict:is_key(kz_term:to_binary(Key), Dict).

-spec kvs_map(fun((any(), any()) -> any()), im()) -> im().
kvs_map(Pred, #kapps_im{kvs=Dict}=Im) ->
    Im#kapps_im{kvs=orddict:map(Pred, Dict)}.

-spec kvs_store(any(), any(), im()) -> im().
kvs_store(Key, Value, #kapps_im{kvs=Dict}=Im) ->
    Im#kapps_im{kvs=orddict:store(kz_term:to_binary(Key), Value, Dict)}.

-spec kvs_store_proplist(kz_term:proplist(), im()) -> im().
kvs_store_proplist(List, #kapps_im{kvs=Dict}=Im) ->
    Im#kapps_im{kvs=add_to_store(List, Dict)}.

add_to_store(List, Dict) ->
    lists:foldr(fun add_to_store_fold/2, Dict, List).

add_to_store_fold({K, V}, D) ->
    orddict:store(kz_term:to_binary(K), V, D).

-spec kvs_to_proplist(im()) -> kz_term:proplist().
kvs_to_proplist(#kapps_im{kvs=Dict}) ->
    orddict:to_list(Dict).

-spec kvs_update(any(), fun((any()) -> any()), im()) -> im().
kvs_update(Key, Fun, #kapps_im{kvs=Dict}=Im) ->
    Im#kapps_im{kvs=orddict:update(kz_term:to_binary(Key), Fun, Dict)}.

-spec kvs_update(any(), fun((any()) -> any()), any(), im()) -> im().
kvs_update(Key, Fun, Initial, #kapps_im{kvs=Dict}=Im) ->
    Im#kapps_im{kvs=orddict:update(kz_term:to_binary(Key), Fun, Initial, Dict)}.

-spec kvs_update_counter(any(), number(), im()) -> im().
kvs_update_counter(Key, Number, #kapps_im{kvs=Dict}=Im) ->
    Im#kapps_im{kvs=orddict:update_counter(kz_term:to_binary(Key), Number, Dict)}.

-spec inception_type(im()) -> kz_term:ne_binary().
inception_type(#kapps_im{inception='undefined'}) -> <<"onnet">>;
inception_type(#kapps_im{}) -> <<"offnet">>.

-spec is_inter_account(im()) -> boolean().
is_inter_account(#kapps_im{}=Im) ->
    inter_account_id(Im) /= 'undefined'.

-spec inter_account_id(im()) -> kz_term:api_binary().
inter_account_id(#kapps_im{}=Im) ->
    custom_var(<<"Inception-Account-ID">>, Im).

-spec to_payload(im()) -> kz_types:api_terms().
to_payload(IM) ->
    [{<<"Message-ID">>, message_id(IM)}
    ,{<<"Body">>, body(IM)}
    ,{<<"From">>, from(IM)}
    ,{<<"To">>, to(IM)}
    ,{<<"Route-Type">>, kz_term:to_binary(route_type(IM))}
    ,{<<"Route-ID">>, kz_term:to_api_binary(route_id(IM))}
    ,{<<"Custom-Vars">>, custom_vars(IM)}
    ,{?KEY_EVENT_CATEGORY, kz_term:to_binary(type(IM))}
    ,{?KEY_EVENT_NAME, kz_term:to_binary(direction(IM))}
     | kz_api:default_headers(controller_queue(IM)
                             ,default(application_name(IM), ?APP_NAME)
                             ,default(application_version(IM), ?APP_VERSION)
                             )
    ].

-spec from_payload(kz_json:object()) -> im().
from_payload(JObj) ->
    CCVs = kz_json:to_proplist(<<"Custom-Vars">>, JObj),
    Routines = [{fun set_message_id/2, kz_im:message_id(JObj, kz_binary:rand_hex(16))}
               ,{fun set_type/2, kz_api:event_category(JObj)}
               ,{fun set_direction/2, kz_api:event_name(JObj)}
               ,{fun set_account_id/2, kz_im:account_id(JObj)}
               ,{fun set_custom_vars/2, CCVs}
               ,{fun set_from/2, kz_im:from(JObj)}
               ,{fun set_to/2, kz_im:to(JObj)}
               ,{fun set_application_name/2, default(kz_api:app_name(JObj), ?APP_NAME)}
               ,{fun set_application_version/2, default(kz_api:app_version(JObj), ?APP_VERSION)}
               ,{fun set_body/2, kz_im:body(JObj)}
               ,fun fetch_endpoint/1
               ],
   exec(Routines,  new()).

-spec fetch_endpoint(im()) -> im().
fetch_endpoint(#kapps_im{authorizing_id='undefined'}=Im) -> Im;
fetch_endpoint(#kapps_im{account_id='undefined'}=Im) -> Im;
fetch_endpoint(Im) ->
    case kz_endpoint:get(authorizing_id(Im), account_id(Im)) of
        {'ok', JObj} -> set_endpoint(JObj, Im);
        _Else -> Im
    end.

default('undefined', Default) -> Default;
default(Value, _Default) -> Value.
