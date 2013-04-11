%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Registration viewer / creator
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_registrations).

-export([init/0
         ,allowed_methods/0
         ,resource_exists/0
         ,validate/1
         ,lookup_regs/1
        ]).

-include("../crossbar.hrl").

-define(MASK_REG_FIELDS, [<<"Account-DB">>, <<"Account-ID">>, <<"App-Name">>
                              ,<<"App-Version">>, <<"Event-Category">>, <<"Event-Name">>
                              ,<<"Server-ID">>
                         ]).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.registrations">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.registrations">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.registrations">>, ?MODULE, validate).

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    ['GET'].

-spec resource_exists() -> 'true'.
resource_exists() ->
    true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(#cb_context{req_verb = <<"get">>, db_name=DbName, account_id=AccountId}=Context) ->
    AccountRealm = wh_util:get_account_realm(DbName, AccountId),
    crossbar_util:response(lookup_regs(AccountRealm), Context).

-spec lookup_regs(ne_binary()) -> wh_json:json_objects().
lookup_regs(AccountRealm) ->
    Req = [{<<"Realm">>, AccountRealm}
           ,{<<"Fields">>, []}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    ReqResp = whapps_util:amqp_pool_collect(Req
                                            ,fun wapi_registration:publish_query_req/1
                                            ,'registrar'
                                           ),
    case ReqResp of
        {'error', _} -> [];
        {_, JObjs} ->
            merge_responses(JObjs)
    end.

merge_responses(JObjs) ->
    [normalize_registration(JObj)
     || {_, JObj} <- dict:to_list(merge_responses(JObjs, dict:new()))
    ].

merge_responses([], Regs) ->
    Regs;
merge_responses([JObj|JObjs], Regs) ->
    merge_responses(JObjs, merge_response(JObj, Regs)).

merge_response(JObj, Regs) ->
    Fields = case wh_json:is_true(<<"Multiple">>, JObj) of
                 'true' -> wh_json:get_value(<<"Fields">>, JObj, []);
                 'false' ->
                     [wh_json:get_value(<<"Fields">>, JObj, wh_json:new())]
             end,
    lists:foldl(fun(J, R) ->
                        case wh_json:get_ne_value(<<"Contact">>, J) of
                            'undefined' -> R;
                            Contact ->
                                dict:store(Contact, J, R)
                        end
                end, Regs, Fields).

normalize_registration(JObj) ->
    Contact = wh_json:get_binary_value(<<"Contact">>, JObj, <<>>),
    Updaters = [fun(J) -> wh_json:delete_keys(?MASK_REG_FIELDS, J) end
                ,fun(J) ->
                         case re:run(Contact, "sip:[^@]+@(.*?):([0-9]+)", [{capture, [1, 2], binary}]) of
                             {match,[Ip, Port]} ->
                                 wh_json:set_value(<<"Contact-IP">>, Ip
                                                   ,wh_json:set_value(<<"Contact-Port">>, Port, J));
                             _Else -> J
                         end
                 end
                ,fun(J) ->
                         case re:run(Contact, "received=sip:([^:;]+):?([0-9]+)?", [{capture, [1, 2], binary}]) of
                             {match,[Ip, Port]} ->
                                 wh_json:set_value(<<"Received-IP">>, Ip
                                                   ,wh_json:set_value(<<"Received-Port">>, Port, J));
                             _Else -> J
                         end
                 end
                ,fun(J) ->
                         case re:run(Contact, "fs_path=sip:([^:;]+):?([0-9]+)?", [{capture, [1, 2], binary}]) of
                             {match,[Ip, Port]} ->
                                 wh_json:set_value(<<"Proxy-IP">>, Ip
                                                   ,wh_json:set_value(<<"Proxy-Port">>, Port, J));
                             _Else -> J
                         end
                 end
               ],
    wh_json:normalize(lists:foldr(fun(F, J) -> F(J) end, JObj, Updaters)).
