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
        ]).

-include("src/crossbar.hrl").

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
    Q = amqp_util:new_queue(),
    ok = amqp_util:bind_q_to_targeted(Q),
    ok = amqp_util:basic_consume(Q),
    Req = [{<<"Realm">>, AccountRealm}
           ,{<<"Fields">>, []}
           | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
          ],
    wapi_registration:publish_query_req(Req),
    [normalize_registration(JObj) || {_, JObj} <- collect_registrar_responses([])].

-spec collect_registrar_responses(wh_proplist()) -> wh_proplist().
collect_registrar_responses(Registrations) ->
    receive
        {_, #amqp_msg{payload = Payload}} ->
            JObj = wh_json:decode(Payload),
            true = wapi_registration:query_resp_v(JObj),
            case wh_json:get_value(<<"Fields">>, JObj) of
                undefined -> collect_registrar_responses(Registrations);
                Response -> 
                    Regs = accumulate_unique_registrations(Response, Registrations),
                    collect_registrar_responses(Regs)
            end
    after
        500 -> Registrations
    end.

-spec accumulate_unique_registrations(wh_json:json_objects(), wh_proplist()) -> wh_proplist().
accumulate_unique_registrations([], Accumulator) ->
    Accumulator;
accumulate_unique_registrations([Registration|Registrations], Accumulator) ->
    AuthorizingId = wh_json:get_value(<<"Authorizing-ID">>, Registration),
    case AuthorizingId =:= undefined orelse proplists:is_defined(AuthorizingId, Accumulator) of
        true -> accumulate_unique_registrations(Registrations, Accumulator);
        false -> accumulate_unique_registrations(Registrations, [{AuthorizingId, Registration}|Accumulator])
    end.
            
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
