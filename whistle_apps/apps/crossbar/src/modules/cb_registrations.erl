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

-include_lib("crossbar/include/crossbar.hrl").
%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.registrations">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.registrations">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.registrations">>, ?MODULE, validate).

-spec allowed_methods/0 :: () -> http_methods().
allowed_methods() ->
    ['GET'].

-spec resource_exists/0 :: () -> 'true'.
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
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>, db_name=DbName, account_id=AccountId}=Context) ->
    {ok, JObjs} = couch_mgr:get_all_results(DbName, <<"devices/sip_credentials">>),
    DefaultRealm = wh_util:get_account_realm(DbName, AccountId),
    AccountUsers = [{wh_json:get_value([<<"value">>, <<"realm">>], JObj, DefaultRealm)
                     ,wh_json:get_value([<<"value">>, <<"username">>], JObj)}
                    || JObj <- JObjs],
    CurrentRegs = [wh_json:normalize(JObj) || {_, JObj} <- cb_modules_util:lookup_regs(AccountUsers)],

    crossbar_util:response(CurrentRegs, Context).
