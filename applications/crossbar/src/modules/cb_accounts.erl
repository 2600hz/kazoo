%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%% Account module
%%%
%%% Handle client requests for account documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_accounts).

-export([init/0]).

-spec init() -> 'ok'.
init() ->
    V1Bindings = [{<<"v1_resource.allowed_methods.accounts">>, 'allowed_methods'}
                  ,{<<"v1_resource.resource_exists.accounts">>, 'resource_exists'}
                  ,{<<"v1_resource.validate.accounts">>, 'validate'}
                  ,{<<"v1_resource.execute.put.accounts">>, 'put'}
                  ,{<<"v1_resource.execute.post.accounts">>, 'post'}
                  ,{<<"v1_resource.execute.delete.accounts">>, 'delete'}
                 ],
    V2Bindings = [{<<"v2_resource.allowed_methods.accounts">>, 'allowed_methods'}
                  ,{<<"v2_resource.resource_exists.accounts">>, 'resource_exists'}
                  ,{<<"v2_resource.validate.accounts">>, 'validate'}
                  ,{<<"v2_resource.execute.put.accounts">>, 'put'}
                  ,{<<"v2_resource.execute.post.accounts">>, 'post'}
                  ,{<<"v2_resource.execute.delete.accounts">>, 'delete'}
                 ],

    cb_modules_util:bind('cb_accounts_v1', V1Bindings),
    cb_modules_util:bind('cb_accounts_v2', V2Bindings).
