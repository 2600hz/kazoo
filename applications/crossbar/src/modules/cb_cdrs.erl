%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% CDR
%%% Read only access to CDR docs
%%%
%%% @end
%%% @contributors
%%%   Edouard Swiac
%%%   James Aimonetti
%%%   Karl Anderson
%%%   Ben Wann
%%%-------------------------------------------------------------------
-module(cb_cdrs).

-export([init/0]).

-include("../crossbar.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec init() -> 'ok'.
init() ->
    V1Bindings = [{<<"v1_resource.allowed_methods.cdrs">>, 'allowed_methods'}
                  ,{<<"v1_resource.resource_exists.cdrs">>, 'resource_exists'}
                  ,{<<"v1_resource.content_types_provided.cdrs">>, 'content_types_provided'}
                  ,{<<"v1_resource.validate.cdrs">>, 'validate'}
                 ],

    V2Bindings = [{<<"v2_resource.allowed_methods.cdrs">>, 'allowed_methods'}
                  ,{<<"v2_resource.resource_exists.cdrs">>, 'resource_exists'}
                  ,{<<"v2_resource.content_types_provided.cdrs">>, 'content_types_provided'}
                  ,{<<"v2_resource.validate.cdrs">>, 'validate'}
                 ],

    cb_modules_util:bind('cb_cdrs_v1', V1Bindings),
    cb_modules_util:bind('cb_cdrs_v2', V2Bindings).
