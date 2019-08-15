%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Listing of all expected v1 callbacks
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_contact_list).

-export([init/0
        ,allowed_methods/0
        ,resource_exists/0
        ,validate/1
        ]).

-include("crossbar.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.contact_list">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.contact_list">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.contact_list">>, ?MODULE, 'validate').

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() -> [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /contact_list => []
%%    /contact_list/foo => [<<"foo">>]
%%    /contact_list/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /contact_list might load a list of skel objects
%% /contact_list/123 might load the skel object 123
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), http_method()) -> cb_context:context().
validate(Context, ?HTTP_GET) ->
    ContactList = provisioner_contact_list:build(cb_context:account_db(Context)),
    cb_context:setters(Context, [{fun cb_context:set_resp_data/2, ContactList}
                                ,{fun cb_context:set_resp_status/2, 'success'}
                                ]).
