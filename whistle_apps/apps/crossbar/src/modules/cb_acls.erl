%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% ACLs from 7 to 77
%%%
%%% @end
%%% Contributors: Karl Anderson
%%%               James Aimonetti
%%%               Edouard Swiac 
%%%-------------------------------------------------------------------
-module(cb_acls).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,delete/2
        ]).

-include("include/crossbar.hrl").
-define(ECALLMGR, <<"ecallmgr">>).
-define(ECALLMGR_ACLS, <<"acls">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init/0 :: () -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.acls">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.acls">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.acls">>, ?MODULE, validate),
    %%_ = crossbar_bindings:bind(<<"v1_resource.execute.put.acls">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.delete.acls">>, ?MODULE, delete),
    ok.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods() | [].
-spec allowed_methods/1 :: (path_token()) -> http_methods() | [].
allowed_methods() ->
    ['GET', 'PUT'].
allowed_methods(_) ->
    ['GET', 'DELETE'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /acls => []
%%    /acls/foo => [<<"foo">>]
%%    /acls/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
-spec resource_exists/1 :: (path_tokens()) -> 'true'.
resource_exists() -> true.
resource_exists(_) -> true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /acls mights load a list of skel objects
%% /acls/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    summary(Context);
validate(#cb_context{req_verb = <<"put">>}=Context) ->
   create(Context);

validate(Context) ->
    crossbar_util:response_faulty_request(Context).

validate(#cb_context{req_verb = <<"get">>}=Context, Id) ->
    read(Id, Context);
validate(#cb_context{req_verb = <<"delete">>}=Context, Id) ->
    delete(Context, Id);
validate(Context, _) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
%-spec put/1 :: (#cb_context{}) -> #cb_context{}.
%put(=Context) ->
%    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
delete(Context, Id) ->
    Acls = whapps_config:get(?ECALLMGR, ?ECALLMGR_ACLS),
    _ = whapps_config:set_default(?ECALLMGR, ?ECALLMGR_ACLS, wh_json:delete_key(Id, Acls)),
    Context#cb_context{doc=wh_json:new(), resp_data=[], resp_status=success}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create/1 :: (#cb_context{}) -> #cb_context{}.
create(#cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"acls">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            Acls = whapps_config:get(?ECALLMGR, ?ECALLMGR_ACLS),
            Key = mochiweb_util:quote_plus(wh_json:get_value(<<"cidr">>, JObj)),
            Merged = wh_json:set_value(binary:list_to_bin(Key), JObj, Acls),
            _ = whapps_config:set_default(?ECALLMGR, ?ECALLMGR_ACLS, Merged),
            wapi_switch:publish_reloadacl(),
    
            Context#cb_context{doc=JObj, resp_status=success}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
read(Id, Context) ->
    case whapps_config:get(?ECALLMGR, [?ECALLMGR_ACLS, Id]) of
        undefined -> crossbar_util:response_bad_identifier(Id, Context);
        Acl -> Context#cb_context{resp_data=Acl, resp_status=success}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary/1 :: (#cb_context{}) -> #cb_context{}.
summary(Context) ->
    Acls = whapps_config:get(?ECALLMGR, ?ECALLMGR_ACLS, wh_json:new()),
    Context#cb_context{resp_data=Acls, resp_status=success}.
