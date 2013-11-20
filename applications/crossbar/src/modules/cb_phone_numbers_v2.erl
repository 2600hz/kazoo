-module(cb_phone_numbers_v2).

-export([authenticate/1]).
-export([authorize/1]).
-export([allowed_methods/1]).
-export([resource_exists/1]).
-export([validate/1, validate/2]).
-export([post/2]).

-include("../crossbar.hrl").
-include_lib("whistle_number_manager/include/wh_number_manager.hrl").

-define(FIND_NUMBER_SCHEMA, "{\"$schema\": \"http://json-schema.org/draft-03/schema#\", \"id\": \"http://json-schema.org/draft-03/schema#\", \"properties\": {\"prefix\": {\"required\": \"true\", \"type\": \"string\", \"minLength\": 3, \"maxLength\": 10}, \"quantity\": {\"default\": 1, \"type\": \"integer\", \"minimum\": 1}}}").
-define(DEFAULT_COUNTRY, <<"US">>).
-define(PHONE_NUMBERS_CONFIG_CAT, <<"crossbar.phone_numbers">>).
-define(FREE_URL, <<"phonebook_url">>).
-define(PAYED_URL, <<"phonebook_url_premium">>).
-define(PREFIX, <<"prefix">>).
-define(LOCALITY, <<"locality">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authenticates the incoming request, returning true if the requestor is
%% known, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> 'true'.
authenticate(#cb_context{req_nouns=[{<<"phone_numbers">>,[?PREFIX]}]
                         ,req_verb = ?HTTP_GET
                        }) ->
    'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> 'true'.
authorize(#cb_context{req_nouns=[{<<"phone_numbers">>,[?PREFIX]}]
                      ,req_verb = ?HTTP_GET
                     }) ->
    'true'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?PREFIX) -> [?HTTP_GET];
allowed_methods(?LOCALITY) -> [?HTTP_POST].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(path_token()) -> 'true'.
resource_exists(?PREFIX) -> 'true';
resource_exists(?LOCALITY) -> 'true'.

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
-spec validate(cb_context:context(), path_token()) -> cb_context:context().

validate(#cb_context{req_verb = ?HTTP_GET
                     ,account_id='undefined'
                    }=Context) ->
    find_numbers(Context);
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    summary(Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?PREFIX) ->
    find_prefix(Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, ?LOCALITY) ->
    find_locality(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?LOCALITY) -> Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    case cb_phone_numbers:summary(Context) of
        #cb_context{resp_status='success'}=C ->
            maybe_update_locality(C);
        Else -> Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(cb_context:context()) -> cb_context:context().
find_numbers(Context) ->
    AccountId = cb_context:auth_account_id(Context),
    JObj = wh_json:set_value(<<"Account-ID">>, AccountId, cb_context:query_string(Context)),
    OnSuccess = fun(C) ->
                        Prefix = wh_json:get_ne_value(<<"prefix">>, JObj),
                        Quantity = wh_json:get_ne_value(<<"quantity">>, JObj, 1),
                        cb_context:set_resp_data(
                          cb_context:set_resp_status(C, 'success')
                          ,wh_number_manager:find(Prefix, Quantity, wh_json:to_proplist(JObj))
                         )
                end,
    Schema = wh_json:decode(?FIND_NUMBER_SCHEMA),
    cb_context:validate_request_data(Schema
                                     ,cb_context:set_req_data(Context, JObj)
                                     ,OnSuccess
                                    ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec find_prefix(cb_context:context()) -> cb_context:context().
find_prefix(Context) ->
    QS = cb_context:query_string(Context),
    case wh_json:get_ne_value(<<"city">>, QS) of
        'undefined' -> cb_context:add_system_error('bad_identifier', Context);
        City ->
            case get_prefix(City) of
                {'ok', Data} ->
                    cb_context:set_resp_data(
                        cb_context:set_resp_status(Context, 'success')
                        ,Data
                    );
                {'error', Error} ->
                    lager:error("error while prefix for city: ~p : ~p", [City, Error]),
                    cb_context:set_resp_data(
                        cb_context:set_resp_status(Context, 'error')
                        ,Error
                    )
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec find_locality(cb_context:context()) -> cb_context:context().
find_locality(#cb_context{req_data=Data}=C) ->
    case wh_json:get_value(<<"numbers">>, Data) of
        'undefined' ->
            cb_context:add_validation_error(<<"numbers">>
                                            ,<<"required">>
                                            ,<<"list of numbers missing">>
                                            ,C);
        [] ->
           cb_context:add_validation_error(<<"numbers">>
                                            ,<<"minimum">>
                                            ,<<"minimum 1 number required">>
                                            ,C);
        Numbers when is_list(Numbers) ->
            Url = get_url(wh_json:get_value(<<"quality">>, Data)),
            case get_locality(Numbers, Url) of
                {'error', E} ->
                    crossbar_util:response('error', E, 500, C);
                Localities ->
                    cb_context:set_resp_data(cb_context:set_resp_status(C, 'success')
                                            ,Localities)
            end;
        _E ->
            cb_context:add_validation_error(<<"numbers">>
                                            ,<<"type">>
                                            ,<<"numbers must be a list">>
                                            ,C)
    end.

-spec get_url(any()) -> binary().
get_url(<<"high">>) -> ?PAYED_URL;
get_url(_) -> ?FREE_URL.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec get_prefix(ne_binary()) -> {'ok', wh_json:object()} | {'error', any()}.
get_prefix(City) ->
    Country = whapps_config:get(?PHONE_NUMBERS_CONFIG_CAT, <<"default_country">>, ?DEFAULT_COUNTRY),
    case whapps_config:get(?PHONE_NUMBERS_CONFIG_CAT, ?FREE_URL) of
        'undefined' ->
            {'error', <<"Unable to acquire numbers missing carrier url">>};
        Url ->
            ReqParam  = wh_util:uri_encode(binary:bin_to_list(City)),
            Req = binary:bin_to_list(<<Url/binary, "/", Country/binary, "/city?pattern=">>),
            Uri = lists:append(Req, ReqParam),
            case ibrowse:send_req(Uri, [], 'get') of
                {'error', Reason} ->
                    {'error', Reason};
                {'ok', "200", _Headers, Body} ->
                    JObj =  wh_json:decode(Body),
                    case wh_json:get_value(<<"data">>, JObj) of
                        'undefined' -> {'error ', JObj};
                        Data -> {'ok', Data}
                    end;
                {'ok', _Status, _Headers, Body} ->
                    {'error', wh_json:decode(Body)}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec maybe_update_locality(cb_context:context()) -> cb_context:context().
maybe_update_locality(Context) ->
    Numbers = wh_json:foldl(
                fun(Key, Value, Acc) ->
                        case wh_json:get_value(<<"locality">>, Value) =:= 'undefined'
                            andalso  wnm_util:is_reconcilable(Key)
                        of
                            'true' -> [Key|Acc];
                            'false' -> Acc
                        end
                end
                ,[]
                ,wh_json:get_value(<<"numbers">>, cb_context:resp_data(Context))
               ),
    update_locality(Context, Numbers).

-spec update_locality(cb_context:context(), ne_binaries()) -> cb_context:context().
update_locality(Context, []) -> Context;
update_locality(Context, Numbers) ->
    case get_locality(Numbers, ?FREE_URL) of
        {'error', _} -> Context;
        Localities ->
            spawn(fun() -> 
                          update_phone_numbers_locality(Context, Localities)
                  end),
            update_context_locality(Context, Localities)
    end.

-spec update_context_locality(cb_context:context(), wh_json:objects()) -> cb_context:context().
update_context_locality(Context, Localities) ->
    JObj = wh_json:foldl(fun(Key, Value, J) ->
                                 wh_json:set_value([<<"numbers">>
                                                    ,Key
                                                    ,<<"locality">>
                                                   ], Value, J)
                         end, cb_context:resp_data(Context), Localities),
    cb_context:set_resp_data(Context, JObj).

-spec update_phone_numbers_locality(cb_context:context(), wh_json:objects()) -> {'ok', wh_json:object()} | {'error', _}.
update_phone_numbers_locality(Context, Localities) ->
    AccountDb = cb_context:account_db(Context),
    DocId = wh_json:get_value(<<"_id">>, cb_context:doc(Context), <<"phone_numbers">>),
    case couch_mgr:open_doc(AccountDb, DocId) of
        {'ok', JObj} ->
            J = wh_json:foldl(fun(Key, Value, J) ->
                                      case wh_json:get_value(Key, J) of
                                          'undefined' -> J;
                                          _Else ->
                                              wh_json:set_value([Key
                                                                 ,<<"locality">>
                                                                ], Value, J)
                                      end
                              end, JObj, Localities),
            couch_mgr:save_doc(AccountDb, J);
        {'error', _E} ->
            lager:error("failed to update locality for ~p in ~p", [DocId, AccountDb])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec get_locality(ne_binaries(), binary()) -> {'error', binary()} | wh_json:object().
get_locality([], _) -> {'error', <<"number missing">>};
get_locality(Numbers, UrlType) ->
    case whapps_config:get(?PHONE_NUMBERS_CONFIG_CAT, UrlType) of
        'undefined' ->
            lager:error("could not get number locality url", []),
            {'error', <<"missing phonebook url">>};
        Url ->
            ReqBody = wh_json:set_value(<<"data">>, Numbers, wh_json:new()),
            Uri = <<Url/binary, "/location">>,
            case ibrowse:send_req(binary:bin_to_list(Uri), [], 'post', wh_json:encode(ReqBody)) of
                {'error', Reason} ->
                    lager:error("number locality lookup failed: ~p", [Reason]),
                    {'error', <<"number locality lookup failed">>};
                {'ok', "200", _Headers, Body} ->
                    handle_locality_resp(wh_json:decode(Body));
                {'ok', _Status, _, _Body} ->
                    lager:error("number locality lookup failed: ~p ~p", [_Status, _Body]),
                    {'error', <<"number locality lookup failed">>}
            end
    end.

-spec handle_locality_resp(wh_json:object()) -> wh_json:object() | {'error', binary()}.
handle_locality_resp(Resp) ->
    case wh_json:get_value(<<"status">>, Resp, <<"error">>) of
        <<"success">> -> wh_json:get_value(<<"data">>, Resp);
        _E ->
            lager:error("number locality lookup failed, status: ~p", [_E]),
            {'error', <<"number locality lookup failed">>}
    end.
