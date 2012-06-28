%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Upload a rate deck, query rates for a given DID
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_rates).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,content_types_accepted/1
         ,validate/1, validate/2
         ,put/1
         ,post/1, post/2
         ,delete/2
        ]).

-include_lib("crossbar/include/crossbar.hrl").

-define(PVT_FUNS, [fun add_pvt_type/2]).
-define(PVT_TYPE, <<"rate">>).
-define(CB_LIST, <<"rates/crossbar_listing">>).

-define(UPLOAD_MIME_TYPES, [{<<"text">>, <<"csv">>}
                            ,{<<"text">>, <<"comma-separated-values">>}
                           ]).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    init_db(),

    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.rates">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.rates">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.rates">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_accepted.rates">>, ?MODULE, content_types_accepted),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.rates">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.rates">>, ?MODULE, post),
    crossbar_bindings:bind(<<"v1_resource.execute.delete.rates">>, ?MODULE, delete).

init_db() ->
    _ = couch_mgr:db_create(?WH_RATES_DB),
    _ = couch_mgr:revise_doc_from_file(?WH_RATES_DB, crossbar, "views/rates.json"),
    _ = couch_mgr:load_doc_from_file(?WH_RATES_DB, crossbar, "fixtures/us-1.json").


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods().
-spec allowed_methods/1 :: (path_token()) -> http_methods().
allowed_methods() ->
    ['GET', 'PUT', 'POST'].
allowed_methods(_) ->
    ['GET', 'POST', 'DELETE'].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
-spec resource_exists/1 :: (path_token()) -> 'true'.
resource_exists() -> true.
resource_exists(_) -> true.

-spec content_types_accepted/1 :: (#cb_context{}) -> #cb_context{}.
content_types_accepted(#cb_context{req_verb = <<"post">>}=Context) ->
    Context#cb_context{content_types_accepted = [{from_binary, ?UPLOAD_MIME_TYPES}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    summary(Context#cb_context{db_name=?WH_RATES_DB});
validate(#cb_context{req_verb = <<"put">>}=Context) ->
    create(Context#cb_context{db_name=?WH_RATES_DB});
validate(#cb_context{req_verb = <<"post">>}=Context) ->
    check_uploaded_file(Context#cb_context{db_name=?WH_RATES_DB}).

validate(#cb_context{req_verb = <<"get">>}=Context, Id) ->
    read(Id, Context#cb_context{db_name=?WH_RATES_DB});
validate(#cb_context{req_verb = <<"post">>}=Context, Id) ->
    update(Id, Context#cb_context{db_name=?WH_RATES_DB});
validate(#cb_context{req_verb = <<"delete">>}=Context, Id) ->
    read(Id, Context#cb_context{db_name=?WH_RATES_DB}).

-spec post/1 :: (#cb_context{}) -> #cb_context{}.
-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
post(#cb_context{}=Context) ->
    init_db(),
    spawn(fun() -> upload_csv(Context) end),
    crossbar_util:response_202(list_to_binary(["attempting to insert rates from the uploaded document"]), Context).
post(#cb_context{}=Context, _RateId) ->
    crossbar_doc:save(Context).

-spec put/1 :: (#cb_context{}) -> #cb_context{}.
put(#cb_context{}=Context) ->
    crossbar_doc:save(Context).

-spec delete/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
delete(#cb_context{}=Context, _RateId) ->
    crossbar_doc:delete(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create/1 :: (#cb_context{}) -> #cb_context{}.
create(#cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"rates">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            {JObj1, _} = add_pvt_fields(JObj, Context),
            Context#cb_context{doc=JObj1, resp_status=success}
    end.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
read(Id, Context) ->
    crossbar_doc:load(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing instance with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
update(Id, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"rates">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            {JObj1, _} = lists:foldr(fun(F, {J, C}) ->
                                             {F(J, C), C}
                                     end, {JObj, Context}, ?PVT_FUNS),
            crossbar_doc:load_merge(Id, JObj1, Context)
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
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the uploaded file for CSV
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec check_uploaded_file/1 :: (#cb_context{}) -> #cb_context{}.
check_uploaded_file(#cb_context{req_files=[{_Name, File}|_]}=Context) ->
    lager:debug("checking file ~s", [_Name]),
    case wh_json:get_value(<<"contents">>, File) of
        undefined ->
            Context#cb_context{resp_status=error};
        Bin when is_binary(Bin) ->
            Context#cb_context{resp_status=success}
    end;
check_uploaded_file(Context) ->
    lager:debug("no file to process"),
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add any pvt_* fields to the json object
%% @end
%%--------------------------------------------------------------------
add_pvt_fields(JObjs) when is_list(JObjs) ->
    [add_pvt_fields(JObj) || JObj <- JObjs];
add_pvt_fields(JObj) ->
    {JObj1, _} = add_pvt_fields(JObj, undefined),
    JObj1.

add_pvt_fields(JObjs, Context) when is_list(JObjs) ->
    [add_pvt_fields(JObj, Context) || JObj <- JObjs];
add_pvt_fields(JObj, Context) ->
    lists:foldr(fun(F, {J, C}) ->
                        {F(J, C), C}
                end, {JObj, Context}, ?PVT_FUNS).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% These are the pvt funs that add the necessary pvt fields to every
%% instance
%% @end
%%--------------------------------------------------------------------
-spec add_pvt_type/2 :: (wh_json:json_object(), #cb_context{} | 'undefined') -> wh_json:json_object().
add_pvt_type(JObj, _) ->
    wh_json:set_value(<<"pvt_type">>, ?PVT_TYPE, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert the file, based on content-type, to rate documents
%% @end
%%--------------------------------------------------------------------
-spec process_upload_file/1 :: (#cb_context{}) -> {'ok', {non_neg_integer(), wh_json:json_objects()}}.
process_upload_file(#cb_context{req_files=[{_Name, File}|_]}=Context) ->
    lager:debug("converting file ~s", [_Name]),
    convert_file(wh_json:get_binary_value([<<"headers">>, <<"content_type">>], File)
                 ,wh_json:get_value(<<"contents">>, File)
                 ,Context
                ).

-spec convert_file/3 :: (ne_binary(), ne_binary(), #cb_context{}) -> {'ok', {non_neg_integer(), wh_json:json_objects()}}.
convert_file(<<"text/csv">>, FileContents, Context) ->
    csv_to_rates(FileContents, Context);
convert_file(<<"text/comma-separated-values">>, FileContents, Context) ->
    csv_to_rates(FileContents, Context);
convert_file(ContentType, _, _) ->
    lager:debug("unknown content type: ~s", [ContentType]),
    throw({unknown_content_type, ContentType}).

-spec csv_to_rates/2 :: (ne_binary(), #cb_context{}) -> {'ok', {integer(), wh_json:json_objects()}}.
csv_to_rates(CSV, Context) ->
    BulkInsert = couch_util:max_bulk_insert(),
    ecsv:process_csv_binary_with(CSV
                                 ,fun(Row, {Cnt, RateDocs}) ->
                                          process_row(Context, Row, Cnt, RateDocs, BulkInsert)
                                  end
                                 ,{0, []}
                                ).

-spec process_row/2 :: ([string(),...], {integer(), wh_json:json_objects()}) -> {integer(), wh_json:json_objects()}.
process_row([Prefix, ISO, Desc, Rate], Acc) ->
    process_row([Prefix, ISO, Desc, Rate, Rate], Acc);
process_row([Prefix, ISO, Desc, InternalCost, Rate], {Cnt, RateDocs}=Acc) ->
    case catch wh_util:to_integer(Prefix) of
        {'EXIT', _} -> Acc;
        _ ->
            Prefix1 = wh_util:to_binary(Prefix),
            ISO1 = strip_quotes(wh_util:to_binary(ISO)),
            Desc1 = strip_quotes(wh_util:to_binary(Desc)),
            InternalCost1 = wh_util:to_binary(InternalCost),
            Rate1 = wh_util:to_binary(Rate),

            %% The idea here is the more expensive rate will have a higher CostF
            %% and decrement it from the weight so it has a lower weight #
            %% meaning it should be more likely used
            CostF = trunc(wh_util:to_float(InternalCost) * 100),

            {Cnt+1, [add_pvt_fields(
                       wh_json:from_list([{<<"_id">>, list_to_binary([ISO1, "-", Prefix])}
                                          ,{<<"prefix">>, Prefix1}
                                          ,{<<"iso_country_code">>, ISO1}
                                          ,{<<"description">>, Desc1}
                                          ,{<<"rate_name">>, Desc1}
                                          ,{<<"rate_cost">>, wh_util:to_float(Rate1)}
                                          ,{<<"rate_increment">>, 60}
                                          ,{<<"rate_minimum">>, 60}
                                          ,{<<"rate_surcharge">>, 0}
                                          ,{<<"pvt_rate_cost">>, wh_util:to_float(InternalCost1)}
                                          ,{<<"weight">>, constrain_weight(byte_size(Prefix1) * 10 - CostF)}
                                          ,{<<"options">>, []}
                                          ,{<<"routes">>, [<<"^\\+", (wh_util:to_binary(Prefix1))/binary, "(\\d*)$">>]}
                                          ,{<<"pvt_carrier">>, <<"default">>}
                                         ]))
                     | RateDocs]}
    end;
process_row(_Row, Acc) ->
    lager:debug("ignoring row ~p", [_Row]),
    Acc.

-spec strip_quotes/1 :: (ne_binary()) -> ne_binary().
strip_quotes(Bin) ->
    binary:replace(Bin, [<<"\"">>, <<"\'">>], <<>>, [global]).

-spec constrain_weight/1 :: (integer()) -> 1..100.
constrain_weight(X) when X =< 0 -> 1;
constrain_weight(X) when X >= 100 -> 100;
constrain_weight(X) -> X.

upload_csv(Context) ->
    _ = crossbar_util:put_reqid(Context),
    Now = erlang:now(),
    {ok, {Count, Rates}} = process_upload_file(Context),

    lager:debug("trying to save ~b rates (took ~b ms to process)", [Count, wh_util:elapsed_ms(Now)]),
    _  = crossbar_doc:save(Context#cb_context{doc=Rates}, [{publish_doc, false}]),

    lager:debug("it took ~b milli to process and save ~b rates", [wh_util:elapsed_ms(Now), Count]).

save_processed_rates(Context, Cnt) ->
    spawn(fun() ->
                  Now = erlang:now(),
                  _ = crossbar_util:put_reqid(Context),
                  _ = crossbar_doc:save(Context, [{publish_doc, false}]),
                  lager:debug("saved up to ~b docs (took ~b ms)", [Cnt, wh_util:elapsed_ms(Now)])
          end).

-spec process_row/5 :: (#cb_context{}, [string(),...], integer(), wh_json:json_objects(), integer()) -> {integer(), wh_json:json_objects()}.
process_row(Context, Row, Cnt, RateDocs, BulkInsert) ->
    RateDocs1 = case Cnt > 1 andalso (Cnt rem BulkInsert) =:= 0 of
                    false -> RateDocs;
                    true ->
                        save_processed_rates(Context#cb_context{doc=RateDocs}, Cnt),
                        []
                end,
    process_row(Row, {Cnt, RateDocs1}).
