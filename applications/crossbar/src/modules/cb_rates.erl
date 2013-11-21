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

-include("../crossbar.hrl").

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
    _ = init_db(),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.rates">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.rates">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.rates">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.rates">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.execute.put.rates">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.rates">>, ?MODULE, 'post'),
    crossbar_bindings:bind(<<"*.execute.delete.rates">>, ?MODULE, 'delete').

init_db() ->
    _ = couch_mgr:db_create(?WH_RATES_DB),
    couch_mgr:revise_doc_from_file(?WH_RATES_DB, 'crossbar', "views/rates.json").

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

-spec content_types_accepted(#cb_context{}) -> #cb_context{}.
content_types_accepted(#cb_context{req_verb = ?HTTP_POST}=Context) ->
    Context#cb_context{content_types_accepted = [{'from_binary', ?UPLOAD_MIME_TYPES}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(#cb_context{}) -> #cb_context{}.
-spec validate(#cb_context{}, path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    summary(Context#cb_context{db_name=?WH_RATES_DB});
validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    create(Context#cb_context{db_name=?WH_RATES_DB});
validate(#cb_context{req_verb = ?HTTP_POST}=Context) ->
    check_uploaded_file(Context#cb_context{db_name=?WH_RATES_DB}).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id) ->
    read(Id, Context#cb_context{db_name=?WH_RATES_DB});
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id) ->
    update(Id, Context#cb_context{db_name=?WH_RATES_DB});
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, Id) ->
    read(Id, Context#cb_context{db_name=?WH_RATES_DB}).

-spec post(#cb_context{}) -> #cb_context{}.
-spec post(#cb_context{}, path_token()) -> #cb_context{}.
post(#cb_context{}=Context) ->
    _ = init_db(),
    spawn(fun() -> upload_csv(Context) end),
    crossbar_util:response_202(<<"attempting to insert rates from the uploaded document">>, Context).
post(#cb_context{}=Context, _RateId) ->
    crossbar_doc:save(Context).

-spec put(#cb_context{}) -> #cb_context{}.
put(#cb_context{}=Context) ->
    crossbar_doc:save(Context).

-spec delete(#cb_context{}, path_token()) -> #cb_context{}.
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
-spec create(#cb_context{}) -> #cb_context{}.
create(#cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"rates">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), #cb_context{}) -> #cb_context{}.
read(Id, Context) ->
    crossbar_doc:load(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), #cb_context{}) -> #cb_context{}.
update(Id, #cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"rates">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation('undefined' | ne_binary(), #cb_context{}) -> #cb_context{}.
on_successful_validation(undefined, #cb_context{doc=JObj}=Context) ->
    Context#cb_context{doc=wh_json:set_value(<<"pvt_type">>, <<"rate">>, JObj)};
on_successful_validation(Id, #cb_context{}=Context) ->
    crossbar_doc:load_merge(Id, Context).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(#cb_context{}) -> #cb_context{}.
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the uploaded file for CSV
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec check_uploaded_file(#cb_context{}) -> #cb_context{}.
check_uploaded_file(#cb_context{req_files=[{_Name, File}|_]}=Context) ->
    lager:debug("checking file ~s", [_Name]),
    case wh_json:get_value(<<"contents">>, File) of
        'undefined' ->
            Message = <<"file contents not found">>,
            cb_context:add_validation_error(<<"file">>, <<"required">>, Message, Context);
        Bin when is_binary(Bin) ->
            Context#cb_context{resp_status='success'}
    end;
check_uploaded_file(Context) ->
    Message = <<"no file to process">>,
    cb_context:add_validation_error(<<"file">>, <<"required">>, Message, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert the file, based on content-type, to rate documents
%% @end
%%--------------------------------------------------------------------
-spec upload_csv(#cb_context{}) -> 'ok'.
upload_csv(Context) ->
    _ = cb_context:put_reqid(Context),
    Now = erlang:now(),
    {'ok', {Count, Rates}} = process_upload_file(Context),
    lager:debug("trying to save ~b rates (took ~b ms to process)", [Count, wh_util:elapsed_ms(Now)]),
    _  = crossbar_doc:save(Context#cb_context{doc=Rates}, [{'publish_doc', 'false'}]),
    lager:debug("it took ~b milli to process and save ~b rates", [wh_util:elapsed_ms(Now), Count]).

-spec process_upload_file(#cb_context{}) -> {'ok', {non_neg_integer(), wh_json:objects()}}.
process_upload_file(#cb_context{req_files=[{_Name, File}|_]}=Context) ->
    lager:debug("converting file ~s", [_Name]),
    convert_file(wh_json:get_binary_value([<<"headers">>, <<"content_type">>], File)
                 ,wh_json:get_value(<<"contents">>, File)
                 ,Context
                ).

-spec convert_file(ne_binary(), ne_binary(), #cb_context{}) -> {'ok', {non_neg_integer(), wh_json:objects()}}.
convert_file(<<"text/csv">>, FileContents, Context) ->
    csv_to_rates(FileContents, Context);
convert_file(<<"text/comma-separated-values">>, FileContents, Context) ->
    csv_to_rates(FileContents, Context);
convert_file(ContentType, _, _) ->
    lager:debug("unknown content type: ~s", [ContentType]),
    throw({'unknown_content_type', ContentType}).

-spec csv_to_rates(ne_binary(), #cb_context{}) -> {'ok', {integer(), wh_json:objects()}}.
csv_to_rates(CSV, Context) ->
    BulkInsert = couch_util:max_bulk_insert(),
    ecsv:process_csv_binary_with(CSV
                                 ,fun(Row, {Count, JObjs}) ->
                                          process_row(Context, Row, Count, JObjs, BulkInsert)
                                  end
                                 ,{0, []}
                                ).

%% NOTE: Support row formates-
%%    [Prefix, ISO, Desc, Rate]
%%    [Prefix, ISO, Desc, InternalRate, Rate]
%%    [Prefix, ISO, Desc, Surcharge, InternalRate, Rate]
%%    [Prefix, ISO, Desc, InternalSurcharge, Surcharge, InternalRate, Rate]

-type rate_row() :: [string(),...].
-type rate_row_acc() :: {integer(), wh_json:objects()}.

-spec process_row(#cb_context{}, rate_row(), integer(), wh_json:objects(), integer()) -> rate_row_acc().
process_row(Context, Row, Count, JObjs, BulkInsert) ->
    J = case Count > 1 andalso (Count rem BulkInsert) =:= 0 of
            'false' -> JObjs;
            'true' ->
                save_processed_rates(Context#cb_context{doc=JObjs}, Count),
                []
        end,
    process_row(Row, {Count, J}).

-spec process_row(rate_row(), rate_row_acc()) -> rate_row_acc().
process_row(Row, {Count, JObjs}=Acc) ->
    case get_row_prefix(Row) of
        'undefined' -> Acc;
        Prefix ->
            ISO = get_row_iso(Row),
            Description = get_row_description(Row),
            InternalRate = get_row_internal_rate(Row),
            %% The idea here is the more expensive rate will have a higher CostF
            %% and decrement it from the weight so it has a lower weight #
            %% meaning it should be more likely used
            Weight = constrain_weight(byte_size(wh_util:to_binay(Prefix)) * 10
                                      - trunc(InternalRate * 100)),
            Id = <<ISO/binary, "-", (wh_util:to_binary(Prefix))/binary>>,
            Props = props:filter_undefined([{<<"_id">>, Id}
                                            ,{<<"prefix">>, Prefix}
                                            ,{<<"weight">>, Weight}
                                            ,{<<"description">>, Description}
                                            ,{<<"rate_name">>, Description}
                                            ,{<<"iso_country_code">>, ISO}
                                            ,{<<"pvt_rate_cost">>, InternalRate}
                                            ,{<<"pvt_carrier">>, <<"default">>}
                                            ,{<<"pvt_type">>, <<"rate">>}
                                            ,{<<"rate_increment">>, 60}
                                            ,{<<"rate_minimum">>, 60}
                                            ,{<<"rate_surcharge">>, get_row_surcharge(Row)}
                                            ,{<<"rate_cost">>, get_row_rate(Row)}
                                            ,{<<"pvt_rate_surcharge">>, get_row_internal_surcharge(Row)}
                                            ,{<<"routes">>, [<<"^\\+", (wh_util:to_binary(Prefix))/binary, "(\\d*)$">>]}
                                            ,{?HTTP_OPTIONS, []}
                                         ]),
                
            {Count + 1, [wh_json:from_list(Props) | JObjs]}
    end.

-spec get_row_prefix(rate_row()) -> api_binary().
get_row_prefix([Prefix | _]=_R) ->
    try wh_util:to_integer(Prefix) of
        P -> P
    catch
        _:_ -> 
            lager:info("non-integer prefix on row: ~p", [_R]),
            'undefined'
    end;
get_row_prefix(_R) ->
    lager:info("prefix not found on row: ~p", [_R]),
    'undefined'.

-spec get_row_iso(rate_row()) -> ne_binary().
get_row_iso([_, ISO | _]) -> strip_quotes(wh_util:to_binary(ISO));
get_row_iso(_R) ->
    lager:info("iso not found on row: ~p", [_R]),
    <<"XX">>.

-spec get_row_description(rate_row()) -> api_binary().
get_row_description([_, _, Description | _]) ->
    strip_quotes(wh_util:to_binary(Description));
get_row_description(_R) ->
    lager:info("description not found on row: ~p", [_R]),
    'undefined'.

-spec get_row_internal_surcharge(rate_row()) -> api_binary().
get_row_internal_surcharge([_, _, _, InternalSurcharge, _, _ | _]) ->
    wh_util:to_float(InternalSurcharge);
get_row_internal_surcharge(_R) ->
    lager:info("internal surcharge not found on row: ~p", [_R]),
    'undefined'.

-spec get_row_surcharge(rate_row()) -> api_binary().
get_row_surcharge([_, _, _, Surcharge, _, _]) ->
    get_row_surcharge(Surcharge);
get_row_surcharge([_, _, _, _, Surcharge, _ | _]) ->
    get_row_surcharge(Surcharge);
get_row_surcharge([_|_]=_R) -> 
    lager:info("surcharge not found on row: ~p", [_R]),
    'undefined';
get_row_surcharge(Surcharge) ->
    wh_util:to_float(Surcharge).

-spec get_row_internal_rate(rate_row()) -> api_binary().
get_row_internal_rate([_, _, _, Rate]) -> get_row_internal_rate(Rate);
get_row_internal_rate([_, _, _, InternalRate, _]) ->
    get_row_internal_rate(InternalRate);
get_row_internal_rate([_, _, _, _, InternalRate, _]) ->
    get_row_internal_rate(InternalRate);
get_row_internal_rate([_, _, _, _, _, InternalRate | _]) -> 
    get_row_internal_rate(InternalRate);
get_row_internal_rate([_|_]=_R) ->
    lager:info("internal rate not found on row: ~p", [_R]),
    'undefined';
get_row_internal_rate(InternalRate) ->
    wh_util:to_float(InternalRate).

-spec get_row_rate(rate_row()) -> api_binary().
get_row_rate([_, _, _, Rate]) -> get_row_rate(Rate);
get_row_rate([_, _, _, _, Rate]) -> get_row_rate(Rate);
get_row_rate([_, _, _, _, _, Rate]) -> get_row_rate(Rate);
get_row_rate([_, _, _, _, _, _, Rate | _]) -> get_row_rate(Rate);
get_row_rate([_|_]=_R) ->
    lager:info("rate not found on row: ~p", [_R]),
    'undefined';
get_row_rate(Rate) -> wh_util:to_float(Rate).

-spec strip_quotes(ne_binary()) -> ne_binary().
strip_quotes(Bin) ->
    binary:replace(Bin, [<<"\"">>, <<"\'">>], <<>>, ['global']).

-spec constrain_weight(integer()) -> 1..100.
constrain_weight(X) when X =< 0 -> 1;
constrain_weight(X) when X >= 100 -> 100;
constrain_weight(X) -> X.

-spec save_processed_rates(#cb_context{}, integer()) -> pid().
save_processed_rates(Context, Count) ->
    spawn(fun() ->
                  Now = erlang:now(),
                  _ = cb_context:put_reqid(Context),
                  _ = crossbar_doc:save(Context, [{'publish_doc', 'false'}]),
                  lager:debug("saved up to ~b docs (took ~b ms)", [Count, wh_util:elapsed_ms(Now)])
          end).
