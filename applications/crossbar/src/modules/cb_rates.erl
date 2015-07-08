%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%% Upload a rate deck, query rates for a given DID
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_rates).

-export([init/0
         ,authorize/1
         ,allowed_methods/0, allowed_methods/1 ,allowed_methods/2
         ,resource_exists/0, resource_exists/1 ,resource_exists/2
         ,content_types_accepted/1
         ,validate/1, validate/2, validate/3
         ,put/1
         ,post/1, post/2
         ,patch/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(PVT_FUNS, [fun add_pvt_type/2]).
-define(PVT_TYPE, <<"rate">>).
-define(NUMBER, <<"number">>).
-define(CB_LIST, <<"rates/crossbar_listing">>).

-define(UPLOAD_MIME_TYPES, [{<<"text">>, <<"csv">>}
                            ,{<<"text">>, <<"comma-separated-values">>}
                           ]).

-define(NUMBER_RESP_FIELDS, [<<"Prefix">>, <<"Rate-Name">>
                             ,<<"Rate-Description">>, <<"Base-Cost">>
                             ,<<"Rate">>, <<"Rate-Minimum">>
                             ,<<"Rate-Increment">>, <<"Surcharge">>
                             ,<<"E164-Number">>
                            ]).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = init_db(),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.rates">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.rates">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.rates">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.rates">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.execute.put.rates">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.rates">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.rates">>, ?MODULE, 'patch'),
    crossbar_bindings:bind(<<"*.execute.delete.rates">>, ?MODULE, 'delete').

init_db() ->
    _ = couch_mgr:db_create(?WH_RATES_DB),
    couch_mgr:revise_doc_from_file(?WH_RATES_DB, 'crossbar', "views/rates.json").

authorize(Context) ->
    authorize(Context, cb_context:req_nouns(Context)).

authorize(_Context, [{<<"rates">>, [?NUMBER, _NumberToRate]}]) ->
    lager:debug("authorizing rate request for ~s", [_NumberToRate]),
    'true';
authorize(_Context, _Nouns) ->
    'false'.

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
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

-spec allowed_methods(path_token(),path_token()) -> http_methods().
allowed_methods(?NUMBER,_) ->
    [?HTTP_GET].

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

-spec resource_exists(path_token(),path_token()) -> 'true'.
resource_exists(?NUMBER,_) -> 'true'.

-spec content_types_accepted(cb_context:context()) -> cb_context:context().
content_types_accepted(Context) ->
    content_types_accepted_by_verb(Context, cb_context:req_verb(Context)).

-spec content_types_accepted_by_verb(cb_context:context(), http_method()) -> cb_context:context().
content_types_accepted_by_verb(Context, ?HTTP_POST) ->
    cb_context:set_content_types_accepted(Context, [{'from_binary', ?UPLOAD_MIME_TYPES}]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_rates(Context, cb_context:req_verb(Context)).
validate(Context, Id) ->
    validate_rate(Context, Id, cb_context:req_verb(Context)).
validate(Context, ?NUMBER, Phonenumber) ->
    validate_number(Phonenumber, Context).

-spec validate_rates(cb_context:context(), http_method()) -> cb_context:context().
validate_rates(Context, ?HTTP_GET) ->
    summary(cb_context:set_account_db(Context, ?WH_RATES_DB));
validate_rates(Context, ?HTTP_PUT) ->
    create(cb_context:set_account_db(Context, ?WH_RATES_DB));
validate_rates(Context, ?HTTP_POST) ->
    check_uploaded_file(cb_context:set_account_db(Context, ?WH_RATES_DB)).

-spec validate_rate(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_rate(Context, Id, ?HTTP_GET) ->
    read(Id, cb_context:set_account_db(Context, ?WH_RATES_DB));
validate_rate(Context, Id, ?HTTP_POST) ->
    update(Id, cb_context:set_account_db(Context, ?WH_RATES_DB));
validate_rate(Context, Id, ?HTTP_PATCH) ->
    validate_patch(Id, cb_context:set_account_db(Context, ?WH_RATES_DB));
validate_rate(Context, Id, ?HTTP_DELETE) ->
    read(Id, cb_context:set_account_db(Context, ?WH_RATES_DB)).

-spec post(cb_context:context()) -> cb_context:context().
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context) ->
    _ = init_db(),
    _ = wh_util:spawn(fun() -> upload_csv(Context) end),
    crossbar_util:response_202(<<"attempting to insert rates from the uploaded document">>, Context).
post(Context, _RateId) ->
    crossbar_doc:save(Context).

patch(Context, _RateId) ->
    crossbar_doc:save(Context).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _RateId) ->
    crossbar_doc:delete(Context).

-spec validate_number(ne_binary(), cb_context:context()) -> cb_context:context().
validate_number(Phonenumber, Context) ->
    case wnm_util:is_reconcilable(Phonenumber) of
        'true' ->
            rate_for_number(wnm_util:to_e164(Phonenumber), Context);
        'false' ->
            cb_context:add_validation_error(
              <<"number format">>
              ,<<"error">>
              ,wh_json:from_list([{<<"message">>, <<"Number is un-rateable">>}])
              ,Context
             )
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"rates">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"rates">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update-merge an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec validate_patch(ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch(Id, Context) ->
    crossbar_doc:patch_and_validate(Id, Context, fun update/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context, wh_doc:set_type(cb_context:doc(Context), <<"rate">>));
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the uploaded file for CSV
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec check_uploaded_file(cb_context:context()) -> cb_context:context().
check_uploaded_file(Context) ->
    check_uploaded_file(Context, cb_context:req_files(Context)).

check_uploaded_file(Context, [{_Name, File}|_]) ->
    lager:debug("checking file ~s", [_Name]),
    case wh_json:get_value(<<"contents">>, File) of
        'undefined' ->
            cb_context:add_validation_error(
                <<"file">>
                ,<<"required">>
                ,wh_json:from_list([
                    {<<"message">>, <<"file contents not found">>}
                 ])
                ,Context
            );
        Bin when is_binary(Bin) ->
            lager:debug("file: ~s", [Bin]),
            cb_context:set_resp_status(Context, 'success')
    end;
check_uploaded_file(Context, _ReqFiles) ->
    cb_context:add_validation_error(
        <<"file">>
        ,<<"required">>
        ,wh_json:from_list([
            {<<"message">>, <<"no file to process">>}
         ])
        ,Context
    ).

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
-spec upload_csv(cb_context:context()) -> 'ok'.
upload_csv(Context) ->
    _ = cb_context:put_reqid(Context),
    Now = erlang:now(),
    {'ok', {Count, Rates}} = process_upload_file(Context),
    lager:debug("trying to save ~b rates (took ~b ms to process)", [Count, wh_util:elapsed_ms(Now)]),
    _  = crossbar_doc:save(cb_context:set_doc(Context, Rates), [{'publish_doc', 'false'}]),
    lager:debug("it took ~b milli to process and save ~b rates", [wh_util:elapsed_ms(Now), Count]).

-spec process_upload_file(cb_context:context()) ->
                                 {'ok', {non_neg_integer(), wh_json:objects()}}.
process_upload_file(Context) ->
    process_upload_file(Context, cb_context:req_files(Context)).
process_upload_file(Context, [{_Name, File}|_]) ->
    lager:debug("converting file ~s", [_Name]),
    convert_file(wh_json:get_binary_value([<<"headers">>, <<"content_type">>], File)
                 ,wh_json:get_value(<<"contents">>, File)
                 ,Context
                );
process_upload_file(Context, _ReqFiles) ->
    cb_context:add_validation_error(
        <<"file">>
        ,<<"required">>
        ,wh_json:from_list([
            {<<"message">>, <<"no file to process">>}
         ])
        ,Context
    ).

-spec convert_file(ne_binary(), ne_binary(), cb_context:context()) ->
                          {'ok', {non_neg_integer(), wh_json:objects()}}.
convert_file(<<"text/csv">>, FileContents, Context) ->
    csv_to_rates(FileContents, Context);
convert_file(<<"text/comma-separated-values">>, FileContents, Context) ->
    csv_to_rates(FileContents, Context);
convert_file(ContentType, _, _) ->
    lager:debug("unknown content type: ~s", [ContentType]),
    throw({'unknown_content_type', ContentType}).

-spec csv_to_rates(ne_binary(), cb_context:context()) ->
                          {'ok', {integer(), wh_json:objects()}}.
csv_to_rates(CSV, Context) ->
    BulkInsert = couch_util:max_bulk_insert(),
    ecsv:process_csv_binary_with(CSV
                                 ,fun(Row, {Count, JObjs}) ->
                                          process_row(Context, Row, Count, JObjs, BulkInsert)
                                  end
                                 ,{0, []}
                                ).

%% NOTE: Support row formats-
%%    [Prefix, ISO, Desc, Rate]
%%    [Prefix, ISO, Desc, InternalRate, Rate]
%%    [Prefix, ISO, Desc, Surcharge, InternalRate, Rate]
%%    [Prefix, ISO, Desc, InternalSurcharge, Surcharge, InternalRate, Rate]

-type rate_row() :: [string(),...] | string().
-type rate_row_acc() :: {integer(), wh_json:objects()}.

-spec process_row(cb_context:context(), rate_row(), integer(), wh_json:objects(), integer()) ->
                         rate_row_acc().
process_row(Context, Row, Count, JObjs, BulkInsert) ->
    J = case Count > 1 andalso (Count rem BulkInsert) =:= 0 of
            'false' -> JObjs;
            'true' ->
                _Pid = save_processed_rates(cb_context:set_doc(Context, JObjs), Count),
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
            Weight = constrain_weight(byte_size(wh_util:to_binary(Prefix)) * 10
                                      - trunc(InternalRate * 100)),
            Id = <<ISO/binary, "-", (wh_util:to_binary(Prefix))/binary>>,
            Props = props:filter_undefined(
                      [{<<"_id">>, Id}
                       ,{<<"prefix">>, wh_util:to_binary(Prefix)}
                       ,{<<"weight">>, Weight}
                       ,{<<"description">>, Description}
                       ,{<<"rate_name">>, Id}
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

-spec get_row_internal_surcharge(rate_row()) -> api_float().
get_row_internal_surcharge([_, _, _, InternalSurcharge, _, _ | _]) ->
    wh_util:to_float(InternalSurcharge);
get_row_internal_surcharge(_R) ->
    lager:info("internal surcharge not found on row: ~p", [_R]),
    'undefined'.

-spec get_row_surcharge(rate_row()) -> api_float().
get_row_surcharge([_, _, _, Surcharge, _, _]) ->
    wh_util:to_float(Surcharge);
get_row_surcharge([_, _, _, _, Surcharge, _ | _]) ->
    wh_util:to_float(Surcharge);
get_row_surcharge([_|_]=_R) ->
    lager:info("surcharge not found on row: ~p", [_R]),
    'undefined'.

-spec get_row_internal_rate(rate_row()) -> api_float().
get_row_internal_rate([_, _, _, Rate]) ->
    wh_util:to_float(Rate);
get_row_internal_rate([_, _, _, InternalRate, _]) ->
    wh_util:to_float(InternalRate);
get_row_internal_rate([_, _, _, _, InternalRate, _]) ->
    wh_util:to_float(InternalRate);
get_row_internal_rate([_, _, _, _, _, InternalRate | _]) ->
    wh_util:to_float(InternalRate);
get_row_internal_rate([_|_]=_R) ->
    lager:info("internal rate not found on row: ~p", [_R]),
    'undefined'.

-spec get_row_rate(rate_row()) -> api_float().
get_row_rate([_, _, _, Rate]) -> wh_util:to_float(Rate);
get_row_rate([_, _, _, _, Rate]) -> wh_util:to_float(Rate);
get_row_rate([_, _, _, _, _, Rate]) -> wh_util:to_float(Rate);
get_row_rate([_, _, _, _, _, _, Rate | _]) -> wh_util:to_float(Rate);
get_row_rate([_|_]=_R) ->
    lager:info("rate not found on row: ~p", [_R]),
    'undefined'.

-spec strip_quotes(ne_binary()) -> ne_binary().
strip_quotes(Bin) ->
    binary:replace(Bin, [<<"\"">>, <<"\'">>], <<>>, ['global']).

-spec constrain_weight(integer()) -> 1..100.
constrain_weight(X) when X =< 0 -> 1;
constrain_weight(X) when X >= 100 -> 100;
constrain_weight(X) -> X.

-spec save_processed_rates(cb_context:context(), integer()) -> pid().
save_processed_rates(Context, Count) ->
    wh_util:spawn(
      fun() ->
              Now = erlang:now(),
              _ = cb_context:put_reqid(Context),
              _ = crossbar_doc:save(Context, [{'publish_doc', 'false'}]),
              lager:debug("saved up to ~b docs (took ~b ms)", [Count, wh_util:elapsed_ms(Now)])
      end).

-spec rate_for_number(ne_binary(), cb_context:context()) -> cb_context:context().
rate_for_number(Phonenumber, Context) ->
    case wh_amqp_worker:call([{<<"To-DID">>, Phonenumber}
                              ,{<<"Send-Empty">>, 'true'}
                              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                             ]
                             ,fun wapi_rate:publish_req/1
                             ,fun wapi_rate:resp_v/1
                             ,10 * ?MILLISECONDS_IN_SECOND
                            )
    of
        {'ok', Rate} ->
            lager:debug("found rate for ~s", [Phonenumber]),
            maybe_handle_rate(Phonenumber, Context, Rate);
        _E ->
            lager:debug("failed to query for number ~s: ~p", [Phonenumber, _E]),
            cb_context:add_system_error('No rate found for this number', Context)
    end.

-spec maybe_handle_rate(ne_binary(), cb_context:context(), wh_json:object()) ->
                               cb_context:context().
maybe_handle_rate(Phonenumber, Context, Rate) ->
    case wh_json:get_value(<<"Base-Cost">>, Rate) of
        'undefined' ->
            lager:debug("empty rate response for ~s", [Phonenumber]),
            cb_context:add_system_error('No rate found for this number', Context);
        _BaseCost ->
            normalize_view(wh_json:set_value(<<"E164-Number">>, Phonenumber, Rate), Context)
    end.

-spec normalize_view(wh_json:object(),cb_context:context()) -> cb_context:context().
normalize_view(Rate, Context) ->
    crossbar_util:response(filter_view(Rate), Context).

-spec filter_view(wh_json:object()) -> wh_json:object().
filter_view(Rate) ->
    normalize_fields(
      wh_json:filter(fun filter_fields/1, wh_api:remove_defaults(Rate))
     ).

-spec filter_fields(tuple()) -> boolean().
filter_fields({K,_}) ->
    lists:member(K, ?NUMBER_RESP_FIELDS).

-spec normalize_fields(wh_json:object()) -> wh_json:object().
normalize_fields(Rate) ->
    wh_json:map(fun normalize_field/2, Rate).

-spec normalize_field(wh_json:key(), wh_json:json_term()) ->
                             {wh_json:key(), wh_json:json_term()}.
normalize_field(<<"Base-Cost">> = K, BaseCost) ->
    {K, wht_util:units_to_dollars(BaseCost)};
normalize_field(<<"Rate">> = K, Rate) ->
    {K, wht_util:units_to_dollars(Rate)};
normalize_field(<<"Surcharge">> = K, Surcharge) ->
    {K, wht_util:units_to_dollars(Surcharge)};
normalize_field(K, V) ->
    {K, V}.
