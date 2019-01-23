%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_service_added).
-behaviour(teletype_gen_email_template).

-export([id/0
        ,init/0
        ,macros/0, macros/1
        ,subject/0
        ,category/0
        ,friendly_name/0
        ,to/0, from/0, cc/0, bcc/0, reply_to/0
        ]).
-export([handle_req/1]).

-include("teletype.hrl").

-spec id() -> kz_term:ne_binary().
id() -> <<"service_added">>.

-spec macros() -> kz_json:object().
macros() ->
    kz_json:from_list(
      [?MACRO_VALUE(<<"affected.id">>, <<"affected_id">>, <<"Affected Account ID">>, <<"Affected Account ID">>)
      ,?MACRO_VALUE(<<"affected.name">>, <<"affected_name">>, <<"Affected Account Name">>, <<"Affected Account Name">>)
      ,?MACRO_VALUE(<<"affected.realm">>, <<"affected_realm">>, <<"Affected Account Realm">>, <<"Affected Account Realm">>)
      ,?MACRO_VALUE(<<"affected.language">>, <<"affected_language">>, <<"Affected Account Language">>, <<"Affected Account Language">>)
      ,?MACRO_VALUE(<<"affected.timezone">>, <<"affected_timezone">>, <<"Affected Account Timezone">>, <<"Affected Account Timezone">>)
      ,?MACRO_VALUE(<<"authentication.type">>, <<"authentication_type">>, <<"Authentication Type">>, <<"Type of authentication used by user">>)
      ,?MACRO_VALUE(<<"authentication.account_id">>, <<"authentication_account_id">>, <<"Authentication Account ID">>, <<"Account ID of authentication">>)
      ,?MACRO_VALUE(<<"authentication.account_name">>, <<"authentication_account_name">>, <<"Authentication Account Name">>, <<"Account name of authentication">>)
      ,?MACRO_VALUE(<<"cascade.id">>, <<"cascade_id">>, <<"cascade Account ID">>, <<"Changes cascaded from Account ID">>)
      ,?MACRO_VALUE(<<"cascade.name">>, <<"cascade_name">>, <<"cascade Account Name">>, <<"Changes cascaded from Account Name">>)
      ,?MACRO_VALUE(<<"cascade.realm">>, <<"cascade_realm">>, <<"cascade Account Realm">>, <<"Changes cascaded from Account Realm">>)
      ,?MACRO_VALUE(<<"cascade.language">>, <<"cascade_language">>, <<"cascade Account Language">>, <<"Changes cascaded from Account Language">>)
      ,?MACRO_VALUE(<<"cascade.timezone">>, <<"cascade_timezone">>, <<"cascade Account Timezone">>, <<"Changes cascaded from Timezone">>)
      ,?MACRO_VALUE(<<"invoice.items.[item_name].category">>, <<"invoice_item_category">>, <<"Invoice Item Category">>, <<"Ccategory name that the item belongs to">>)
      ,?MACRO_VALUE(<<"invoice.items.[item_name].changes.type">>, <<"invoice_item_change_type">>, <<"Invoice Item Change Type">>, <<"The type of change to the item">>)
      ,?MACRO_VALUE(<<"invoice.items.[item_name].changes.quantity">>, <<"invoice_item_change_quantity">>, <<"Invoice Item Change Quantity">>, <<"Quantity amount affected the item">>)
      ,?MACRO_VALUE(<<"invoice.items.[item_name].discounts">>, <<"invoice_item_discounts">>, <<"Invoice Item Discounts">>, <<"Item's discounts">>)
      ,?MACRO_VALUE(<<"invoice.items.[item_name].item">>, <<"invoice_item_item">>, <<"Invoice Item">>, <<"Item in the category">>)
      ,?MACRO_VALUE(<<"invoice.items.[item_name].quantity">>, <<"invoice_item_quantity">>, <<"Invoice Item Quantity">>, <<"Item's quantity">>)
      ,?MACRO_VALUE(<<"invoice.items.[item_name].rate">>, <<"invoice_item_rate">>, <<"Invoice Item Rate">>, <<"Item's rate">>)
      ,?MACRO_VALUE(<<"invoice.items.[item_name].total">>, <<"invoice_item_total">>, <<"Invoice Item Total">>, <<"Item's total price">>)
      ,?MACRO_VALUE(<<"invoice.total_discounts">>, <<"invoice_total_discounts">>, <<"Invoice Total Discounts">>, <<"Invoice total discounts">>)
      ,?MACRO_VALUE(<<"invoice.total">>, <<"invoice_total">>, <<"Invoice Total">>, <<"Invoice total price">>)
      ,?MACRO_VALUE(<<"reseller.id">>, <<"reseller_id">>, <<"Reseller Account ID">>, <<"Reseller Account ID">>)
      ,?MACRO_VALUE(<<"reseller.name">>, <<"reseller_name">>, <<"Reseller Account Name">>, <<"Reseller Account Name">>)
      ,?MACRO_VALUE(<<"reseller.realm">>, <<"reseller_realm">>, <<"Reseller Account Realm">>, <<"Reseller Account Realm">>)
      ,?MACRO_VALUE(<<"reseller.language">>, <<"reseller_language">>, <<"Reseller Account Language">>, <<"Reseller Account Language">>)
      ,?MACRO_VALUE(<<"reseller.timezone">>, <<"reseller_timezone">>, <<"Reseller Account Timezone">>, <<"Reseller Account Timezone">>)
      ,?MACRO_VALUE(<<"timestamp.utc">>, <<"date_called_utc">>, <<"Date (UTC)">>, <<"When was the change happened (UTC)">>)
      ,?MACRO_VALUE(<<"timestamp.local">>, <<"date_called_local">>, <<"Date (Local)">>, <<"When was the changed happened (Local time)">>)
       | ?USER_MACROS
       ++ ?SYSTEM_MACROS
      ]).

-spec subject() -> kz_term:ne_binary().
subject() -> <<"Service change invoice for account '{{affected.name}}'">>.

-spec category() -> kz_term:ne_binary().
category() -> <<"account">>.

-spec friendly_name() -> kz_term:ne_binary().
friendly_name() -> <<"Service Change Invoice">>.

-spec to() -> kz_json:object().
to() -> ?CONFIGURED_EMAILS(?EMAIL_ADMINS).

-spec from() -> kz_term:api_ne_binary().
from() -> teletype_util:default_from_address().

-spec cc() -> kz_json:object().
cc() -> ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, []).

-spec bcc() -> kz_json:object().
bcc() -> ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, []).

-spec reply_to() -> kz_term:api_ne_binary().
reply_to() -> teletype_util:default_reply_to().

-spec init() -> 'ok'.
init() ->
    kz_util:put_callid(?MODULE),
    teletype_templates:init(?MODULE),
    teletype_bindings:bind(id(), ?MODULE, 'handle_req').

-spec handle_req(kz_json:object()) -> template_response().
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:service_added_v(JObj)).

-spec handle_req(kz_json:object(), boolean()) -> template_response().
handle_req(_, 'false') ->
    lager:debug("invalid data for ~s", [id()]),
    teletype_util:notification_failed(id(), <<"validation_failed">>);
handle_req(JObj, 'true') ->
    lager:debug("valid data for ~s, processing...", [id()]),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    maybe_handle_req(DataJObj, teletype_util:is_notice_enabled(AccountId, JObj, id())).

-spec maybe_handle_req(kz_json:object(), boolean()) -> template_response().
maybe_handle_req(DataJObj, 'true') ->
    process_req(DataJObj);
maybe_handle_req(DataJObj, 'false') ->
    teletype_util:notification_disabled(DataJObj, id()).

-spec process_req(kz_json:object()) -> template_response().
process_req(DataJObj) ->
    Macros = props:filter_undefined(macros(DataJObj)),

    %% Load templates
    RenderedTemplates = teletype_templates:render(id(), Macros, DataJObj),

    AccountId = kapi_notifications:account_id(DataJObj),
    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(id(), AccountId),
    Subject0 = kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj]),
    Subject = teletype_util:render_subject(Subject0, Macros),
    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, id()),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:notification_completed(id());
        {'error', Reason} -> teletype_util:notification_failed(id(), Reason)
    end.

-spec macros(kz_json:object()) -> kz_term:proplist().
macros(DataJObj) ->
    macros(DataJObj, teletype_util:is_preview(DataJObj)).

-spec macros(kz_json:object(), boolean()) -> kz_term:proplist().
macros(DataJObj, 'true') ->
    {'ok', Invoice} = teletype_util:read_preview_doc(<<"service_invoice">>),
    {'ok', AccountJObj} = teletype_util:read_preview_doc(<<"account">>),
    Account = [{<<"name">>, kzd_accounts:name(AccountJObj)}
              ,{<<"realm">>, kzd_accounts:realm(AccountJObj)}
              ,{<<"id">>, kz_doc:id(AccountJObj)}
              ,{<<"language">>, kzd_accounts:language(AccountJObj)}
              ,{<<"timezone">>, kzd_accounts:timezone(AccountJObj)}
              ],
    Auth = [{<<"type">>, <<"x-auth-token">>}
           ,{<<"account_id">>, kz_doc:id(AccountJObj)}
           ,{<<"account_name">>, kzd_accounts:name(AccountJObj)}
           ],
    Request = [{<<"id">>, <<"qweasdzxc123456">>}
              ,{<<"client_ip">>, <<"192.168.0.1">>}
              ,{<<"method">>, <<"PUT">>}
              ,{<<"path">>, <<"/v2/accounts/example_account_id/">>}
              ],
    {'ok', UserJObj} = teletype_util:read_preview_doc(<<"user">>),
    [{<<"affected">>, Account}
    ,{<<"authentication">>, Auth}
    ,{<<"cascade">>, Account}
    ,{<<"invoice">>, kz_json:recursive_to_proplist(Invoice)}
    ,{<<"reseller">>, Account}
    ,{<<"request">>, Request}
    ,{<<"system">>, teletype_util:system_params()}
    ,{<<"timestamp">>, teletype_util:fix_timestamp(kz_time:now_s(), DataJObj)}
    ,{<<"user">>, teletype_util:user_params(UserJObj)}
    ];
macros(DataJObj, 'false') ->
    Timestamp = timestamp(DataJObj),
    Invoice = invoice_data(DataJObj),
    Reseller = reseller_info_data(DataJObj),
    Affected = affected_account_data(DataJObj),
    Cascade = cascade_account_data(DataJObj, Affected),
    [{<<"account">>, Reseller} %% backward compatibility
    ,{<<"affected">>, Affected}
    ,{<<"authentication">>, authentication_data(DataJObj)}
    ,{<<"cascade">>, Cascade}
    ,{<<"invoice">>, Invoice}
    ,{<<"reseller">>, Reseller}
    ,{<<"service_changes">>, Invoice} %% backward compatibility
    ,{<<"sub_account">>, Cascade} %% backward compatibility
    ,{<<"request">>, request_data(DataJObj)}
    ,{<<"system">>, teletype_util:system_params()}
    ,{<<"time_stamp">>, Timestamp} %% backward compatibility
    ,{<<"timestamp">>, Timestamp}
    ,{<<"user">>, agent_user_data(DataJObj)}
    ].

-spec timestamp(kz_json:object()) -> kz_term:proplist().
timestamp(DataJObj) ->
    TS = kz_json:get_integer_value(<<"timestamp">>, DataJObj),
    teletype_util:fix_timestamp(TS, DataJObj).

-spec reseller_info_data(kz_json:object()) -> kz_term:proplist().
reseller_info_data(DataJObj) ->
    AccountId = kz_json:get_ne_binary_value(<<"account_id">>, DataJObj),
    ResellerId = kz_services_reseller:get_id(AccountId),
    teletype_util:find_account_params(ResellerId).

-spec affected_account_data(kz_json:object()) -> kz_term:proplist().
affected_account_data(DataJObj) ->
    AccountId = kz_json:get_ne_binary_value(<<"account_id">>, DataJObj),
    teletype_util:find_account_params(AccountId).

-spec authentication_data(kz_json:object()) -> kz_term:proplist().
authentication_data(DataJObj) ->
    kz_json:to_proplist(kz_json:get_json_value([<<"audit_log">>, <<"audit">>, <<"authentication">>], DataJObj, kz_json:new())).

-spec cascade_account_data(kz_json:object(), kz_term:api_proplist()) -> kz_term:proplist().
cascade_account_data(DataJObj, Affected) ->
    AffectedId = props:get_ne_binary_value(<<"id">>, Affected),
    AccountId = kz_json:get_ne_binary_value([<<"audit_log">>, <<"audit">>, <<"changes">>, <<"account_id">>], DataJObj),
    case AffectedId =:= AccountId of
        'true' -> 'undefined';
        'false' ->
            teletype_util:find_account_params(AccountId)
    end.

-spec agent_user_data(kz_json:object()) -> kz_term:proplist().
agent_user_data(DataJObj) ->
    AgentJObj = kz_json:get_json_value([<<"audit_log">>, <<"audit">>, <<"agent">>], DataJObj, kz_json:new()),
    AccountId = kz_json:get_ne_binary_value(<<"account_id">>, AgentJObj),
    UserId = kz_json:get_ne_binary_value(<<"type_id">>, AgentJObj),
    case kzd_users:fetch(AccountId, UserId) of
        {'ok', UserJObj} -> teletype_util:user_params(UserJObj);
        {'error', _} -> []
    end.

-spec request_data(kz_json:object()) -> kz_term:proplist().
request_data(DataJObj) ->
    kz_json:to_proplist(kz_json:get_json_value([<<"audit_log">>, <<"audit">>, <<"request">>], DataJObj, kz_json:new())).

-spec invoice_data(kz_json:object()) -> kz_term:proplist().
invoice_data(DataJObj) ->
    case kz_json:get_list_value(<<"items">>, DataJObj) of
        'undefined' -> [];
        [] -> [];
        Items0 ->
            {TotalDiscounts, Total, Items} = invoice_items_fold(Items0),
            [{<<"items">>, Items}
            ,{<<"total_discounts">>, format_price(TotalDiscounts)}
            ,{<<"total">>, format_price(Total)}
            ]
    end.

-spec invoice_items_fold(kz_json:objects()) -> {float(), float(), kz_term:proplist()}.
invoice_items_fold(Items0) ->
    {TotalDiscounts, Total, Items1} = lists:foldl(fun invoice_items_fold/2 , {0.0, 0.0, []}, Items0),
    {TotalDiscounts, Total, lists:keysort(1, Items1)}.

-spec invoice_items_fold(kz_json:object(), {float(), float(), kz_term:proplist()}) -> {float(), float(), kz_term:proplist()}.
invoice_items_fold(ItemJObj, {TotalDiscounts, TotalAcc, CategoryAcc}) ->
    Discounts = kz_json:get_float_value([<<"discounts">>, <<"total">>], ItemJObj, 0.0),
    Item = all_name(kz_json:get_ne_binary_value(<<"item">>, ItemJObj)),
    Category = all_name(kz_json:get_ne_binary_value(<<"category">>, ItemJObj)),
    Quantity = kz_json:get_integer_value(<<"quantity">>, ItemJObj, 0),
    Name = kz_binary:ucfirst(kz_json:get_ne_binary_value(<<"name">>, ItemJObj, <<Category/binary, "/", Item/binary>>)),
    Total = kz_json:get_float_value(<<"total">>, ItemJObj),
    ItemProps = [{<<"category">>, Category}
                ,{<<"discounts">>, format_price(Discounts)}
                ,{<<"item">>, Item}
                ,{<<"quantity">>, Quantity}
                ,{<<"rate">>, format_price(kz_json:get_float_value(<<"rate">>, ItemJObj))}
                ,{<<"total">>, format_price(Total)}
                ],
    Changes = changes_props(ItemJObj, kz_json:get_ne_binary_value([<<"changes">>, <<"type">>], ItemJObj), Quantity),
    case Quantity =:= 0
        andalso Changes =:= []
    of
        'true' -> {TotalDiscounts, TotalAcc, CategoryAcc};
        'false' ->
            {TotalDiscounts + Discounts, Total + TotalAcc, [{Name, props:filter_undefined(ItemProps ++ Changes)} | CategoryAcc]}
    end.

-spec all_name(kz_term:api_ne_binary()) -> kz_term:ne_binary().
all_name('undefined') ->
    %% it shouldn't be here
    <<"All">>;
all_name(<<"_all">>) ->
    <<"All">>;
all_name(<<"All/All">>) ->
    <<"All">>;
all_name(Name) ->
    kz_binary:ucfirst(Name).

-spec changes_props(kz_json:object(), kz_term:ne_binary(), non_neg_integer()) -> kz_term:proplist().
changes_props(_, <<"created">>, Quantity) ->
    [{<<"changes">>, [{<<"type">>, <<"plan_new">>}
                     ,{<<"quantity">>, Quantity}
                     ]
     }
    ];
changes_props(ItemJObj, <<"modified">>, _) ->
    Changes = kz_json:get_json_value(<<"changes">>, ItemJObj),
    DiffQuantity = kz_json:get_integer_value([<<"difference">>, <<"quantity">>], Changes, 0),
    Quantity = erlang:abs(DiffQuantity),
    case DiffQuantity > 0 of
        'true' ->
            [{<<"changes">>, [{<<"type">>, <<"added">>}
                             ,{<<"quantity">>, Quantity}
                             ]
             }
            ];
        'false' ->
            [{<<"changes">>, [{<<"type">>, <<"removed">>}
                             ,{<<"quantity">>, Quantity}
                             ]
             }
            ]
    end;
changes_props(ItemJObj, <<"removed">>, _) ->
    Changes = kz_json:get_json_value(<<"changes">>, ItemJObj),
    DiffQuantity = kz_json:get_integer_value([<<"difference">>, <<"quantity">>], Changes),
    Quantity = erlang:abs(DiffQuantity),
    [{<<"changes">>, [{<<"type">>, <<"plan_removed">>}
                     ,{<<"quantity">>, Quantity}
                     ]
     }
    ];
changes_props(_, _, _) ->
    [].

-spec format_price(number()) -> kz_term:ne_binary().
format_price(Price) when is_integer(Price) ->
    kz_term:to_binary(io_lib:format("~b", [Price]));
format_price(Price) when is_float(Price) ->
    kz_term:to_binary(io_lib:format("~.2f", [Price])).
