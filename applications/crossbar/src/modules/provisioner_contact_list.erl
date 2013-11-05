%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% Common functions for the provisioner modules
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(provisioner_contact_list).

-export([build/1]).

-include("../crossbar.hrl").

-record(contact, {id
                  ,callflow
                  ,name
                  ,first_module
                  ,external_numbers = []
                  ,internal_numbers = []
                 }).

build(AccountDb) ->
    [contact_to_json(Contact)
     || Contact <- build_contacts(AccountDb)
    ].

contact_to_json(#contact{name=Name
                         ,external_numbers=ExternalNumbers
                         ,internal_numbers=InternalNumbers}) ->
    Props = [{<<"name">>, Name}
             ,{<<"external_number">>, first_number(ExternalNumbers)}
             ,{<<"internal_number">>, first_number(InternalNumbers)}
            ],
    wh_json:from_list(props:filter_undefined(Props)).

first_number([Number|_]) -> Number;
first_number(_) -> undefined.

build_contacts(AccountDb) ->
    Routines = [fun filter_excluded/2
                ,fun find_missing_names/2
                ,fun maybe_append_module/2
                ,fun filter_contacts/2
               ],
    lists:foldl(fun(F, C) -> F(C, AccountDb) end
                ,get_extension_contacts(AccountDb)
                ,Routines).

get_extension_contacts(AccountDb) ->
    ViewOptions = [],
    case couch_mgr:get_results(AccountDb, <<"contact_list/extensions">>, ViewOptions) of
        {error, _} -> [];
        {ok, JObjs} ->
            Includes = get_contact_list_includes(AccountDb),
            lists:foldr(fun(JObj, Contacts) ->
                                Key = wh_json:get_value(<<"key">>, JObj),
                                case wh_util:is_empty(Includes)
                                    orelse lists:member(Key, Includes)
                                of
                                    'false' -> Contacts;
                                    'true' ->
                                        Extension = wh_json:get_value(<<"value">>, JObj),
                                        [jobj_to_contact(Extension)|Contacts]
                                end
                        end, [], JObjs)
    end.

get_contact_list_includes(AccountDb) ->
    Default = whapps_config:get(<<"crossbar.contact_list">>, <<"default_includes">>, []),
    AccountId = wh_util:format_account_id(AccountDb, raw),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'ok', JObj} ->
            wh_json:get_value([<<"contact_list">>, <<"includes">>], JObj, Default);
        {'error', _} ->
            Default
    end.

filter_excluded(Contacts, AccountDb) ->
    ViewOptions = [],
    case couch_mgr:get_results(AccountDb, <<"contact_list/excluded">>, ViewOptions) of
        {error, _} -> Contacts;
        {ok, JObjs} ->
            Ids = [wh_json:get_value(<<"id">>, JObj) || JObj <- JObjs],
            lists:filter(fun(#contact{id=Id}) -> (not lists:member(Id, Ids)) end, Contacts)
    end.

find_missing_names(Contacts, AccountDb) ->
    ViewOptions = [],
    case couch_mgr:get_results(AccountDb, <<"contact_list/names">>, ViewOptions) of
        {error, _} -> Contacts;
        {ok, JObjs} -> merge_results(JObjs, Contacts)
    end.

maybe_append_module(Contacts, _) ->
    lists:map(fun(#contact{first_module=undefined}=Contact) -> Contact;
                 (#contact{name=undefined}=Contact) -> Contact;
                 (#contact{first_module=Module, name=Name}=Contact) ->
                      Contact#contact{name = <<Name/binary, " (",  Module/binary, ")">>}
              end, Contacts).

filter_contacts(Contacts, _) ->
    lists:filter(fun filter_contacts/1, Contacts).

filter_contacts(#contact{external_numbers = [], internal_numbers = []}) -> false;
filter_contacts(#contact{name = undefined}) -> false;
filter_contacts(_) -> true.

jobj_to_contact(JObj) ->
    jobj_to_contact(JObj, #contact{}).

jobj_to_contact(JObj, Contact) ->
    J = maybe_fix_numbers(JObj),
    Fields = record_info(fields, contact),
    maybe_update_contact(Fields, undefined, J, Contact).

merge_results([], Contacts) -> Contacts;
merge_results([JObj|JObjs], Contacts) ->
    Contact = wh_json:get_value(<<"value">>, JObj),
    Id = wh_json:get_value(<<"id">>, Contact),
    merge_results(JObjs, maybe_update_contacts(Id, Contact, Contacts)).

maybe_update_contacts(Id, JObj, Contacts) ->
    Fields = record_info(fields, contact),
    lists:foldr(fun(Contact, C) ->
                        [maybe_update_contact(Fields, Id, JObj, Contact)|C]
                end, [], Contacts).

maybe_update_contact([Field|Fields], undefined, JObj, Contact) ->
    C = maybe_update_contact(Field, JObj, Contact),
    maybe_update_contact(Fields, undefined, JObj, C);
maybe_update_contact([Field|Fields], Id, JObj, #contact{id=Id}=Contact) ->
    C = maybe_update_contact(Field, JObj, Contact),
    maybe_update_contact(Fields, Id, JObj, C);
maybe_update_contact(_, _, _, Contact) -> Contact.

maybe_update_contact(id, JObj, #contact{id=undefined}=Contact) ->
    Contact#contact{id=wh_json:get_ne_value(<<"id">>, JObj)};
maybe_update_contact(callflow, JObj, #contact{callflow=undefined}=Contact) ->
    Contact#contact{callflow=wh_json:get_ne_value(<<"callflow">>, JObj)};
maybe_update_contact(name, JObj, #contact{name=undefined}=Contact) ->
    Contact#contact{name=wh_json:get_ne_value(<<"name">>, JObj)};
maybe_update_contact(first_module, JObj, #contact{first_module=undefined}=Contact) ->
    Contact#contact{first_module = wh_json:get_ne_value(<<"first_module">>, JObj)};
maybe_update_contact(external_numbers, JObj, #contact{external_numbers=[]}=Contact) ->
    Contact#contact{external_numbers=wh_json:get_ne_value(<<"external_numbers">>, JObj, [])};
maybe_update_contact(internal_numbers, JObj, #contact{internal_numbers=[]}=Contact) ->
    Contact#contact{internal_numbers=wh_json:get_ne_value(<<"internal_numbers">>, JObj, [])};
maybe_update_contact(_, _, Contact) -> Contact.

maybe_fix_numbers(JObj) ->
    case wh_json:get_ne_value(<<"numbers">>, JObj) of
        undefined -> JObj;
        Numbers ->
            {External, Internal} = split_contact_numbers(Numbers),
            Props = [{<<"external_numbers">>, External}
                     ,{<<"internal_numbers">>, Internal}
                    ],
            wh_json:set_values(Props, wh_json:delete_key(<<"numbers">>, JObj))
    end.

split_contact_numbers(Numbers) ->
    split_contact_numbers({[], []}, Numbers).

split_contact_numbers(Results, []) ->
    Results;
split_contact_numbers({External, Internal}, [Number|Numbers]) ->
    case wnm_util:is_reconcilable(Number) of
        true ->
            split_contact_numbers({[Number|External], Internal}, Numbers);
        false ->
            split_contact_numbers({External, [Number|Internal]}, Numbers)
    end.
