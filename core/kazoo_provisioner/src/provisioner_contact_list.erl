%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Common functions for the provisioner modules
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(provisioner_contact_list).

-export([build/1]).

-record(contact, {id :: kz_term:api_ne_binary()
                 ,callflow :: kz_term:api_ne_binary()
                 ,name :: kz_term:api_ne_binary()
                 ,first_module :: kz_term:api_ne_binary()
                 ,external_numbers = [] :: kz_term:ne_binaries()
                 ,internal_numbers = [] :: kz_term:ne_binaries()
                 }).

-spec build(kz_term:ne_binary()) -> kz_json:objects().
build(AccountId) ->
    [contact_to_json(Contact)
     || Contact <- build_contacts(AccountId)
    ].

contact_to_json(#contact{name=Name
                        ,external_numbers=ExternalNumbers
                        ,internal_numbers=InternalNumbers
                        }) ->
    kz_json:from_list([{<<"name">>, Name}
                      ,{<<"external_number">>, first_number(ExternalNumbers)}
                      ,{<<"internal_number">>, first_number(InternalNumbers)}
                      ]).

first_number([Number|_]) -> Number;
first_number(_) -> 'undefined'.

build_contacts(AccountId) ->
    Routines = [fun filter_excluded/2
               ,fun find_missing_names/2
               ,fun maybe_append_module/2
               ,fun filter_contacts/2
               ],
    lists:foldl(fun(F, C) -> F(C, AccountId) end
               ,get_extension_contacts(AccountId)
               ,Routines
               ).

get_extension_contacts(AccountId) ->
    ViewOptions = [],
    case kz_datamgr:get_results(AccountId, <<"contact_list/extensions">>, ViewOptions) of
        {'error', _} -> [];
        {'ok', JObjs} ->
            Includes = get_contact_list_includes(AccountId),
            lists:foldr(fun(JObj, Contacts) ->
                                Key = kz_json:get_value(<<"key">>, JObj),
                                case kz_term:is_empty(Includes)
                                    orelse lists:member(Key, Includes)
                                of
                                    'false' -> Contacts;
                                    'true' ->
                                        Extension = kz_json:get_value(<<"value">>, JObj),
                                        [jobj_to_contact(Extension)|Contacts]
                                end
                        end
                       ,[]
                       ,JObjs
                       )
    end.

get_contact_list_includes(AccountId) ->
    Default = kapps_config:get_jsons(<<"crossbar.contact_list">>, <<"default_includes">>, []),
    case kzd_accounts:fetch(AccountId) of
        {'ok', JObj} ->
            kz_json:get_value([<<"contact_list">>, <<"includes">>], JObj, Default);
        {'error', _} ->
            Default
    end.

filter_excluded(Contacts, AccountId) ->
    ViewOptions = [],
    case kz_datamgr:get_results(AccountId, <<"contact_list/excluded">>, ViewOptions) of
        {'error', _} -> Contacts;
        {'ok', JObjs} ->
            Ids = [kz_doc:id(JObj) || JObj <- JObjs],
            lists:filter(fun(#contact{id=Id}) -> (not lists:member(Id, Ids)) end, Contacts)
    end.

find_missing_names(Contacts, AccountId) ->
    ViewOptions = [],
    case kz_datamgr:get_results(AccountId, <<"contact_list/names">>, ViewOptions) of
        {'error', _} -> Contacts;
        {'ok', JObjs} -> merge_results(JObjs, Contacts)
    end.

maybe_append_module(Contacts, _) ->
    lists:map(fun(#contact{first_module='undefined'}=Contact) -> Contact;
                 (#contact{name='undefined'}=Contact) -> Contact;
                 (#contact{first_module=Module, name=Name}=Contact) ->
                      Contact#contact{name = <<Name/binary, " (",  Module/binary, ")">>}
              end
             ,Contacts
             ).

filter_contacts(Contacts, _) ->
    [C || C <- Contacts, filter_contacts(C)].

filter_contacts(#contact{external_numbers = [], internal_numbers = []}) -> 'false';
filter_contacts(#contact{name = 'undefined'}) -> 'false';
filter_contacts(_) -> 'true'.

jobj_to_contact(JObj) ->
    jobj_to_contact(JObj, #contact{}).

jobj_to_contact(JObj, Contact) ->
    J = maybe_fix_numbers(JObj),
    Fields = record_info('fields', 'contact'),
    maybe_update_contact(Fields, 'undefined', J, Contact).

merge_results([], Contacts) -> Contacts;
merge_results([JObj|JObjs], Contacts) ->
    Contact = kz_json:get_value(<<"value">>, JObj),
    Id = kz_doc:id(Contact),
    merge_results(JObjs, maybe_update_contacts(Id, Contact, Contacts)).

maybe_update_contacts(Id, JObj, Contacts) ->
    Fields = record_info('fields', 'contact'),
    lists:foldr(fun(Contact, C) ->
                        [maybe_update_contact(Fields, Id, JObj, Contact)|C]
                end
               ,[]
               ,Contacts
               ).

maybe_update_contact([Field|Fields], 'undefined', JObj, Contact) ->
    C = maybe_update_contact(Field, JObj, Contact),
    maybe_update_contact(Fields, 'undefined', JObj, C);
maybe_update_contact([Field|Fields], Id, JObj, #contact{id=Id}=Contact) ->
    C = maybe_update_contact(Field, JObj, Contact),
    maybe_update_contact(Fields, Id, JObj, C);
maybe_update_contact(_, _, _, Contact) -> Contact.

maybe_update_contact('id', JObj, #contact{id='undefined'}=Contact) ->
    Contact#contact{id=kz_doc:id(JObj)};
maybe_update_contact('callflow', JObj, #contact{callflow='undefined'}=Contact) ->
    Contact#contact{callflow=kz_json:get_ne_value(<<"callflow">>, JObj)};
maybe_update_contact('name', JObj, #contact{name='undefined'}=Contact) ->
    Contact#contact{name=kz_json:get_ne_value(<<"name">>, JObj)};
maybe_update_contact('first_module', JObj, #contact{first_module='undefined'}=Contact) ->
    Contact#contact{first_module = kz_json:get_ne_value(<<"first_module">>, JObj)};
maybe_update_contact('external_numbers', JObj, #contact{external_numbers=[]}=Contact) ->
    Contact#contact{external_numbers=kz_json:get_ne_value(<<"external_numbers">>, JObj, [])};
maybe_update_contact('internal_numbers', JObj, #contact{internal_numbers=[]}=Contact) ->
    Contact#contact{internal_numbers=kz_json:get_ne_value(<<"internal_numbers">>, JObj, [])};
maybe_update_contact(_, _, Contact) -> Contact.

maybe_fix_numbers(JObj) ->
    case kz_json:get_ne_value(<<"numbers">>, JObj) of
        'undefined' -> JObj;
        Numbers ->
            {External, Internal} = split_contact_numbers(Numbers),
            Props = [{<<"external_numbers">>, External}
                    ,{<<"internal_numbers">>, Internal}
                    ],
            kz_json:set_values(Props, kz_json:delete_key(<<"numbers">>, JObj))
    end.

split_contact_numbers(Numbers) ->
    split_contact_numbers({[], []}, Numbers).

split_contact_numbers(Results, []) ->
    Results;
split_contact_numbers({External, Internal}, [Number|Numbers]) ->
    case knm_converters:is_reconcilable(Number) of
        'true' ->
            split_contact_numbers({[Number|External], Internal}, Numbers);
        'false' ->
            split_contact_numbers({External, [Number|Internal]}, Numbers)
    end.
