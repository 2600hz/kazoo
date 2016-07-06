%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Instructs the switch to receive a fax from the caller
%%% Stores the fax in the database and optionally emails a configured
%%% user(s).
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Ben Wann
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(cf_faxbox).

-include("callflow.hrl").

-export([handle/2]).

-define(FAX_OPTION, [<<"media">>, <<"fax_option">>]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    FaxboxId = get_faxbox_id(Data),
    lager:info("receive fax for faxbox: ~s", [FaxboxId]),
    Props = props:filter_undefined(
              props:filter_empty([{<<"Call">>, kapps_call:to_json(Call)}
                                 ,{<<"Action">>, <<"receive">>}
                                 ,{<<"FaxBox-ID">>, FaxboxId}
                                 ,{<<"Fax-T38-Option">>, lookup_fax_option(Call, Data)}
                                  | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                 ])),
    kapi_fax:publish_req(Props),
    cf_exe:control_usurped(Call).

-spec lookup_fax_option(kapps_call:call(), kz_json:object()) -> ne_binary().
lookup_fax_option(Call, Data) ->
    FaxBoxId = get_faxbox_id(Data),
    DefaultFaxBoxOption = case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), FaxBoxId) of
                              {'ok', JObj} -> kz_json:get_value(?FAX_OPTION, JObj);
                              _ -> 'undefined'
                          end,
    kz_json:get_value(?FAX_OPTION, Data, DefaultFaxBoxOption).

-spec get_faxbox_id(kz_json:object()) -> api_binary().
get_faxbox_id(JObj) ->
    kz_json:get_first_defined([<<"id">>, <<"faxbox_id">>], JObj).
