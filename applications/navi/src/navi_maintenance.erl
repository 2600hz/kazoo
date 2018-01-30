%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Voyager Internet Ltd.
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Ben Partridge
%%%-------------------------------------------------------------------
-module(navi_maintenance).

-include("navi.hrl").

-export([add_apns_app/5, add_fcm_app/2]).

-spec add_apns_app(ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> {'ok', kz_json:object()} | {{'error', any()}, ne_binary()}.
add_apns_app(Name, Topic, Environment, CertificateFile, KeyFile) ->
    case {file:read_file(CertificateFile), file:read_file(KeyFile)} of
        {{'ok', CertificateBin}, {'ok', KeyBin}} ->
            NewApp = kz_json:from_list([{<<"app_name">>, Name}
                                       ,{<<"certificate">>, CertificateBin}
                                       ,{<<"default_topic">>, Topic}
                                       ,{<<"environment">>, Environment}
                                       ,{<<"key">>, KeyBin}
                                       ,{<<"notification_type">>, <<"apns">>}
                                       ]),
            validate_and_set_app(NewApp);
        {{'error', _} = Error, _} -> {Error, <<"Error reading certificate file">>};
        {_, {'error', _} = Error} -> {Error, <<"Error reading key file">>}
    end.

-spec add_fcm_app(ne_binary(), ne_binary()) -> {'ok', kz_json:object()} | {{'error', any()}, ne_binary()}.
add_fcm_app(Name, ApiKey) ->
    NewApp = kz_json:from_list([{<<"app_name">>, Name}
                               ,{<<"api_key">>, ApiKey}
                               ,{<<"notification_type">>, <<"fcm">>}
                               ]),
    validate_and_set_app(NewApp).

-spec validate_and_set_app(kz_json:object()) -> {'ok', kz_json:object()} | {{'error', any()}, ne_binary()}.
validate_and_set_app(NewApp) ->
    {'ok', Config} = kapps_config:get_category(?CONFIG_CAT),
    Apps = kz_json:get_value(<<"notification_servers">>, kz_json:get_value(<<"default">>, Config)),
    NewConfig = kz_json:set_value(<<"notification_servers">>, [NewApp|Apps], Config),
    case kz_json_schema:validate(<<"system_config.navi">>, NewConfig) of
        {'ok', _} ->
            lager:error("Did validate"),
            kapps_config:set(?CONFIG_CAT, <<"notification_servers">>, [NewApp|Apps]);
        Error ->
            lager:error("Did not validate"),
            {{'error', Error}, <<"Error validating app">>}
    end.
