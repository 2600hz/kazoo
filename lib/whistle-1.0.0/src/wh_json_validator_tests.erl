%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Reference Schemas and Objects to validate against
%%%
%%% See http://json-schema.org/shared.html
%%%
%%% @end
%%% Created : 25 Nov 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(wh_json_validator_tests).

-export([]).

-include_lib("eunit/include/eunit.hrl").

card_test() ->
    CardSchema = wh_json:decode(<<"{ \"description\":\"A representation of a person, company, organization, or place\", \"type\":\"object\", \"properties\":{ \"fn\":{ \"description\":\"Formatted Name\", \"type\":\"string\" }, \"familyName\":{ \"type\":\"string\", \"required\":true }, \"givenName\":{ \"type\":\"string\", \"required\":true }, \"additionalName\":{ \"type\":\"array\", \"items\":{ \"type\":\"string\" } }, \"honorificPrefix\":{ \"type\":\"array\", \"items\":{ \"type\":\"string\" } }, \"honorificSuffix\":{ \"type\":\"array\", \"items\":{ \"type\":\"string\" } }, \"nickname\":{ \"type\":\"string\" }, \"url\":{ \"type\":\"string\", \"format\":\"url\" }, \"email\":{ \"type\":\"object\", \"properties\":{ \"type\":{ \"type\":\"string\" }, \"value\":{ \"type\":\"string\", \"format\":\"email\" } } }, \"tel\":{ \"type\":\"object\", \"properties\":{ \"type\":{ \"type\":\"string\" }, \"value\":{ \"type\":\"string\", \"format\":\"phone\" } } }, \"adr\":{\"$ref\" : \"http://json-schema.org/address\"}, \"geo\":{\"$ref\" : \"http://json-schema.org/geo\"}, \"tz\":{ \"type\":\"string\" }, \"photo\":{ \"format\":\"image\", \"type\":\"string\" }, \"logo\":{ \"format\":\"image\", \"type\":\"string\" }, \"sound\":{ \"format\":\"attachment\", \"type\":\"string\" }, \"bday\":{ \"type\":\"string\", \"format\":\"date\" }, \"title\":{ \"type\":\"string\" }, \"role\":{ \"type\":\"string\" }, \"org\":{ \"type\":\"object\", \"properties\":{ \"organizationName\":{ \"type\":\"string\" }, \"organizationUnit\":{ \"type\":\"string\" } } } }}">>),
    Failure = wh_json:to_proplist(wh_json:decode(<<"{ \"givenName\": { \"required\": \"Field is required but missing\" }, \"familyName\": { \"required\": \"Field is required but missing\" }}">>)),
    CardJObj = wh_json:from_list([]),    
    ?assertEqual({fail, Failure}, wh_json_validator:is_valid(CardJObj, CardSchema)).

address_test() ->
    AddressSchema = wh_json:decode(<<"{ \"description\" : \"An Address following the convention of http://microformats.org/wiki/hcard\", \"type\" : \"object\", \"properties\" : { \"post-office-box\" : { \"type\" : \"string\" }, \"extended-address\" : { \"type\" : \"string\" }, \"street-address\" : { \"type\":\"string\" }, \"locality\" : { \"type\" : \"string\", \"required\" : true }, \"region\" : { \"type\" : \"string\", \"required\" : true }, \"postal-code\" : { \"type\" : \"string\" }, \"country-name\" : { \"type\" : \"string\", \"required\" : true } }, \"dependencies\" : { \"post-office-box\" : \"street-address\", \"extended-address\" : \"street-address\", \"street-address\" : \"region\", \"locality\" : \"region\", \"region\" : \"country-name\" }}">>),
    Failure = wh_json:to_proplist(wh_json:decode(<<"{ \"country-name\": { \"required\": \"Field is required but missing\" }, \"region\": { \"required\": \"Field is required but missing\" }, \"locality\": { \"required\": \"Field is required but missing\" }}">>)),
    AddressJObj = wh_json:from_list([]),
    ?assertEqual({fail, Failure}, wh_json_validator:is_valid(AddressJObj, AddressSchema)).

geo_test() ->
    GeoSchema = wh_json:decode(<<"{ \"description\" : \"A geographical coordinate\", \"type\" : \"object\", \"properties\" : { \"latitude\" : { \"type\" : \"number\" }, \"longitude\" : { \"type\" : \"number\" } }}">>),
    GeoJObj = wh_json:from_list([]),
    ?assertEqual({pass, GeoJObj}, wh_json_validator:is_valid(GeoJObj, GeoSchema)).
