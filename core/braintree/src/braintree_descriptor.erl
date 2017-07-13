%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(braintree_descriptor).

-export([get_name/1]).
-export([xml_to_record/1, xml_to_record/2]).
-export([record_to_xml/1, record_to_xml/2]).
-export([record_to_json/1]).
-export([json_to_record/1]).

-include("bt.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_name(bt_descriptor()) -> api_ne_binary().
get_name(#bt_descriptor{name=Name}) ->
    Name.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Contert the given XML to a descriptor record
%% @end
%%--------------------------------------------------------------------
-spec xml_to_record(bt_xml()) -> bt_descriptor().
xml_to_record(Xml) ->
    xml_to_record(Xml, "/descriptor").

-spec xml_to_record(bt_xml(), kz_deeplist()) -> bt_descriptor().
xml_to_record(Xml, Base) ->
    #bt_descriptor{name = kz_xml:get_value([Base, "/name/text()"], Xml)
                  ,phone = kz_xml:get_value([Base, "/phone/text()"], Xml)
                  ,url = kz_xml:get_value([Base, "/url/text()"], Xml)
                  }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Contert the given XML to a descriptor record
%% @end
%%--------------------------------------------------------------------
-spec record_to_xml(bt_descriptor()) -> kz_proplist() | bt_xml().
record_to_xml(Descriptor) ->
    record_to_xml(Descriptor, false).

-spec record_to_xml(bt_descriptor(), boolean()) -> kz_proplist() | bt_xml().
record_to_xml('undefined', _ToString) -> 'undefined';
record_to_xml(Descriptor, ToString) ->
    Props = [{'name', Descriptor#bt_descriptor.name}
            ,{'phone', Descriptor#bt_descriptor.phone}
            ,{'url', Descriptor#bt_descriptor.url}
            ],
    case ToString of
        true -> braintree_util:make_doc_xml(Props, 'descriptor');
        false -> Props
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a given record into a json object
%% @end
%%--------------------------------------------------------------------
-spec record_to_json(bt_descriptor()) -> kz_json:object().
record_to_json(#bt_descriptor{name=Name, phone=Phone, url=Url}) ->
    kz_json:from_list(
      [{<<"name">>, Name}
      ,{<<"phone">>, Phone}
      ,{<<"url">>, Url}
      ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a given json obj into a record
%% @end
%%--------------------------------------------------------------------
-spec json_to_record(api_object()) -> bt_descriptor() | 'undefined'.
json_to_record('undefined') -> 'undefined';
json_to_record(JObj) ->
    #bt_descriptor{name = kz_json:get_binary_value(<<"name">>, JObj)
                  ,phone = kz_json:get_binary_value(<<"phone">>, JObj)
                  ,url = kz_json:get_value(<<"url">>, JObj)
                  }.
