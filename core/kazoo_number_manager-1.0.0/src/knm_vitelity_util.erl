%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz INC
%%% @doc
%%%
%%%
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(knm_vitelity_util).

-export([api_uri/0
         ,config_cat/0
         ,add_options_fold/2
         ,get_query_value/2
         ,default_options/0, default_options/1
         ,build_uri/1
         ,query_vitelity/1
         ,get_short_state/1

         ,xml_resp_status_msg/1
         ,xml_resp_error_msg/1
         ,xml_resp_response_msg/1
         ,xml_resp_numbers/1
         ,xml_resp_info/1
         ,xml_resp_response/1
         ,xml_els_to_proplist/1
        ]).

-include("knm.hrl").
-include("knm_vitelity.hrl").

-export_type([query_options/0]).

-spec api_uri() -> ne_binary().
api_uri() ->
    ?VITELITY_URI.

-spec config_cat() -> ne_binary().
config_cat() ->
    ?KNM_VITELITY_CONFIG_CAT.

-spec add_options_fold({atom(), api_binary()}, query_options()) ->
                              query_options().
add_options_fold({_K, 'undefined'}, Options) -> Options;
add_options_fold({K, V}, Options) ->
    props:insert_value(K, V, Options).

-ifdef(TEST).
get_query_value(Key, Options) ->
    props:get_value(Key, Options).
-else.
-spec get_query_value(atom() | ne_binary(), query_options()) -> term().
get_query_value(Key, Options) ->
    case props:get_value(Key, Options) of
        'undefined' ->
            whapps_config:get(?KNM_VITELITY_CONFIG_CAT, Key);
        Value -> Value
    end.
-endif.

-spec default_options() -> qs_options().
-spec default_options(wh_proplist()) -> qs_options().
default_options() ->
     default_options([]).
default_options(Options) ->
    [{'login', get_query_value(<<"login">>, Options)}
     ,{'pass', get_query_value(<<"pass">>, Options)}
    ].

-spec build_uri(query_options()) -> ne_binary().
build_uri(Options) ->
    URI = props:get_value('uri', Options),
    QS = wh_util:to_binary(
           props:to_querystring(
             props:filter_undefined(
               props:get_value('qs', Options)
              ))),
    <<URI/binary, "?", QS/binary>>.

-spec xml_resp_status_msg(xml_els()) -> api_binary().
xml_resp_status_msg(XmlEls) ->
    xml_el_to_binary(xml_resp_tag(XmlEls, 'status')).

-spec xml_resp_error_msg(xml_els()) -> api_binary().
xml_resp_error_msg(XmlEls) ->
    xml_el_to_binary(xml_resp_tag(XmlEls, 'error')).

-spec xml_resp_response_msg(xml_els()) -> api_binary().
xml_resp_response_msg(XmlEls) ->
    xml_el_to_binary(xml_resp_tag(XmlEls, 'response')).

-spec xml_resp_numbers(xml_els()) -> xml_el() | 'undefined'.
xml_resp_numbers(XmlEls) ->
    xml_resp_tag(XmlEls, 'numbers').

-spec xml_resp_info(xml_els()) -> xml_el() | 'undefined'.
xml_resp_info(XmlEls) ->
    xml_resp_tag(XmlEls, 'info').

-spec xml_resp_response(xml_els()) -> xml_el() | 'undefined'.
xml_resp_response(XmlEls) ->
    xml_resp_tag(XmlEls, 'response').

-spec xml_resp_tag(xml_els(), atom()) -> xml_el() | 'undefined'.
xml_resp_tag([#xmlElement{name=Name}=El|_], Name) -> El;
xml_resp_tag([_|Els], Name) ->
    xml_resp_tag(Els, Name);
xml_resp_tag([], _Name) ->
    'undefined'.

-spec xml_el_to_binary('undefined' | xml_el()) -> api_binary().
xml_el_to_binary('undefined') -> 'undefined';
xml_el_to_binary(#xmlElement{content=Content}) ->
    kz_xml:texts_to_binary(Content).

-spec xml_els_to_proplist(xml_els()) -> wh_proplist().
xml_els_to_proplist(Els) ->
    [KV || El <- Els,
           begin
               {_, V}=KV = xml_el_to_kv_pair(El),
               V =/= 'undefined'
           end
    ].

-spec xml_el_to_kv_pair(xml_el()) -> {ne_binary(), api_binary() | wh_json:object()}.
xml_el_to_kv_pair(#xmlElement{name='did'
                              ,content=Value
                             }) ->
    %% due to inconsistency in listdids
    Num = kz_xml:texts_to_binary(Value),
    {<<"number">>
     ,knm_converters:normalize(Num)
    };
xml_el_to_kv_pair(#xmlElement{name='number'
                              ,content=Value
                             }) ->
    %% due to inconsistency in listdids
    Num = kz_xml:texts_to_binary(Value),
    {<<"number">>
     ,knm_converters:normalize(Num)
    };
xml_el_to_kv_pair(#xmlElement{name=Name
                              ,content=[]
                             }) ->
    {Name, 'undefined'};
xml_el_to_kv_pair(#xmlElement{name=Name
                              ,content=Value
                             }) ->
    case kz_xml:elements(Value) of
        [] ->
            {wh_util:to_binary(Name)
             ,kz_xml:texts_to_binary(Value)
            };
        Els ->
            {wh_util:to_binary(Name)
             ,wh_json:from_list(xml_els_to_proplist(Els))
            }
    end.

-spec query_vitelity(ne_binary()) ->
                            {'ok', text()} |
                            {'error', _}.
query_vitelity(URI) ->
    lager:debug("querying ~s", [URI]),
    case ibrowse:send_req(wh_util:to_list(URI), [], 'post') of
        {'ok', _RespCode, _RespHeaders, RespXML} ->
            lager:debug("recv ~s: ~s", [_RespCode, RespXML]),
            {'ok', RespXML};
        {'error', _R}=E ->
            lager:debug("error querying: ~p", [_R]),
            E
    end.

-spec get_short_state(ne_binary()) -> state_two_letters() | 'undefined'.
get_short_state(FullState) ->
    States = [{<<"alabama">>, <<"AL">>}
              ,{<<"alaska">>, <<"AK">>}
              ,{<<"american samoa">>, <<"AS">>}
              ,{<<"arizona">>, <<"AZ">>}
              ,{<<"arkansas">>, <<"AR">>}
              ,{<<"california">>, <<"CA">>}
              ,{<<"colorado">>, <<"CO">>}
              ,{<<"connecticut">>, <<"CT">>}
              ,{<<"delaware">>, <<"DE">>}
              ,{<<"district of columbia">>, <<"DC">>}
              ,{<<"federated states of micronesia">>, <<"FM">>}
              ,{<<"florida">>, <<"FL">>}
              ,{<<"georgia">>, <<"GA">>}
              ,{<<"guam">>, <<"GU">>}
              ,{<<"hawaii">>, <<"HI">>}
              ,{<<"idaho">>, <<"ID">>}
              ,{<<"illinois">>, <<"IL">>}
              ,{<<"indiana">>, <<"IN">>}
              ,{<<"iowa">>, <<"IA">>}
              ,{<<"kansas">>, <<"KS">>}
              ,{<<"kentucky">>, <<"KY">>}
              ,{<<"louisiana">>, <<"LA">>}
              ,{<<"maine">>, <<"ME">>}
              ,{<<"marshall islands">>, <<"MH">>}
              ,{<<"maryland">>, <<"MD">>}
              ,{<<"massachusetts">>, <<"MA">>}
              ,{<<"michigan">>, <<"MI">>}
              ,{<<"minnesota">>, <<"MN">>}
              ,{<<"mississippi">>, <<"MS">>}
              ,{<<"missouri">>, <<"MO">>}
              ,{<<"montana">>, <<"MT">>}
              ,{<<"nebraska">>, <<"NE">>}
              ,{<<"nevada">>, <<"NV">>}
              ,{<<"new hampshire">>, <<"NH">>}
              ,{<<"new jersey">>, <<"NJ">>}
              ,{<<"new mexico">>, <<"NM">>}
              ,{<<"new york">>, <<"NY">>}
              ,{<<"north carolina">>, <<"NC">>}
              ,{<<"north dakota">>, <<"ND">>}
              ,{<<"northern mariana islands">>, <<"MP">>}
              ,{<<"ohio">>, <<"OH">>}
              ,{<<"oklahoma">>, <<"OK">>}
              ,{<<"oregon">>, <<"OR">>}
              ,{<<"palau">>, <<"PW">>}
              ,{<<"pennsylvania">>, <<"PA">>}
              ,{<<"puerto rico">>, <<"PR">>}
              ,{<<"rhode island">>, <<"RI">>}
              ,{<<"south carolina">>, <<"SC">>}
              ,{<<"south dakota">>, <<"SD">>}
              ,{<<"tennessee">>, <<"TN">>}
              ,{<<"texas">>, <<"TX">>}
              ,{<<"utah">>, <<"UT">>}
              ,{<<"vermont">>, <<"VT">>}
              ,{<<"virgin islands">>, <<"VI">>}
              ,{<<"virginia">>, <<"VA">>}
              ,{<<"washington">>, <<"WA">>}
              ,{<<"west virginia">>, <<"WV">>}
              ,{<<"wisconsin">>, <<"WI">>}
              ,{<<"wyoming">>, <<"WY">>}
             ],
    State = wh_util:to_lower_binary(FullState),
    props:get_value(State, States).
