%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%------------------------------------------------------------------
-module(lineman_workorder).

-export([empty/0]).
-export([read/1]).
-export([toolbag/1]).
-export([sequences/1]).

-include_lib("lineman/src/lineman.hrl").

-record(workorder, {workorder
                    ,file
                    ,name = <<"Unknown">>
                    ,max_running_sequences = 10
                    ,deadcall_wait = 5000
                    ,sequence_order = simultanous
                    ,toolbag = []
                    ,sequences = []
                   }).

-opaque workorder() :: #workorder{}.
-export_type([workorder/0]).

-spec empty/0 :: () -> workorder().
empty() ->
    #workorder{}.

-spec read/1 :: (string()) -> {'ok', xml()} | {'error', atom()}.
read(File) ->
    try xmerl_scan:file(File) of
        {error, _}=E -> E;
        {Xml, []} -> 
            {ok, populate_workorder(#workorder{file=File}, Xml)};
        {_, _} -> {error, non_xml_content}
    catch
        _:_ -> {error, parse_failed}
    end.

-spec toolbag/1 :: (workorder()) -> xml().
toolbag(#workorder{toolbag=Toolbag}) ->
    Toolbag.

-spec sequences/1 :: (workorder()) -> xml().
sequences(#workorder{sequences=Sequences}) ->
    Sequences.

-spec populate_workorder/2 :: (workorder(), xml()) -> workorder().
populate_workorder(#workorder{file=File}=Workorder, Xml) ->
    Workorder#workorder{workorder=Xml
                        ,name = lineman_util:xml_binary_value("/workorder/parameters/name", Xml, File)
                        ,max_running_sequences = lineman_util:xml_binary_value("/workorder/parameters/max-running-sequences", Xml, 10)
                        ,deadcall_wait = lineman_util:xml_binary_value("/workorder/parameters/deadcall-wait", Xml, 5000)
                        ,file = File
                        ,toolbag = xmerl_xpath:string("/workorder/toolbag/*", Xml)
                        ,sequences = xmerl_xpath:string("/workorder/sequences/*", Xml)
                       }.
