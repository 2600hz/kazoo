%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license. 
%%% See the NOTICE for more information.

-module(couchbeam_changes).

-include("couchbeam.hrl").
-export([wait_for_change/1, continuous_acceptor/2]).

-export([decode_row/1]).
-record(state, {
    partial_chunk = <<"">>
}).

wait_for_change(Reqid) ->
    wait_for_change(Reqid, 200, []).

wait_for_change(Reqid, ReqStatus, Acc) ->
    receive
        {ibrowse_async_response_end, Reqid} ->
            Change = iolist_to_binary(lists:reverse(Acc)),
            try
                if ReqStatus >= 400 ->
                    {error, {ReqStatus, couchbeam_util:json_decode(Change)}};
                true ->
                    {ok, couchbeam_util:json_decode(Change)}
                end
            catch
            throw:{invalid_json, Error} ->
                {error, Error}
            end;
        {ibrowse_async_response, Reqid, {error,Error}} ->
            {error, Error};
        {ibrowse_async_response, Reqid, Chunk} ->
            ibrowse:stream_next(Reqid),
            wait_for_change(Reqid, ReqStatus, [Chunk|Acc]);
        {ibrowse_async_headers, Reqid, Status, _Headers} ->

            ibrowse:stream_next(Reqid), 
            wait_for_change(Reqid, list_to_integer(Status), Acc) 
    end.
    


%% @doc initiate continuous loop 
continuous_acceptor(Pid, PidRef) ->
    receive
        {ibrowse_req_id, PidRef, IbrowseRef} ->
            continuous_acceptor(Pid, PidRef, IbrowseRef,
                #state{})
    after ?DEFAULT_TIMEOUT ->
        Pid ! {PidRef, {error, {timeout, []}}}
    end.


%% @doc main ibrowse loop to receive continuous changes 
continuous_acceptor(Pid, PidRef, IbrowseRef, State) ->
    receive
        {ibrowse_async_response_end, IbrowseRef} ->
            Pid ! {PidRef, done};
        {ibrowse_async_response, IbrowseRef, {error,Error}} ->
            Pid ! {PidRef, {error, Error}};
        {ibrowse_async_response, IbrowseRef, Chunk} ->
            Messages = [M || M <- re:split(Chunk, ",?\n", [trim]), M =/= <<>>],
            {ok, State1} = handle_messages(Messages, Pid, PidRef, IbrowseRef, State),
            continuous_acceptor(Pid, PidRef, IbrowseRef, State1);
        {ibrowse_async_headers, IbrowseRef, Status, Headers} ->
            if Status =/= "200" ->
                    Pid ! {PidRef, {error, {Status, Headers}}};
                true ->
                    ibrowse:stream_next(IbrowseRef), 
                    continuous_acceptor(Pid, PidRef, IbrowseRef, State)
                    
            end 
    end.

handle_messages([], _Pid, _PidRef, IbrowseRef, State) ->
    ibrowse:stream_next(IbrowseRef),
    {ok, State};
handle_messages([<<"{\"last_seq\":", LastSeq/binary>>], Pid, PidRef,
        IbrowseRef, State) ->
    %% end of continuous response
    
    %% get last sequence
    L = size(LastSeq) - 1,
    <<LastSeq1:L/binary, _/binary>> = LastSeq,
    LastSeqInt = list_to_integer(binary_to_list(LastSeq1)),
    
    Pid ! {PidRef, {last_seq, LastSeqInt}},

    ibrowse:stream_next(IbrowseRef),
    {ok, State};
handle_messages([Chunk|Rest], Pid, PidRef, IbrowseRef, State) ->
    #state{partial_chunk=Partial}=State,
    NewState = try
        ChangeRow = decode_row(<<Partial/binary, Chunk/binary>>),
        Pid! {PidRef, {change, ChangeRow}},
        #state{}
    catch
    throw:{invalid_json, Bad} ->
        State#state{partial_chunk = Bad}
    end,
    handle_messages(Rest,  Pid, PidRef, IbrowseRef, NewState).

decode_row(<<",", Rest/binary>>) ->
    decode_row(Rest);
decode_row(Row) ->
    couchbeam_util:json_decode(Row).



