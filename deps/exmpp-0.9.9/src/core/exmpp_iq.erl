%% Copyright ProcessOne 2006-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides helpers to do IQ common
%% operations.

-module(exmpp_iq).

-include("exmpp.hrl").

%% avoid name clash with local error/2 function
-compile({no_auto_import,[error/2]}).

%% Creation.
-export([
	 get/2,
	 get/3,
	 set/2,
	 set/3,
	 result/1,
	 result/2,
	 error/2,
	 error/3,
	 error_without_original/2
	]).

%% #iq record conversion.
-export([
	 xmlel_to_iq/1,
	 iq_to_xmlel/1,
	 iq_to_xmlel/3
	]).

%% IQ standard attributes.
-export([
	 is_iq/1,
	 is_iq_record/1,
	 is_request/1,
	 is_response/1,
	 is_result/1,
	 is_error/1,
	 get_type/1,
	 get_kind/1,
	 get_request/1,
	 get_result/1,
	 get_payload/1,
     get_payload_ns_as_atom/1
	]).

%% --------------------------------------------------------------------
%% Documentation / type definitions.
%% --------------------------------------------------------------------

%% @type iq() = {iq, Kind, Type, ID, NS, Payload, Error, Lang, IQ_NS}
%%     Kind = request | response
%%     Type = get | set | result | error
%%     ID = binary() | undefined
%%     NS = atom() | string() | undefined
%%     Payload = exmpp_xml:xmlel() | undefined
%%     Error = exmpp_xml:xmlel() | undefined
%%     Lang = binary() | undefined
%%     IQ_NS = atom() | string() | undefined.
%% Record representing an IQ stanza.
%%
%% It's created from an #xmlel using {@link xmlel_to_iq/1}. This record
%% eases matching in function clauses. It may be passed to functions in
%% {@link exmpp_stanza} and {@link exmpp_iq}. For other operations, it
%% must be converted back to #xmlel using {@link iq_to_xmlel/1}.

%% --------------------------------------------------------------------
%% IQ creation.
%% --------------------------------------------------------------------

%% @spec (NS, Request) -> IQ
%%     NS = atom()
%%     Request = exmpp_xml:xmlel()
%%     IQ = exmpp_xml:xmlel()
%% @doc Prepare an `<iq/>' to transport the given `get' request.

-spec(get/2 :: (xmlname(), #xmlel{}) -> #xmlel{}).

get(NS, Request) ->
    get(NS, Request, random).

%% @spec (NS, Request, ID) -> Request_IQ
%%     NS = atom()
%%     Request = exmpp_xml:xmlel()
%%     ID = binary() | string() | random
%%     Request_IQ = exmpp_xml:xmlel()
%% @doc Prepare an `<iq/>' to transport the given `get' request.

-spec(get/3 ::
      (xmlname(), #xmlel{}, binary() | string() | random) -> #xmlel{}).

get(NS, Request, ID) ->
    Attrs1 = exmpp_stanza:set_type_in_attrs([], <<"get">>),
    Attrs2 = exmpp_stanza:set_id_in_attrs(Attrs1, ID),
    #xmlel{ns = NS,
	   name = 'iq',
	   attrs = Attrs2,
	   children = [Request]
	  }.

%% @spec (NS, Request) -> Request_IQ
%%     NS = atom()
%%     Request = exmpp_xml:xmlel()
%%     Request_IQ = exmpp_xml:xmlel()
%% @doc Prepare an `<iq/>' to transport the given `set' request.

-spec(set/2 :: (xmlname(), #xmlel{}) -> #xmlel{}).

set(NS, Request) ->
    set(NS, Request, random).

%% @spec (NS, Request, ID) -> Request_IQ
%%     NS = atom()
%%     Request = exmpp_xml:xmlel()
%%     ID = binary() | string() | random
%%     Request_IQ = exmpp_xml:xmlel()
%% @doc Prepare an `<iq/>' to transport the given `set' request.

-spec(set/3 ::
      (xmlname(), #xmlel{}, binary() | string() | random) -> #xmlel{}).

set(NS, Request, ID) ->
    Attrs1 = exmpp_stanza:set_type_in_attrs([], <<"set">>),
    Attrs2 = exmpp_stanza:set_id_in_attrs(Attrs1, ID),
    #xmlel{ns = NS,
	   name = 'iq',
	   attrs = Attrs2,
	   children = [Request]
	  }.

%% @spec (Request_IQ) -> Response_IQ
%%     Request_IQ = exmpp_xml:xmlel() | iq()
%%     Response_IQ = exmpp_xml:xmlel() | iq()
%% @doc Prepare an `<iq/>' to answer to the given request.

-spec(result/1 :: (#xmlel{} | #iq{}) -> #xmlel{} | #iq{}).

result(Request_IQ) when ?IS_IQ(Request_IQ) ->
    Attrs1 = exmpp_stanza:reply_from_attrs(Request_IQ#xmlel.attrs),
    Attrs2 = exmpp_stanza:set_type_in_attrs(Attrs1, <<"result">>),
    #xmlel{ns = Request_IQ#xmlel.ns,
	   name = 'iq',
	   attrs = Attrs2
	  };
result(Request_IQ_Rec) when ?IS_IQ_RECORD(Request_IQ_Rec) ->
    Request_IQ_Rec#iq{kind = response,
		      type = result,
		      ns = undefined,
		      payload = undefined
		     }.

%% @spec (Request_IQ, Result) -> Response_IQ
%%     Request_IQ = exmpp_xml:xmlel() | iq()
%%     Result = exmpp_xml:xmlel()
%%     Response_IQ = exmpp_xml:xmlel() | iq()
%% @doc Prepare an `<iq/>' to answer to the given request with `Result'.

-spec(result/2 :: (#xmlel{} | #iq{}, #xmlel{}) -> #xmlel{} | #iq{}).

result(Request_IQ, Result) when ?IS_IQ(Request_IQ) ->
    exmpp_xml:set_children(result(Request_IQ), [Result]);
result(Request_IQ_Rec, Result) when ?IS_IQ_RECORD(Request_IQ_Rec) ->
    Result_IQ_Rec = result(Request_IQ_Rec),
    Result_IQ_Rec#iq{ns = Result#xmlel.ns, payload = Result}.

%% @spec (Request_IQ, Error) -> Response_IQ
%%     Request_IQ = exmpp_xml:xmlel() | iq()
%%     Error = exmpp_xml:xmlel() | atom()
%%     Response_IQ = exmpp_xml:xmlel() | iq()
%% @doc Prepare an `<iq/>' to notify an error.
%%
%% If `Error' is an atom, it must be a standard condition defined by
%% XMPP Core.

-spec(error/2 ::
      (#xmlel{} | #iq{}, #xmlel{} | atom()) -> #xmlel{} | #iq{}).

error(IQ, Condition)
  when is_atom(Condition) andalso ?IS_IQ(IQ) ->
    Error = exmpp_stanza:error(IQ#xmlel.ns, Condition),
    error(IQ, Error);
error(IQ_Rec, Condition)
  when is_atom(Condition) andalso ?IS_IQ_RECORD(IQ_Rec) ->
    Error = exmpp_stanza:error(IQ_Rec#iq.iq_ns, Condition),
    error(IQ_Rec, Error);
error(IQ, Error) when ?IS_IQ(IQ) ->
    exmpp_stanza:reply_with_error(IQ, Error);
error(IQ_Rec, Error) when ?IS_IQ_RECORD(IQ_Rec) ->
    IQ_Rec#iq{kind = response,
	      type = error,
	      error = Error
	     }.

%% @spec (Request_IQ, Condition, Text) -> Response_IQ
%%     Request_IQ = exmpp_xml:xmlel() | iq()
%%     Condition = atom()
%%     Text = string() | binary()
%%     Response_IQ = exmpp_xml:xmlel() | iq()
%% @doc Prepare an `<iq/>' to notify an error
%%      with an error <text/>
%% If `Error' is an atom, it must be a standard condition defined by
%% XMPP Core.

-spec(error/3 ::
(
  Request_IQ :: #xmlel{} | #iq{},
  Condition  :: atom(),
  Text       :: string() | binary())
    -> #xmlel{} | #iq{}
).

error(IQ, Condition, Text)
  when is_atom(Condition) andalso ?IS_IQ(IQ) ->
    Error = exmpp_stanza:error(IQ#xmlel.ns, Condition, {undefined, Text}),
    error(IQ, Error);
error(IQ_Rec, Condition, Text)
  when is_atom(Condition) andalso ?IS_IQ_RECORD(IQ_Rec) ->
    Error = exmpp_stanza:error(IQ_Rec#iq.iq_ns, Condition, {undefined, Text}),
    error(IQ_Rec, Error).

%% @spec (Request_IQ, Error) -> Response_IQ
%%     Request_IQ = exmpp_xml:xmlel() | iq()
%%     Error = exmpp_xml:xmlel() | atom()
%%     Response_IQ = exmpp_xml:xmlel() | iq()
%% @doc Prepare an `<iq/>' to notify an error.
%%
%% Child elements from `Request_IQ' are not kept.
%%
%% If `Error' is an atom, it must be a standard condition defined by
%% XMPP Core.

-spec(error_without_original/2 ::
      (#xmlel{} | #iq{}, #xmlel{} | atom()) -> #xmlel{} | #iq{}).

error_without_original(IQ, Condition) when is_atom(Condition) ->
    Error = exmpp_stanza:error(IQ#xmlel.ns, Condition),
    error_without_original(IQ, Error);
error_without_original(IQ, Error) when ?IS_IQ(IQ) ->
    exmpp_stanza:reply_with_error(IQ#xmlel{children = []}, Error);
error_without_original(IQ_Rec, Error) when ?IS_IQ_RECORD(IQ_Rec) ->
    IQ_Rec#iq{kind = response,
	      type = error,
	      error = Error,
	      payload = undefined
	     }.

%% --------------------------------------------------------------------
%% #iq record conversion.
%% --------------------------------------------------------------------

%% @spec (IQ) -> IQ_Rec
%%     IQ = exmpp_xml:xmlel()
%%     IQ_Rec = iq()
%% @doc Convert an IQ stanza from its #xmlel form to its #iq form.

-spec(xmlel_to_iq/1 :: (#xmlel{}) -> #iq{}).

xmlel_to_iq(#xmlel{ns = IQ_NS} = IQ) when ?IS_IQ(IQ) ->
    Kind = get_kind(IQ),
    Type = get_type(IQ),
    ID = exmpp_stanza:get_id(IQ),
    {NS, Payload, Error} =
	case get_payload(IQ) of
	    #xmlel{ns = IQ_NS} = E when Type == 'error' ->
		case get_request(IQ) of
		    undefined ->
			{undefined, undefined, E};
		    #xmlel{ns = N} = P ->
			{N, P, E}
		end;
	    #xmlel{ns = N} = P ->
		{N, P, undefined};
	    undefined ->
		{undefined, undefined, undefined}
	end,
    Lang = exmpp_stanza:get_lang(IQ),
    #iq{kind = Kind,
	type = Type,
	id = ID,
	ns = NS,
	payload = Payload,
	error = Error,
	lang = Lang,
	iq_ns = IQ_NS
       }.

%% @spec (IQ_Rec) -> IQ
%%     IQ_Rec = iq()
%%     IQ = exmpp_xml:xmlel()
%% @doc Convert an IQ stanza from its #iq form to its #xmlel form.

-spec(iq_to_xmlel/1 :: (#iq{}) -> #xmlel{}).

iq_to_xmlel(IQ_Rec) when ?IS_IQ_RECORD(IQ_Rec) ->
    iq_to_xmlel2(IQ_Rec, []).

%% @spec (IQ_Rec, Sender, Recipient) -> IQ
%%     IQ_Rec = iq()
%%     Sender = exmpp_jid:jid() | binary() | string()
%%     Recipient = exmpp_jid:jid() | binary() | string()
%%     IQ = exmpp_xml:xmlel()
%% @doc Convert an IQ stanza from its #iq form to its #xmlel form and
%% set the sender and recipient at the same time.

-spec(iq_to_xmlel/3 ::
      (#iq{}, exmpp_stanza:jidlike(), exmpp_stanza:jidlike()) -> #xmlel{}).

iq_to_xmlel(IQ_Rec, Sender, Recipient) when ?IS_IQ_RECORD(IQ_Rec) ->
    Attrs = exmpp_stanza:set_jids_in_attrs([], Sender, Recipient),
    iq_to_xmlel2(IQ_Rec, Attrs).

iq_to_xmlel2(#iq{type = Type, id = ID, lang = Lang, payload = Payload,
		 error = Error, iq_ns = IQ_NS}, Attrs) ->
    Attrs1 = exmpp_stanza:set_type_in_attrs(Attrs, Type),
    Attrs2 = exmpp_stanza:set_id_in_attrs(Attrs1, ID),
    Attrs3 = exmpp_stanza:set_lang_in_attrs(Attrs2, Lang),
    Children1 = if
		    Type == 'error' andalso Error /= undefined -> [Error];
		    true                                       -> []
		end,
    Children2 = case Payload of
		    undefined -> Children1;
		    _         -> [Payload | Children1]
		end,
    #xmlel{ns = IQ_NS,
	   name = 'iq',
	   attrs = Attrs3,
	   children = Children2
	  }.

%% --------------------------------------------------------------------
%% IQ standard attributes.
%% --------------------------------------------------------------------

%% @spec (El) -> boolean()
%%     El = exmpp_xml:xmlel()
%% @doc Tell if `El' is an IQ.
%%
%% You should probably use the `IS_IQ(El)' guard expression.

-spec(is_iq/1 :: (#xmlel{}) -> boolean()).

is_iq(IQ) when ?IS_IQ(IQ) -> true;
is_iq(_El)                -> false.

%% @spec (El) -> boolean()
%%     El = iq()
%% @doc Tell if `El' is an IQ record.
%%
%% You should probably use the `IS_IQ_RECORD(El)' guard expression.

-spec(is_iq_record/1 :: (#iq{}) -> boolean()).

is_iq_record(IQ) when ?IS_IQ_RECORD(IQ) -> true;
is_iq_record(_El)                       -> false.

%% @spec (IQ) -> Type
%%     IQ = exmpp_xml:xmlel() | iq()
%%     Type = get | set | result | error | undefined
%% @doc Return the type of the given `<iq/>'.

-spec(get_type/1 ::
      (#xmlel{} | #iq{}) -> get | set | result | error | undefined).

get_type(IQ) when ?IS_IQ(IQ) ->
    case exmpp_stanza:get_type(IQ) of
        <<"get">>    -> 'get';
        <<"set">>    -> 'set';
        <<"result">> -> 'result';
        <<"error">>  -> 'error';
        _            -> undefined
    end;
get_type(#iq{type = Type}) ->
    Type.

%% @spec (IQ) -> Kind
%%     IQ = exmpp_xml:xmlel() | iq()
%%     Kind = request | response | undefined
%% @doc Tell if an IQ is a request or a response.

-spec(get_kind/1 ::
      (#xmlel{} | #iq{}) -> request | response | undefined).

get_kind(IQ) when ?IS_IQ(IQ) ->
    case get_type(IQ) of
        'get'    -> request;
        'set'    -> request;
        'result' -> response;
        'error'  -> response;
        _        -> undefined
    end;
get_kind(#iq{kind = Kind}) ->
    Kind.

%% @spec (IQ) -> boolean()
%%     IQ = exmpp_xml:xmlel() | iq()
%% @doc Tell if the IQ is a request.

-spec(is_request/1 :: (#xmlel{} | #iq{}) -> boolean()).

is_request(IQ) when ?IS_IQ(IQ) ->
    case get_kind(IQ) of
        request -> true;
        _       -> false
    end;
is_request(#iq{kind = Kind}) ->
    Kind == request.

%% @spec (IQ) -> boolean()
%%     IQ = exmpp_xml:xmlel() | iq()
%% @doc Tell if the IQ is a response.

-spec(is_response/1 :: (#xmlel{} | #iq{}) -> boolean()).

is_response(IQ) when ?IS_IQ(IQ) ->
    case get_kind(IQ) of
        response -> true;
        _        -> false
    end;
is_response(#iq{kind = Kind}) ->
    Kind == response.

%% @spec (IQ) -> boolean()
%%     IQ = exmpp_xml:xmlel() | iq()
%% @doc Tell if the IQ is a result (response of type `result').

-spec(is_result/1 :: (#xmlel{} | #iq{}) -> boolean()).

is_result(IQ) when ?IS_IQ(IQ) ->
    case get_type(IQ) of
        'result' -> true;
        _        -> false
    end;
is_result(#iq{type = Type}) ->
    Type == 'result'.

%% @spec (IQ) -> boolean()
%%     IQ = exmpp_xml:xmlel() | iq()
%% @doc Tell if the IQ is an error (response of type `error').

-spec(is_error/1 :: (#xmlel{} | #iq{}) -> boolean()).

is_error(IQ) when ?IS_IQ(IQ) ->
    case get_type(IQ) of
        'error' -> true;
        _       -> false
    end;
is_error(#iq{type = Type}) ->
    Type == 'error'.

%% @spec (IQ) -> Request | undefined
%%     IQ = exmpp_xml:xmlel() | iq()
%%     Request = exmpp_xml:xmlel()
%% @throws {iq, get_request, unexpected_iq, IQ} |
%%         {iq, get_request, invalid_iq, IQ}
%% @doc Return the request contained in a `get' or `set' IQ, or returned
%% by an `error' IQ (if present).

-spec(get_request/1 :: (#xmlel{} | #iq{}) -> #xmlel{} | undefined).

get_request(IQ) when ?IS_IQ(IQ) ->
    case get_type(IQ) of
        undefined ->
            throw({iq, get_request, invalid_iq, IQ});
        Type when Type == 'get' orelse Type == 'set' ->
	    %% We take the first child element. Note that the RFC says
	    %% that this child element MUST be the only one! This doesn't
	    %% take into account text nodes.
            [Request | _] = exmpp_xml:get_child_elements(IQ),
            Request;
        'result' ->
            throw({iq, get_request, unexpected_iq, IQ});
        'error' ->
            NS = IQ#xmlel.ns,
            [Request | Rest] = exmpp_xml:get_child_elements(IQ),
            case Request of
                #xmlel{ns = NS, name = 'error'} ->
                    case Rest of
                        []         -> undefined;
                        [Request2] -> Request2
                    end;
                _ ->
                    Request
            end
    end;
get_request(#iq{kind = request, payload = Request}) ->
    Request;
get_request(#iq{type = 'error', payload = Request}) ->
    Request;
get_request(#iq{} = IQ_Rec) ->
    throw({iq, get_request, unexpected_iq, IQ_Rec}).

%% @spec (IQ) -> Result | undefined
%%     IQ = exmpp_xml:xmlel() | iq()
%%     Result = exmpp_xml:xmlel()
%% @throws {iq, get_request, unexpected_iq, IQ} |
%%         {iq, get_result, invalid_iq, IQ}
%% @doc Return the result contained in a `result' IQ.

-spec(get_result/1 :: (#xmlel{} | #iq{}) -> #xmlel{} | undefined).

get_result(IQ) when ?IS_IQ(IQ) ->
    case get_type(IQ) of
        undefined ->
            throw({iq, get_result, invalid_iq, IQ});
        'result' ->
            case exmpp_xml:get_child_elements(IQ) of
                [] ->
                    undefined;
                [Result | _] ->
                    Result
            end;
        _ ->
            throw({iq, get_result, unexpected_iq, IQ})
    end;
get_result(#iq{type = 'result', payload = Result}) ->
    Result;
get_result(#iq{} = IQ_Rec) ->
    throw({iq, get_result, unexpected_iq, IQ_Rec}).

%% @spec (IQ) -> Payload
%%     IQ = exmpp_xml:xmlel() | iq()
%%     Payload = exmpp_xml:xmlel()
%% @throws {iq, get_payload, unexpected_iq, IQ}
%% @doc Extract the request, the result or the error from `IQ'.

-spec(get_payload/1 :: (#xmlel{} | #iq{}) -> #xmlel{} | undefined).

get_payload(IQ) ->
    case exmpp_iq:get_type(IQ) of
        'get'    -> exmpp_iq:get_request(IQ);
        'set'    -> exmpp_iq:get_request(IQ);
        'result' -> exmpp_iq:get_result(IQ);
        'error'  -> exmpp_stanza:get_error(IQ);
        _        -> throw({iq, get_payload, invalid_iq, IQ})
    end.


%% @spec (IQ) -> NS | undefined
%%    NS = atom()
%%  @doc Returns the query namespace.
%%       returns 'undefined' is there is no query element. 
 get_payload_ns_as_atom(IQ) ->
    case get_payload(IQ) of
        #xmlel{} = Q -> exmpp_xml:get_ns_as_atom(Q);
        undefined -> undefined
     end.
