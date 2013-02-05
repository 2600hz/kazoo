%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Convert FS member xml to #participant{}
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(fs_xml_member).

-export([xml_to_participant/2]).

-include("ecallmgr.hrl").

xml_to_participant(#xmlElement{name='member'
                               ,attributes=Attrs
                               ,content=MemberTags
                              }, Node) ->
    update_from_children(from_attrs(Attrs, Node), MemberTags).

from_attrs(Attrs, Node) -> from_attrs(Attrs, Node, #participant{}).

from_attrs([], Node, P) -> P#participant{node=Node};
from_attrs([#xmlAttribute{name='type'
                          ,value="caller"
                         }
            |Attrs], Node, P) ->
    lager:debug("member is a caller"),
    from_attrs(Attrs, Node, P);
from_attrs([_Attr|Attrs], Node, P) ->
    lager:debug("unhandled attr: ~p", [_Attr]),
    from_attrs(Attrs, Node, P).

update_from_children(P, []) -> P;
update_from_children(P, [#xmlElement{name='id'
                                     ,content=Id
                                    }
                         |Els]) ->
    update_from_children(P#participant{member_id=wh_util:to_integer(xml_text_to_binary(Id))}, Els);
update_from_children(P, [#xmlElement{name='flags'
                                     ,content=Flags
                                    }
                         |Els]) ->
    update_from_children(update_from_flags(P, Flags), Els);
update_from_children(P, [#xmlElement{name='uuid'
                                     ,content=UUID
                                    }
                         |Els]) ->
    update_from_children(P#participant{uuid=xml_text_to_binary(UUID)}, Els);
update_from_children(P, [#xmlElement{name='energy'
                                    ,content=Energy
                                    }
                        |Els]) ->
    update_from_children(P#participant{energy_level=wh_util:to_integer(xml_text_to_binary(Energy))}, Els);
update_from_children(P, [#xmlElement{name=_N}|Els]) ->
    update_from_children(P, Els);
update_from_children(P, [_El|Els]) ->
    update_from_children(P, Els).

update_from_flags(P, []) -> P;
update_from_flags(P, #xmlElement{name='flags'
                                 ,content=Flags
                                }) ->
    update_from_flags(P, Flags);
update_from_flags(P, [#xmlElement{name='talking'
                                 ,content=Speak
                                 }
                     |Els]) ->
    update_from_flags(P#participant{speak=wh_util:is_true(xml_text_to_binary(Speak))}, Els);
update_from_flags(P, [#xmlElement{name='has_floor'
                                 ,content=HasFloor
                                 }
                     |Els]) ->
    update_from_flags(P#participant{floor=wh_util:is_true(xml_text_to_binary(HasFloor))}, Els);
update_from_flags(P, [#xmlElement{name='is_moderator'
                                 ,content=IsMod
                                 }
                     |Els]) ->
    update_from_flags(P#participant{is_moderator=wh_util:is_true(xml_text_to_binary(IsMod))}, Els);
update_from_flags(P, [_|Els]) ->
    update_from_flags(P, Els).

xml_text_to_binary(Els) -> iolist_to_binary([V || #xmlText{value=V} <- Els]).
