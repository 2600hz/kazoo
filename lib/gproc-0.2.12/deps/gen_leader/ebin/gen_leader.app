%%% -*- mode: erlang -*-

{application, gen_leader,
 [
  {description, "The gen_leader behaviour"},
  {vsn, "1.0"},
  {id, "gen_leader"},
  {modules, [gen_leader]},
  {registered, [ ]},
  %% NOTE: do not list applications which are load-only!
  {applications, [ kernel, stdlib ] }
 ]
}.
