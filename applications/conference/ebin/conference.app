{application, conference,
 [
  {description, "Conferencing Service Provider"},
  {vsn, "1.0.0"},
  {modules, [conf_discovery, conference_app, conference, conference_maintenance, conference_sup, conf_participant, conf_participant_sup, wapi_conf_participant]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { conference_app, []}},
  {env, []}
 ]}.
