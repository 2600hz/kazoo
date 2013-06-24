{application, kazoo_translator,
 [
  {description, "Bridging various call control formats to the 2600hz JSON APIs"},
  {vsn, "1.0.0"},
  {registered, []},
  {modules, [kazoo_translator_app, kazoo_translator_sup, kzt_receiver, kzt_translator, kzt_util,kzt_kazoo, kzt_twiml_dial, kzt_twiml]},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { kazoo_translator_app, []}},
  {env, []}
 ]}.
