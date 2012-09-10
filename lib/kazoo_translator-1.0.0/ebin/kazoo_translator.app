{application,kazoo_translator,
             [{description,"Bridging various call control formats to the 2600hz JSON APIs"},
              {vsn,"1.0.0"},
              {registered,[]},
              {applications,[kernel,stdlib]},
              {mod,{kazoo_translator_app,[]}},
              {env,[]},
              {modules,[kazoo_translator_app,kazoo_translator_sup,
                        kzt_translator,kzt_twiml,kzt_util]}]}.
