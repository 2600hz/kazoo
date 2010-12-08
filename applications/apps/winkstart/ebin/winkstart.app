{application,winkstart,
             [{description,"Winkstart - REST Interface to the stars"},
              {vsn,"0.0.1"},
              {modules,[winkstart,winkstart_app,winkstart_bindings,
                        winkstart_resource,winkstart_sup]},
              {registered,[]},
              {applications,[kernel,stdlib,crypto,mochiweb,webmachine]},
              {mod,{winkstart_app,[]}},
              {env,[]}]}.
