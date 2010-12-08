{application,winkstart,
             [{description,"Winkstart - REST Interface to the stars"},
              {vsn,"0.0.1"},
              {modules,[static_resource,winkstart,winkstart_app,
                        winkstart_resource,winkstart_sup]},
              {registered,[]},
              {applications,[kernel,stdlib,crypto,mochiweb,webmachine]},
              {mod,{winkstart_app,[]}},
              {env,[]}]}.
