{application,riak_err,
             [{description,"Custom error handler"},
              {vsn,"1.0.1"},
              {modules,[riak_err_app,riak_err_handler,riak_err_monitor,
                        riak_err_stdlib,riak_err_sup,trunc_io]},
              {applications,[kernel,stdlib,sasl]},
              {registered,[]},
              {mod,{riak_err_app,[]}},
              {env,[]}]}.
