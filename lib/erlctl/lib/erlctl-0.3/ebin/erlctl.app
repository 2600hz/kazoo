% Copyright 2010, Scatterbit, Inc. & Jayson Vantuyl <jvantuyl@scatterbit.com>
%
% This file is part of erlctl.
%
% Erlctl is open source.  See LICENSE.txt for details.
{application,
  erlctl,
  [
    {description, "ErlCtl Control Framework"},
    {vsn, "0.1"},
    {modules, [
			erlctl,
			erlctl_app,
			erlctl_cli,
			erlctl_cmd,
			erlctl_cmdline,
			erlctl_err,
			erlctl_net,
			erlctl_proc,
			erlctl_sup,
			erlctl_vm
		]},
    {registered, [erlctl_sup]},
    {applications, [kernel, stdlib, sasl]},
    {mod, {erlctl_app,[]}},
    {env, []}
 ]
}.
