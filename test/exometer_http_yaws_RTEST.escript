#!/usr/bin/env escript

main(_Args) ->
    %
    %  Set application config before starting all the applications.
    %  The --config option is handled after the --script.
    %
    {ok, [AppEnvs]} = file:consult("test/sys.config"),
    ok = lists:foreach(fun ({App, Envs}) ->
        ok = lists:foreach(fun ({EnvName, EnvValue}) ->
            ok = application:set_env(App,  EnvName, EnvValue, [persistent])
        end, Envs)
    end, AppEnvs),
    {ok, _} = application:ensure_all_started(exometer_http_yaws),
    {ok, _} = application:ensure_all_started(sync),

    exometer:new([testA, cpuUsage], gauge),
    exometer:new([testB, memUsage], histogram),
    ok.

