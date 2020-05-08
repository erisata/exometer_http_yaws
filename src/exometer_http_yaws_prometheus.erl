%%% @doc
%%% Provides stats in the Prometheus line based format as described here:
%%% <https://github.com/prometheus/docs/blob/master/content/docs/instrumenting/exposition_formats.md>.
%%%
-module(exometer_http_yaws_prometheus).
-compile({parse_transform, lager_transform}).
-export([out/1]).
-include_lib("yaws/include/yaws_api.hrl").

%%  @doc
%%  Yaws Appmod callback.
%%
out(Arg  = #arg{req = #http_request{method = Method}}) ->
    out(path_tokens(Arg), Method, Arg).


out(PathTokens, 'GET', _Arg) ->
    {MetricsText, Nested} = lists:mapfoldl(fun ({Name, _Type, Status}, Nested) ->
        case lists:nthtail(length(PathTokens), Name) of
            [LocalName] when Status =:= enabled ->
                % Datapoints = exometer:info(Name, datapoints),
                case exometer:get_value(Name) of
                    {ok, Values} ->
                        {[[
                            io_lib:format("~s {dp = \"~w\"} ~w~n", [LocalName, Datapoint, Value])
                            || {Datapoint, Value} <- Values, Value =/= undefined
                        ], "\n"], Nested};
                    {error, _Reason} ->
                        {[], Nested} % Ignore errors in this case.
                end;
            [LocalName | _] ->
                % List deeper levels.
                {[], [LocalName | Nested]};
            [] ->
                {[], Nested}
        end
    end, [], exometer:find_entries(PathTokens)),
    NestedText = [
        ["# NESTED: ", erlang:atom_to_list(N), "\n"]
        || N <- lists:usort(Nested)
    ],
    [
        {status, 200},
        {content, "text/plain; version=0.0.4", [MetricsText, NestedText]}
    ];

out(_PathTokens, _Method, _Arg) ->
    [
        {status, 400}
    ].


%%
%%  Returns path tokens, that are under the appmod path.
%%
path_tokens(#arg{appmoddata = undefined}) ->
    [];

path_tokens(#arg{appmoddata = AppmodUri}) ->
    try 
        lists:map(
            fun erlang:list_to_existing_atom/1,
            string:tokens(AppmodUri, "/")
        )
    catch
        error:badarg ->
            % consider metrics not found, if path atom is unknown.
            undefined
    end.


