%%% @doc
%%% Provides stats in the Prometheus line based format as described here:
%%% https://github.com/prometheus/docs/blob/master/content/docs/instrumenting/exposition_formats.md.
%%%
-module(exometer_http_yaws_prometheus).
-compile({parse_transform, lager_transform}).
-export([out/1, out/2]).
-include_lib("yaws/include/yaws_api.hrl").


%%
%%
%%
-ignore_xref([
    {?MODULE, out, 1}     % Yaws entry point
]).


%%  @doc
%%  Yaws Appmod callback.
%%
out(Arg  = #arg{req = #http_request{method = Method}}) ->
    out(path_tokens(Arg), Method, Arg).

out(Arg  = #arg{req = #http_request{method = Method}}, Flat) ->
    case Flat of
        true ->
            out_flat(path_tokens(Arg), Method, Arg);
        false ->
            out(path_tokens(Arg), Method, Arg)
    end.

out(undefined, 'GET', _Arg) ->
    [ % We had unknown atoms in the query, thus we don't know those metrics.
        {status, 404}
    ];

out(PathTokens, 'GET', _Arg) when is_list(PathTokens) ->
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

out_flat(PathTokens, 'GET', _Arg) when is_list(PathTokens) ->
    {MetricsText, Nested} = lists:mapfoldl(fun ({Name, _Type, Status}, Nested) ->
        case lists:nthtail(length(PathTokens), Name) of
            LocalNames when Status =:= enabled ->
                LocalName = string:join(lists:map(fun(LName) -> erlang:atom_to_list(LName) end, LocalNames), "_"),
                case exometer:get_value(Name) of
                    {ok, Values} ->
                        {[[
                            io_lib:format("~s {dp = \"~w\"} ~w~n", [LocalName, Datapoint, Value])
                            || {Datapoint, Value} <- Values, Value =/= undefined
                        ], "\n"], Nested};
                    {error, _Reason} ->
                        {[], Nested} % Ignore errors in this case.
                end
        end
    end, [], exometer:find_entries([])),
    NestedText = [
        ["# NESTED: ", erlang:atom_to_list(N), "\n"]
        || N <- lists:usort(Nested)
    ],
    [
        {status, 200},
        {content, "text/plain; version=0.0.4", [MetricsText, NestedText]}
    ];

out_flat(_PathTokens, _Method, _Arg) ->
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


