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
out(Arg = #arg{req = #http_request{method = Method}}) ->
    out_one_level(path_tokens(Arg), Method, Arg).

%%  @doc
%%  To be used with a wrapper module, where specific options can be passed.
%%  For now, only the possibility to get all the nested props are provided:
%%  E.g. `out(Arg, #{flat => true})'.
%%
out(Arg = #arg{req = #http_request{method = Method}}, Opts) ->
    case maps:get(flat, Opts, false) of
        true ->
            out_nested_flat(path_tokens(Arg), Method, Arg);
        false ->
            out_one_level(path_tokens(Arg), Method, Arg)
    end.


%% @private
%% Output metrics from a single nesting level only.
%%
out_one_level(PathTokens, 'GET', _Arg) when is_list(PathTokens) ->
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

out_one_level(undefined, 'GET', _Arg) ->
    [ % We had unknown atoms in the query, thus we don't know those metrics.
        {status, 404}
    ];

out_one_level(_PathTokens, _Method, _Arg) ->
    [
        {status, 400}
    ].


%% @private
%% Output all metrics in a flat list.
%%
out_nested_flat(PathTokens, 'GET', _Arg) when is_list(PathTokens) ->
    MetricsText = lists:map(fun ({Name, _Type, Status}) ->
        case lists:nthtail(length(PathTokens), Name) of
            [_ | _] = SubPath when Status =:= enabled ->
                case exometer:get_value(Name) of
                    {ok, Values} ->
                        MetricName = string:join(lists:map(fun erlang:atom_to_list/1, SubPath), "_"),
                        [[
                            io_lib:format("~s {dp = \"~w\"} ~w~n", [MetricName, DataPoint, Value])
                            || {DataPoint, Value} <- Values, Value =/= undefined
                        ], "\n"];
                    {error, _Reason} ->
                        [] % Ignore errors in this case.
                end;
            _ ->
                []
        end
    end, exometer:find_entries([])),
    [
        {status, 200},
        {content, "text/plain; version=0.0.4", [MetricsText]}
    ];

out_nested_flat(undefined, 'GET', _Arg) ->
    [ % We had unknown atoms in the query, thus we don't know those metrics.
        {status, 404}
    ];

out_nested_flat(_PathTokens, _Method, _Arg) ->
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


