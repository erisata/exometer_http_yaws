%/--------------------------------------------------------------------
%| Copyright 2017 Erisata, UAB (Ltd.)
%|
%| Licensed under the Apache License, Version 2.0 (the "License");
%| you may not use this file except in compliance with the License.
%| You may obtain a copy of the License at
%|
%|     http://www.apache.org/licenses/LICENSE-2.0
%|
%| Unless required by applicable law or agreed to in writing, software
%| distributed under the License is distributed on an "AS IS" BASIS,
%| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%| See the License for the specific language governing permissions and
%| limitations under the License.
%\--------------------------------------------------------------------

%%%
%%% Main API.
%%%
-module(exometer_http_yaws).
-compile([{parse_transform, lager_transform}]).
-export([start/0, out/1]).
-include_lib("yaws/include/yaws_api.hrl").


%%% ============================================================================
%%% Public API
%%% ============================================================================

%%
%%  This is needed to be able to start this application from console,
%%  using `-s axb_webui` option.
%%
start() ->
    application:ensure_all_started(exometer_http_yaws_app:name(), permanent).


%%% @doc
%%% Creates the first line of csv file. The line consists
%%% of metric names. Starts continuous stream supervisor.
%%%
out(A) ->
    lager:info("OUT invoked."),
    Self = self(),
    Entries = exometer:find_entries([]),
    %
    % Form a list of Entries with datapoints
    Metrics = lists:flatten(lists:foldl(
        fun({Name, _Type, _Status}, Acc) ->
            Datapoints = exometer:info(Name, datapoints),
            FullMetricNames = [{Name, Datapoint} || Datapoint <- Datapoints],
            [FullMetricNames, Acc]
        end, [], Entries)),
    ListOfStrings = [format_metric_path(Name, Datapoint) || {Name, Datapoint} <- Metrics],
    FinalString = string:join(ListOfStrings, ";") ++ "\n",
    lager:info("Final string: ~p", [FinalString]),
    exometer_http_yaws_stream_sup:start_stream(Self, Metrics),
    {streamcontent, "application/octet-stream", FinalString}.



%%% ============================================================================
%%% Internal Functions.
%%% ============================================================================

%%  @private
%%  Forms Graphite compatible metric path out of Exometer's Probe and DataPoint.
%%
format_metric_path(Probe, DataPoint) ->
    string:join(lists:map(fun metric_elem_to_list/1, Probe ++ [DataPoint]), ".").


%%  @private
%%  Converts metric path elements to list (string)
%%
metric_elem_to_list(V) when is_atom(V)    -> erlang:atom_to_list(V);
metric_elem_to_list(V) when is_integer(V) -> erlang:integer_to_list(V);
metric_elem_to_list(V) when is_list(V)    -> V.

