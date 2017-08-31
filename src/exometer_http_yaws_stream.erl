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

%%% @doc
%%% Streams values of specified exometer metrics.
%%%
-module(exometer_http_yaws_stream).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-export([start_link/1]).
-export([
    init/1,
    handle_info/2,
    handle_cast/2,
    handle_call/3,
    code_change/3,
    terminate/2
]).

-define(DEFAULT_DELAY, 1000).

%%% ============================================================================
%%% API functions.
%%% ============================================================================

%%  @doc
%%  Start a streamer.
%%
start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).


%%% ============================================================================
%%% Internal state of the module.
%%% ============================================================================

-record(state, {
    socket    :: term() | undefined,
    yaws_pid    :: term() | undefined,
    metrics     :: [{EntryName :: [atom()], DataPoint :: atom()}]
}).



%%% ============================================================================
%%% Callbacks for `gen_server'.
%%% ============================================================================

%%  @doc
%%  Saves socket of the client and starts streaming procedure.
%%
init([Socket]) ->
    self() ! setup,
    {ok, #state{socket = Socket}}.


%%  @doc
%%  Unused.
%%
handle_call(_Unknown, _From, State) ->
    {noreply, State}.


%%  @doc
%%  Unused.
%%
handle_cast(_Unknown, State) ->
    {noreply, State}.


%%  @doc
%%  Setups socket between client and Yaws.
%%  It waits for Yaws to confirm that the socket is ready for streaming.
%%
handle_info(setup, State = #state{socket = Socket}) ->
    receive
        {ok, YawsPid} ->
            self() ! stream_header,
            NewState = State#state{
                yaws_pid = YawsPid
            },
            {noreply, NewState};
        {discard, YawsPid} ->
            lager:info("Yaws unable to send data to client socket. Check HTTP request."),
            yaws_api:stream_process_end(Socket, YawsPid),
            {stop, normal, State}
    after
        4000 ->
            lager:warning("Timeout while setting up Yaws content stream."),
            {stop, normal, State}
    end;

%%  @doc
%%  Streams first line of csv which contains metric names.
%%
handle_info(stream_header, State) ->
    #state{
        socket = Socket,
        yaws_pid = YawsPid
    } = State,
    Metrics = get_metrics(),
    HeaderLine = create_header_line(Metrics),
    lager:info("Stream started. Number of metrics: ~p", [erlang:length(Metrics)]),
    case yaws_api:stream_process_deliver_chunk(Socket, HeaderLine) of
        ok ->
            self() ! stream_data,
            NewState = State#state{metrics = Metrics},
            {noreply, NewState};
        {error, _} ->
            lager:info("Stream stopped. Connection lost."),
            yaws_api:stream_process_end(Socket, YawsPid),
            {stop, normal, State}
    end;

%%  @doc
%%  Stream one line of csv which contains values of the metrics.
%%
handle_info(stream_data, State) ->
    #state{
        socket = Socket,
        yaws_pid = YawsPid,
        metrics = Metrics
    } = State,
    DataLine = create_data_line(Metrics),
    case yaws_api:stream_process_deliver_chunk(Socket, DataLine) of
        ok ->
            Delay = exometer_http_yaws_app:get_env(delay, ?DEFAULT_DELAY),
            erlang:send_after(Delay, self(), stream_data),
            {noreply, State};
        {error, _} ->
            lager:info("Stream stopped. Connection lost."),
            yaws_api:stream_process_end(Socket, YawsPid),
            {stop, normal, State}
    end;

handle_info(_Unknown, State) ->
    {noreply, State}.


%%  @doc
%%  Unused.
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%  @doc
%%  Unused.
%%
terminate(_Reason, _State) ->
    ok.



%%% ============================================================================
%%% Internal functions.
%%% ============================================================================

%%  @private
%%  Creates a single csv line containing names of metrics.
%%
create_header_line(Metrics) ->
    ListOfStrings = [format_metric_path(Name, Datapoint) || {Name, Datapoint} <- Metrics],
    string:join(ListOfStrings, ",") ++ "\n".


%%  @private
%%  Creates a single csv line containing values of metrics.
%%
create_data_line(Metrics) ->
    ListOfStrings = lists:map(
        fun({Name, Datapoint}) ->
            {ok, [{Datapoint, Value}]} = exometer:get_value(Name, Datapoint),
            lists:flatten(io_lib:format("~w", [Value]))
        end, Metrics),
    string:join(ListOfStrings, ",") ++ "\n".


%%  @private
%%  Gets metrics from Exometer. Creates a list of
%%  metrics containing probes and their datapoints.
%%
get_metrics() ->
    Entries = exometer:find_entries([]),
    _Metrics = lists:flatten(lists:foldl(
        fun({Name, _Type, _Status}, Acc) ->
            Datapoints = exometer:info(Name, datapoints),
            FullMetricNames = [{Name, Datapoint} || Datapoint <- Datapoints],
            [FullMetricNames, Acc]
        end, [], Entries)).


%%  @private
%%  Forms a dot separated string of Probe and its datapoint.
%%
format_metric_path(Probe, DataPoint) ->
    string:join(lists:map(fun metric_elem_to_list/1, Probe ++ [DataPoint]), ".").


%%  @private
%%  Converts metric path elements to list (string)
%%
metric_elem_to_list(V) when is_atom(V)    -> erlang:atom_to_list(V);
metric_elem_to_list(V) when is_integer(V) -> erlang:integer_to_list(V);
metric_elem_to_list(V) when is_list(V)    -> V.



%%% ============================================================================
%%% Test cases for internal functions.
%%% ============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%
%%  Check, if pickle message is created properly.
%%
create_header_line_test_() ->
    [
        {"Check, if line of probes and their datapoints formatted correctly", ?_assertEqual(
            "testB.memUsage.n,testB.memUsage.mean,testB.memUsage.min\n",
            create_header_line([
                {[testB, memUsage], n},
                {[testB, memUsage], mean},
                {[testB, memUsage], min}
                ]))},
        {"Check, if header line formatted correctly when there are no metrics", ?_assertEqual(
            "\n",
            create_header_line([]))}
    ].


%%
%%  Check, misc metric path combinations.
%%
format_metric_path_test_() ->
    [
        ?_assertEqual("min",            format_metric_path([], min)),
        ?_assertEqual("dir1.dir2.max",  format_metric_path([dir1, dir2], max)),
        ?_assertEqual("dir1.dir2.max",  format_metric_path(["dir1", "dir2"], max)),
        ?_assertEqual("dir.1.max",      format_metric_path([dir, 1], max)),
        ?_assertEqual("a.first.value",  format_metric_path([a, first], value)),
        ?_assertEqual("20.first.50.75",  format_metric_path([20, first, 50], 75))
    ].



-endif.


