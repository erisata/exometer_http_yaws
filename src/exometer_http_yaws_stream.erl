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
%%  Start a subscription manager.
%%
start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).


%%% ============================================================================
%%% Internal state of the module.
%%% ============================================================================

-record(state, {
    socket    :: term() | undefined,
    yaws_pid    :: term() | undefined,
    metrics     :: list() %% TODO [{[atom(), atom(), ...], atom()}, ...]
}).



%%% ============================================================================
%%% Callbacks for `gen_server'.
%%% ============================================================================

%% @doc
%%
%%
init([Socket]) ->
    self() ! rec_loop,
    {ok, #state{socket = Socket}}. %% TODO dont forget testing with empty metrics


%% @doc
%% Unused.
%%
handle_call(_Unknown, _From, State) ->
    {noreply, State}.


%% @doc
%% Unused.
%%
handle_cast(_Unknown, State) ->
    {noreply, State}.


%% @doc
%%
%%
handle_info(rec_loop, State = #state{socket = Socket}) ->
    NewState = receive
                   {ok, YawsPid} ->
                       self() ! stream_header,
                       State#state{
                           yaws_pid = YawsPid
                       };
                   {discard, YawsPid} ->
                       lager:info("Socket discarded. Stopping stream."),
                       yaws_api:stream_process_end(Socket, YawsPid),
                       state#{} % TODO terminate gen_server?
               after
                   4000 ->
                    lager:info("Yaws error, timeout"), % TODO need better error messages
                    state#{}
               end,
    {noreply, NewState};


handle_info(stream_header, State) ->
    #state{
        socket = Socket,
        yaws_pid = YawsPid
    } = State,
    Metrics = get_metrics(),
    HeaderLine = create_header_line(Metrics),
    case yaws_api:stream_process_deliver_chunk(Socket, HeaderLine) of
        ok ->
            self() ! stream_data;
        {error, _} ->
            lager:info("Connection lost. Stopping stream."),
            yaws_api:stream_process_end(Socket, YawsPid)
    end,
    NewState = State#state{metrics = Metrics},
    {noreply, NewState};


handle_info(stream_data, State) ->
    #state{
        socket = Socket,
        yaws_pid = YawsPid,
        metrics = Metrics
    } = State,
    lager:info("STREAMING"),
    DataLine = create_data_line(Metrics),
    case yaws_api:stream_process_deliver_chunk(Socket, DataLine) of
        ok ->
            Delay = exometer_http_yaws_app:get_env(delay, ?DEFAULT_DELAY),
            erlang:send_after(Delay, self(), stream_data);
        {error, _} ->
            lager:info("Connection lost. Stopping stream."),
            yaws_api:stream_process_end(Socket, YawsPid)
    end,
    {noreply, State};



handle_info(_Unknown, State) ->
    {noreply, State}.


%% @doc
%% Unused.
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @doc
%% Unused.
%%
terminate(_Reason, _State) ->
    ok.



%%% ============================================================================
%%% Internal functions.
%%% ============================================================================

%%  @private
%%  TODO
%%
create_header_line(Metrics) ->
    ListOfStrings = [format_metric_path(Name, Datapoint) || {Name, Datapoint} <- Metrics],
    string:join(ListOfStrings, ";") ++ "\n".


create_data_line(Metrics) ->
    ListOfStrings = lists:map(
        fun({Name, Datapoint}) ->
            {ok, [{Datapoint, Value}]} = exometer:get_value(Name, Datapoint),
            lists:flatten(io_lib:format("~w", [Value]))
        end, Metrics),
    string:join(ListOfStrings, ";") ++ "\n".


%%  @private
%%  TODO
%%
get_metrics() ->
    Entries = exometer:find_entries([]),
    %
    % Form a list of Entries with datapoints
    _Metrics = lists:flatten(lists:foldl(
        fun({Name, _Type, _Status}, Acc) ->
            Datapoints = exometer:info(Name, datapoints),
            FullMetricNames = [{Name, Datapoint} || Datapoint <- Datapoints],
            [FullMetricNames, Acc]
        end, [], Entries)).


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
        ?_assertEqual("testB.memUsage.n;testB.memUsage.mean;testB.memUsage.min\n",
            create_header_line([
                {[testB, memUsage], n},
                {[testB, memUsage], mean},
                {[testB, memUsage], min}
                ])),
        ?_assertEqual("\n", create_header_line([]))
    ].


%%
%%  Check, if pickle message is created properly.
%%
create_data_line_test_() ->
    [

    ].


%%
%%  Check, misc graphite path combinations.
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
