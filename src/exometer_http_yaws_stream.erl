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
-export([start_link/2]).
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
start_link(YawsPid, Metrics) ->
    gen_server:start_link(?MODULE, [YawsPid, Metrics], []).



%%% ============================================================================
%%% Internal state of the module.
%%% ============================================================================

-record(state, {
    yaws_pid    :: term() | undefined,
    metrics     :: list() %% TODO
}).



%%% ============================================================================
%%% Callbacks for `gen_server'.
%%% ============================================================================

%% @doc
%%
%%
init([YawsPid, Metrics]) ->
    erlang:link(YawsPid),
    lager:info("INIT!"),
    State = #state{
        yaws_pid = YawsPid,
        metrics = Metrics
    },
    self() ! stream,
    {ok, State}.


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
handle_info(stream, State = #state{yaws_pid = YawsPid, metrics = Metrics}) ->
    Delay = exometer_http_yaws_app:get_env(delay, ?DEFAULT_DELAY),
    erlang:send_after(Delay, self(), stream),
    ListOfStrings = lists:map(
        fun({Name, Datapoint}) ->
            {ok, [{Datapoint, Value}]} = exometer:get_value(Name, Datapoint),
            lists:flatten(io_lib:format("~w", [Value]))
        end, Metrics),
    FinalString = string:join(ListOfStrings, ";") ++ "\n",
    lager:info("RETURN VAL: ~p", [yaws_api:stream_chunk_deliver(YawsPid, FinalString)]),
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
