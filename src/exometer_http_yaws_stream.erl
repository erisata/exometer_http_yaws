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

%% API
-export([start_link/2]).


%%% ============================================================================
%%% API functions.
%%% ============================================================================

%%  @doc
%%  Start a subscription manager.
%%
start_link(YawsPid, Metrics) ->
    Pid = spawn_link(fun() -> stream_data(YawsPid, Metrics) end),
    {ok, Pid}.



%%% ============================================================================
%%% Internal functions.
%%% ============================================================================

stream_data(YawsPid, Metrics) ->
    ListOfStrings = lists:map(
        fun({Name, Datapoint}) ->
            {ok, Value} = exometer:get_value(Name, Datapoint),
            lists:flatten(io_lib:format("~w", [Value]))
        end, Metrics),
    FinalString = string:join(ListOfStrings, ";"),
    lager:info("Final string: ~p", [FinalString]),
%%    yaws_api:stream_chunk_deliver(YawsPid, Data)
    ok.
