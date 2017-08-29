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
%%% The main supervisor for the application.
%%%
-module(exometer_http_yaws_stream_sup).
-compile([{parse_transform, lager_transform}]).
-behaviour(supervisor).
-export([start_link/0, start_stream/2]).
-export([init/1]).


%%% ============================================================================
%%% API functions.
%%% ============================================================================

%%  @doc
%%  Create this supervisor.
%%
start_link() ->
    lager:info("STARTING STREAM SUP"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).


%%  @doc
%%  Create this supervisor.
%%
start_stream(YawsPid, Metrics) ->
    lager:info("STARTING STREAM!"),
    supervisor:start_child(?MODULE, [YawsPid, Metrics]).



%%% ============================================================================
%%% Callbacks for supervisor.
%%% ============================================================================

%%  @doc
%%  Supervisor initialization.
%%
init({}) ->
    StreamSpec = {exometer_http_yaws_stream,
        {exometer_http_yaws_stream, start_link, []},
        temporary,
        5000,
        worker,
        [exometer_http_yaws_stream]
    },
    {ok, {{simple_one_for_one, 10, 10000}, [StreamSpec]}}.
