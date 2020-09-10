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
-export([start_link/0, start_stream/1]).
-export([init/1]).


%%
%%
%%
-ignore_xref([
    {?MODULE, start_link, 0}    % Function used indirectly by supervisor exometer_http_yaws_sup.
]).


%%% ============================================================================
%%% API functions.
%%% ============================================================================

%%  @doc
%%  Create this supervisor.
%%
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).


%%  @doc
%%  Starts child process out of its spec.
%%
start_stream(Socket) ->
    supervisor:start_child(?MODULE, [Socket]).



%%% ============================================================================
%%% Callbacks for supervisor.
%%% ============================================================================

%%  @doc
%%  Supervisor initialization.
%%
init({}) ->
    SupFlags = #{
        strategy  => simple_one_for_one,
        intensity => 10,
        period    => 10000
    },
    StreamSpec = #{
        id       => exometer_http_yaws_stream,
        start    => {exometer_http_yaws_stream, start_link, []},
        restart  => temporary,
        shutdown => 5000,
        type     => worker,
        modules  => [exometer_http_yaws_stream]
    },
    {ok, {SupFlags, [StreamSpec]}}.


