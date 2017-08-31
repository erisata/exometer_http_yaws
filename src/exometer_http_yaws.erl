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
%%  using `-s exometer_http_yaws` option.
%%
start() ->
    application:ensure_all_started(exometer_http_yaws_app:name(), permanent).


%%% @doc
%%% Starts stream of Exometer metrics as csv values to the client socket.
%%%
out(A) ->
    {ok, Pid} = exometer_http_yaws_stream_sup:start_stream(A#arg.clisock),
    {streamcontent_from_pid, "text/plain", Pid}.


