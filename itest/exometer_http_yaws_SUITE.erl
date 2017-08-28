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
%%% Common Tests for `exometer_http_yaws' application.
%%%
-module(exometer_http_yaws_SUITE).
-compile([{parse_transform, lager_transform}]).
-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
    test_static_configuration/1
]).

%%% ============================================================================
%%% Callbacks for `common_test'
%%% ============================================================================

%%  @doc
%%  CT API.
%%
all() -> [
    test_static_configuration
].


%%
%%  Log test case name at start
%%
init_per_testcase(TestCase, Config) ->
    lager:debug("---------------------- ~p start", [TestCase]),
    Config.


%%
%%  Log test case name at end. Also, clean subscriptions and metrics.
%%
end_per_testcase(TestCase, _Config) ->
    lager:debug("---------------------- ~p end", [TestCase]),
    ok.



%%% ============================================================================
%%% Test cases.
%%% ============================================================================

%%  @doc
%%
%%
test_static_configuration(_Config) ->
    ok.


