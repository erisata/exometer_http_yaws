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
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    test_csv_file_providing/1
]).
-include_lib("common_test/include/ct.hrl").

-define(APP, exometer_http_yaws).

%%% ============================================================================
%%% Callbacks for `common_test'
%%% ============================================================================

%%  @doc
%%  CT API.
%%
all() -> [
    test_csv_file_providing
].


%%
%%  CT API, initialization.
%%
init_per_suite(Config) ->
    Port = 8006,
    _ = file:make_dir("logs"), % Needed by yaws (logdir).
    _ = file:make_dir("test"), % Needed by yaws (docroot).
    {ok, LagerApps} = application:ensure_all_started(lager),
    {ok, Apps} = application:ensure_all_started(?APP),
    {ok, _SConf} = yaws:add_server("test", [
        {port, Port},
        {listen, {0, 0, 0, 0}},
        {appmods, [{"/", exometer_http_yaws}]},
        {opaque, []}
    ]),
    [{itest_apps, LagerApps ++ Apps}, {port, Port} | Config].


%%
%%  CT API, cleanup.
%%
end_per_suite(Config) ->
    ok = lists:foreach(
        fun (A) -> application:stop(A) end,
        proplists:get_value(itest_apps, Config)
    ),
    ok.


%%
%%  Log test case name at start
%%
init_per_testcase(TestCase, Config) ->
    lager:debug("---------------------- ~p start", [TestCase]),
    Config.


%%
%%  Log test case name at end.
%%
end_per_testcase(TestCase, _Config) ->
    lager:debug("---------------------- ~p end", [TestCase]),
    ok.



%%% ============================================================================
%%% Test cases.
%%% ============================================================================

%%  @doc
%%  Check, if streaming metrics to file works.
%%
test_csv_file_providing(Config) ->
    ok = exometer:new([testA, cpuUsage], gauge),
    ok = exometer:new([testB, memUsage], histogram),
    {ok, Socket} = gen_tcp:connect("localhost", proplists:get_value(port, Config), [{active, true}]),
    ok = gen_tcp:send(Socket, "GET / HTTP/1.1\n\n"),
    {ok, Data1} = receive_data(3, Socket),
    MetricsLineRE =
        "testB\.memUsage\.n,"
        "testB\.memUsage\.mean,"
        "testB\.memUsage\.min,"
        "testB\.memUsage\.max,"
        "testB\.memUsage\.median,"
        "testB\.memUsage\.50,"
        "testB\.memUsage\.75,"
        "testB\.memUsage\.90,"
        "testB\.memUsage\.95,"
        "testB\.memUsage\.99,"
        "testB\.memUsage\.999,"
        "testA\.cpuUsage\.value,"
        "testA\.cpuUsage\.ms_since_reset\\n",
    ValueLineRE = "0,0,0,0,0,0,0,0,0,0,0,0,[0-9]+\\n",
    {match, _} = re:run(Data1, MetricsLineRE),
    {match, _} = re:run(Data1, ValueLineRE),
    ok = exometer:update_or_create([testA, cpuUsage], 50.99999, gauge, []),
    ok = exometer:update_or_create([testB, memUsage], 10, histogram, []),
    UpdatedValueLineRE = "10,10,10,10,10,10,10,10,10,10,50.99999,[0-9]+\\n",
    %
    % Wait for retrieval of new exometer values.
    timer:sleep(100),
    {ok, Data2} = receive_data(1, Socket),
    {match, _} = re:run(Data2, UpdatedValueLineRE),
    ok.



%%% ============================================================================
%%% Helper functions.
%%% ============================================================================

receive_data(PacketCount, Socket) ->
    receive_data(PacketCount, Socket, "").

receive_data(0, _Socket, Acc) ->
    {ok, Acc};

receive_data(PacketCount, Socket, Acc) ->
    receive
        {tcp, Socket, Packet} ->
            receive_data(PacketCount - 1, Socket, Acc ++ Packet)
    after
        3000 ->
            {error, timeout}
    end.


