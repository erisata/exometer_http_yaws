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
%%% Helper functions for `make rtest`.
%%%
-module(exometer_http_yaws_RTEST).
-behaviour(supervisor).
-compile([{parse_transform, lager_transform}]).
-export([start/0]).
-export([init/1]).

%%
%%  Invoked from the command line.
%%
start() ->
%%    compile_test_node(),
    start_test_node(),
    lager:debug("Test node started."),
    ok.


%%
%%
%%
start_test_node() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, {}),
    true = erlang:unlink(Pid),
    ok.


%%
%%
%%
compile_test_node() ->
%%    CompileOpts = [{outdir,"itest"}, {i, "deps/axb_core/include"}],
%%    {ok, _} = compile:file("deps/axb_core/itest/axb_itest_adapter.erl", CompileOpts),
%%    {ok, _} = compile:file("deps/axb_core/itest/axb_itest_adapter_listener.erl", CompileOpts),
%%    {ok, _} = compile:file("deps/axb_core/itest/axb_itest_adapter_sup.erl", CompileOpts),
%%    {ok, _} = compile:file("deps/axb_core/itest/axb_itest_eip_pipeline.erl", CompileOpts),
%%    {ok, _} = compile:file("deps/axb_core/itest/axb_itest_flow.erl", CompileOpts),
%%    {ok, _} = compile:file("deps/axb_core/itest/axb_itest_flows.erl", CompileOpts),
%%    {ok, _} = compile:file("deps/axb_core/itest/axb_itest_node.erl", CompileOpts),
    lager:debug("Test node compiled."),
    ok.


%%
%%  Callback for the `supervisor`.
%%
init({}) ->
    {ok, {{one_for_all, 100, 10}, [

    ]}}.


