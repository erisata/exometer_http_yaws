#/--------------------------------------------------------------------
#| Copyright 2020 Erisata, UAB (Ltd.)
#|
#| Licensed under the Apache License, Version 2.0 (the "License");
#| you may not use this file except in compliance with the License.
#| You may obtain a copy of the License at
#|
#|     http://www.apache.org/licenses/LICENSE-2.0
#|
#| Unless required by applicable law or agreed to in writing, software
#| distributed under the License is distributed on an "AS IS" BASIS,
#| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#| See the License for the specific language governing permissions and
#| limitations under the License.
#\--------------------------------------------------------------------

REBAR=rebar3
APP=exometer_http_yaws

all: compile

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

xref:
	$(REBAR) xref

check: test itest xref

test:
	$(REBAR) eunit --verbose

itest:
	$(REBAR) ct --name $(APP) $(CT_ARGS)

rtest: compile
	mkdir -p logs
	$(REBAR) as test shell --script test/$(APP)_RTEST.escript --name "$(APP)@`hostname --fqdn`"

docs:
	$(REBAR) as docs edoc

clean:
	$(REBAR) clean

clean-all:
	$(REBAR) clean --all

.PHONY: all deps compile check test itest xref docs clean clean-all


