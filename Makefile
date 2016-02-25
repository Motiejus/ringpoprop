all: compile dialyzer test

compile:
	rebar3 compile

dialyzer:
	rebar3 dialyzer

test:
	rebar3 eunit

clean:
	rebar3 clean

.PHONY: all compile dialyzer test
