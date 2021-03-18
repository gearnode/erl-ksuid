
all: dialyzer test

dialyzer:
	QUIET=1 rebar3 dialyzer

build:
	QUIET=1 rebar3 compile

test:
	QUIET=1 rebar3 eunit

cover:
	QUIET=1 rebar3 eunit --cover
	QUIET=1 rebar3 cover

clean:
	$(RM) -r _build

.PHONY: all dialyzer build test cover clean
