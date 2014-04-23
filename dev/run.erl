-module(run).

-export([start/0]).

start() ->
	ok = application:start(mochiweb),
	ok.