-module(run).
-export([start/0]).

start() ->
	start_with_deps(backend),
	ok.

start_with_deps(App) ->
  case application:start(App) of
    {error,{not_started,Deps}} ->
		error_logger:info_msg("Starting deps: ~p for application: ~p~n", [Deps, App]),

        start_with_deps(Deps),
		start_with_deps(App);
    ok ->
		error_logger:info_msg("Starting  application: ~p~n",  [App]),
		ok
  end.