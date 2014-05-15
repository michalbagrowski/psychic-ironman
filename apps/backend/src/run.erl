-module(run).
-export([start/0]).
-export([c/0, compile/0, compile/1]).
-export([c_all/0, compile_all/0]).


start() ->
    reloader:start(),
    start_with_deps(backend),
    receive
        stop -> stop
    end.

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


c() -> compile().

compile() ->
    MainAppName = erlang:hd(string:tokens(atom_to_list(node()), "@")),
    compile(MainAppName).

compile(Atom) when is_atom(Atom) -> compile(atom_to_list(Atom));
compile(App)  when is_list(App)  ->
  cmd("sh dev/reload.sh " ++ App).

c_all() -> compile_all().

compile_all() -> cmd("make compile").


cmd(Cmd) ->
    MakeOutput = os:cmd(Cmd),
    io:format("~s", [MakeOutput]).
