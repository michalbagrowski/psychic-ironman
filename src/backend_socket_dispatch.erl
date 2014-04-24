-module(backend_socket_dispatch).

-behaviour(gen_server).

-export([start_link/0, stop/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link() ->
    % io:format("Starting with ~p~n",[Pars]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    io:format("Stopping~n"),
    gen_server:cast(?MODULE, shutdown).

init(_) ->
    % io:format("Initializing with ~p~n",[_]),
    process_flag(trap_exit, true),
    {ok, initialized}.

handle_call(Message, From, State) ->
    io:format("Generic call handler: '~p' from '~p' while in '~p'~n",[Message, From, State]),
    {reply, ok, State}.

handle_cast(shutdown, State) ->
    io:format("Generic cast handler: *shutdown* while in '~p'~n",[State]),
    {stop, normal, State};

handle_cast(Message, State) ->
    io:format("Generic cast handler: '~p' while in '~p'~n",[Message, State]),
    {noreply, State}.

handle_info(_Message, _Server) ->
    io:format("Generic info handler: '~p' '~p'~n",[_Message, _Server]),
    {noreply, _Server}.

terminate(_Reason, _Server) ->
    io:format("Generic termination handler: '~p' '~p'~n",[_Reason, _Server]).


code_change(_OldVersion, _Server, _Extra) -> {ok, _Server}.