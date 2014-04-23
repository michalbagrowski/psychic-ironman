-module(socket_server).

-export([start_link/0]).


start_link() ->
	error_logger:info_msg("a"),

    Options = [ {name, ?MODULE},
        {ip, "127.0.0.1"}, {port, 8081},
        {acceptor_pool_size, 10},
        {loop, {?MODULE, handle_request}} ],

    mochiweb_http:start(Options),
error_logger:info_msg("~p~n",[Options]),

	ok.


handle_request(Req) ->
	error_logger:info_msg("Request ~p~n", [Req]).
