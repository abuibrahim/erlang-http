%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc HTTP Application.

-module(http_app).
-author('ruslan@babayev.com').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% @doc Starts the application.
%% @spec start(Type, Args) -> {ok, Pid} | {ok, Pid, State} | {error, Reason}
%%      Type = normal | {takeover, Node} | {failover, Node}
%%      Args = term()
start(_Type, _Args) ->
    http_sup:start_link().

%% @doc Stops the application.
%% @spec stop(State) -> ok
stop(_State) ->
    ok.
