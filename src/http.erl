%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc HTTP Starter.

-module(http).
-author('ruslan@babayev.com').

-export([start/0, stop/0]).

%% @doc Starts the application.
%% @spec start() -> ok | {error, Reason}
start() ->
    application:start(crypto),
    application:start(ssl),
    application:start(amf),
    application:start(http).

%% @doc Stops the application.
%% @spec stop() -> ok
stop() ->
    application:stop(http).
