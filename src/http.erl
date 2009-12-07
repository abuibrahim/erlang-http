-module(http).
-author('ruslan@babayev.com').
-export([start/0, stop/0]).

start() ->
    application:start(crypto),
    application:start(ssl),
    application:start(amf),
    application:start(http).

stop() ->
    application:stop(http).
