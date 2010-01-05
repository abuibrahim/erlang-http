%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc This module implementes Erlang Server Interface (ESI).

-module(http_mod_esi).
-author('ruslan@babayev.com').

-export([init/0, handle/4]).

-include("http.hrl").

%% @doc Initializes the module.
%% @spec init() -> ok | {error, Reason}
init() ->
    ok.

%% @doc Handles the Request, Response and Flags from previous modules.
%%      Uses `scripts' environment variable.
%% @spec handle(Socket, Request, Response, Flags) -> Result
%%       Request = #http_request{}
%%       Response = #http_response{} | undefined
%%       Flags = list()
%%       Result = #http_response{} | already_sent | {error, Reason} | Proceed
%%       Proceed = {proceed, Request, Response, Flags}
handle(Socket, Request, undefined, Flags) ->
    Path = http_lib:uri_to_path(Request#http_request.uri),
    {ok, Scripts} = application:get_env(scripts),
    case match(Request, Path, Scripts) of
	{match, {Module, Function, Params}} ->
	    try Module:Function(Socket, Request, Params)
	    catch
		exit:Reason -> {error, Reason}
	    end;
	nomatch ->
	    {proceed, Request, undefined, Flags};
	{error, Reason} ->
	    {error, Reason}
    end;
handle(_Socket, Request, Response, Flags) ->
    {proceed, Request, Response, Flags}.

match(_Request, _Path, []) ->
    nomatch;
match(R, Path, [{Alias, Modules} | Rest]) ->
    case re:run(Path, "^" ++ Alias ++ "/") of
	{match, [{0, Length}]} ->
	    case string:tokens(string:substr(Path, Length + 1), "/") of
		[ModStr, FunAndInput | _] ->
		    Module = list_to_atom(ModStr),
		    case lists:member(Module, Modules) of
			true ->
			    [FStr | IStr] = string:tokens(FunAndInput, "?"),
			    Function = list_to_atom(FStr),
			    Params = parse_params(param_str(R, IStr)),
			    {match, {Module, Function, Params}};
			false ->
			    {error, not_allowed}
		    end;
		_ ->
		    nomatch
	    end;
	nomatch ->
	    match(R, Path, Rest)
    end.

param_str(#http_request{method = 'GET'}, Input) ->
    lists:flatten(Input);
param_str(#http_request{method = 'POST', body = Body, headers = Headers}, _) ->
    case proplists:get_value('Content-Type', Headers) of
	undefined ->
	    [];
	CT ->
	    case string:str(CT, "application/x-www-form-urlencoded") of
		Index when Index > 0 ->
		    binary_to_list(Body);
		_ ->
		    []
	    end
    end;
param_str(_, _) ->
    [].

parse_params(String) ->
    parse_params(string:tokens(String, "&;"), []).

parse_params([], Acc) ->
    lists:reverse(Acc);
parse_params([Field | Rest], Acc) ->
    Field2 = re:replace(Field, "[\+]", " ", [global, {return, list}]),
    case re:split(Field2, "=", [{return, list}]) of
	[Key, Value] ->
	    K = list_to_atom(http_lib:url_decode(Key)),
	    V = http_lib:url_decode(Value),
	    parse_params(Rest, [{K, V} | Acc]);
	[Key] ->
	    K = list_to_atom(http_lib:url_decode(Key)),
	    parse_params(Rest, [K | Acc]);
	_ ->
	    parse_params(Rest, Acc)
    end.
