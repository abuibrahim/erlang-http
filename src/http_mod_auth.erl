%%%-------------------------------------------------------------------
%%% @author Ruslan Babayev <ruslan@babayev.com>
%%% @copyright 2009, Ruslan Babayev
%%% @doc This module provides basic user authentication using text files.
%%% @end
%%%-------------------------------------------------------------------
-module(http_mod_auth).
-author('ruslan@babayev.com').
-export([init/0, handle/4]).

-include("http.hrl").

-record(http_user, {name, password}).
-record(http_group, {name, users}).

%% @spec () -> ok | error
%% @doc Returns `ok' if the module initialization succeeds, `error' otherwize.
-spec init() -> ok | error.

init() ->
    ok.

%% @spec handle(Socket, Request, Response, Flags) ->
%%           {proceed, NewResponse, NewFlags} |
%%           {skip_to, Module, NewResponse, NewFlags} |
%%           Response |
%%           already_sent |
%%           {error, closed} |
%%           {error, Reason}
%% where
%%       Socket = http_socket()
%%       Request = #http_request{}
%%       Response = #http_response{}
%%       Flags = proplist()
%%       NewResponse = #http_response{}
%%       NewFlags = proplist()
%%       Module = atom()
%%       Reason = string()
%%
%% @type http_socket() = port() | #sslsocket{}
%% @type proplist() = [atom() | tuple()]
%%
%% @doc The entry point to the module.
%%
%% Returns one of the following:
%% <dl>
%%  <dt>`{proceed, Response, Flags}'
%%  </dt>
%%  <dd>Pass the `Response' and the `Flags' to the next module in
%%      the `modules' list. If this was the last module, send the `Response'
%%      to the client socket.
%%  </dd>
%%  <dt>`{skip_to, Module, Response, Flags}'
%%  </dt>
%%  <dd>Skip to the specified `Module'. If `Module' is not found, send the
%%      `Response' to the client socket.
%%  </dd>
%%  <dt>`#http_response{}'
%%  </dt>
%%  <dd>Modules traversal is complete. Response will be sent to the socket.
%%  </dd>
%%  <dt>`already_sent'
%%  </dt>
%%  <dd>Modules traversal is complete. The response has already been
%%      sent to the client socket.
%%  </dd>
%%  <dt>`{error, closed}'
%%  </dt>
%%  <dd>Client socket was closed. Exit the handler.
%%  </dd>
%%  <dt>`{error, Reason::string()}'
%%  </dt>
%%  <dd>Internal Server Error (500) will be sent to the client.
%%      The error Reason will be logged.
%%  </dd>
%% </dl>
-spec handle(http_socket(), http_request(), http_response(), proplist()) ->
    {proceed, http_response(), proplist()} |
    {skip_to, atom(), http_response(), proplist()} |
    http_response() |
    already_sent |
    {error, string()}.

handle(Socket, Request, Response, Flags) ->
    Path = proplists:get_value(path, Flags),
    {ok, Directories} = application:get_env(directories),
    SortedUniqueDirectories = lists:ukeysort(1, Directories),
    case path_match(Path, SortedUniqueDirectories) of
	{match, {_DirPath, DirOpts}} ->
	    case http_lib:peername(Socket) of
		{ok, {Address, _Port}} ->
		    Allow = proplists:get_value(allow, DirOpts, all),
		    Deny = proplists:get_value(deny, DirOpts, none),
		    case is_allowed(Address, Allow, Deny) of
			true ->
			    auth(Request, Response, DirOpts, Flags);
			false ->
			    http_lib:response(403)
		    end;
		{error, _Reason} ->
		    http_lib:response(403)
	    end;
	nomatch ->
	    {proceed, Response, Flags}
    end.

path_match(Path, Directories) ->
    path_match(Path, Directories, {undefined, undefined}).

path_match(_Path, [], {undefined, undefined}) ->
    nomatch;
path_match(_Path, [], {Path, Opts}) ->
    {match, {Path, Opts}};
path_match(Path, [{Path1, Opts1} | Rest], {Path0, Opts0}) ->
    case re:run(Path, Path1) of
	{match, _} when Path0 == undefined ->
	    path_match(Path, Rest, {Path1, Opts1});
	{match, [{_, Length}]} when Length > length(Path0) ->
	    path_match(Path, Rest, {Path1, Opts1});
	{match, _} ->
	    path_match(Path, Rest, {Path0, Opts0});
	nomatch ->
	    path_match(Path, Rest, {Path0, Opts0})
    end.

is_allowed(Address, AllowedList, DeniedList) ->
    matches(Address, AllowedList) and not matches(Address, DeniedList).

matches(_, all) ->
    true;
matches(_, none) ->
    false;
matches(_, []) ->
    false;
matches(Address, [Pattern | Rest]) ->
    case matches2(Address, Pattern) of
	true ->
	    true;
	false ->
	    matches(Address, Rest)
    end.

matches2(Tuple, Pattern) when size(Tuple) == size(Pattern) ->
    Match = fun({X, X}) -> true;
	       ({_, '_'}) -> true;
	       ({_, _}) -> false
	    end,
    Zipped = lists:zip(tuple_to_list(Tuple), tuple_to_list(Pattern)),
    lists:all(Match, Zipped);
matches2(_, _) ->
    false.

auth(Request, Response, DirOpts, Flags) ->
    ValidUsers = proplists:get_value(require_user, DirOpts, []),
    ValidGroups = proplists:get_value(require_group, DirOpts, []),
    case ValidGroups of
	undefined when ValidUsers == undefined ->
	    {proceed, Response, Flags};
	_ ->
	    Headers = Request#http_request.headers,
	    case proplists:get_value('Authorization', Headers) of
		undefined ->
		    authorization_required(DirOpts);
		"Basic " ++ Encoded ->
		    try base64:decode_to_string(Encoded) of
			Decoded ->
			    validate_user(Response, Flags, DirOpts,
					  ValidUsers, ValidGroups, Decoded)
		    catch
			exit:_ -> http_lib:response(401)
		    end;
		_ ->
		    %%TODO: more auth methods to be handled here
		    http_lib:response(401)
	    end
    end.

authorization_required(DirOpts) ->
    case proplists:get_value(auth_name, DirOpts) of
	undefined ->
	    http_lib:response(500);
	Realm ->
	    www_authenticate_response(Realm)
    end.

www_authenticate_response(Realm) ->
    Body = http_lib:reason_phrase(401),
    Headers = [{'Content-Type', "text/plain"},
	       {'Content-Length', length(Body)},
	       {'WWW-Authenticate', "Basic realm=\"" ++ Realm ++ "\""}],
    #http_response{status = 401, headers = Headers, body = Body}.

validate_user(Response, Flags, DirOpts, ValidUsers, ValidGroups, Decoded) ->
    try re:split(Decoded, ":", [{parts, 2}, {return, list}]) of
	[UserName, Password] ->
 	    case lists:member(UserName, ValidUsers) of
 		true ->
 		    case is_valid_user(UserName, Password, DirOpts) of
 			true ->
 			    {proceed, Response, Flags};
 			false ->
 			    authorization_required(DirOpts)
 		    end;
 		false ->
 		    case group_accepted(UserName, ValidGroups, DirOpts) of
 			true ->
 			    case is_valid_user(UserName, Password, DirOpts) of
				true ->
				    {proceed, Response, Flags};
				false ->
				    authorization_required(DirOpts)
			    end;
 			false ->
 			    authorization_required(DirOpts)
 		    end
 	    end
    catch
	error:badarg ->
	    http_lib:response(401)
    end.

group_accepted(_UserName, [], _DirOpts) ->
    false;
group_accepted(UserName, [Group | Rest], DirOpts) ->
    case get_group_members(DirOpts, Group) of
	{ok, Users} ->
	    case lists:member(UserName, Users) of
		true ->
		    true;
		false ->
		    group_accepted(UserName, Rest, DirOpts)
	    end;
	_ ->
	    false
    end.

get_group_members(DirOpts, Group) ->
    case proplists:get_value(auth_group_file, DirOpts) of
	undefined ->
	    {error, "Missing auth_group_file"};
	AuthGroupFile ->
	    case file:consult(AuthGroupFile) of
		{ok, Terms} ->
		    case lists:keysearch(Group, #http_group.name, Terms) of
			{value, #http_group{users = Users}} ->
			    {ok, Users};
			false ->
			    {error, "Unknown group"}
		    end;
		Else ->
		    Else
	    end
    end.

is_valid_user(UserName, Password, DirOpts) ->
    case usersearch(DirOpts, UserName) of
	{value, #http_user{password = Password}} ->
	    true;
	{value, _} ->
	    error_logger:warning_msg("Password mismatch"),
	    false;
	false ->
	    error_logger:warning_msg("User not found"),
	    false;
	{error, Reason} ->
	    error_logger:warning_msg(Reason),
	    false
    end.

usersearch(DirOpts, UserName) ->
    case get_users(DirOpts) of
	{ok, Users} ->
	    lists:keysearch(UserName, #http_user.name, Users);
	Else ->
	    Else
    end.

get_users(DirOpts) ->
    case proplists:get_value(auth_user_file, DirOpts) of
	undefined ->
	    {error, "Missing auth_user_file"};
	AuthUserFile ->
	    case file:consult(AuthUserFile) of
		{ok, Terms} ->
		    {ok, proplists:lookup_all(http_user, Terms)};
		Else ->
		    Else
	    end
    end.
