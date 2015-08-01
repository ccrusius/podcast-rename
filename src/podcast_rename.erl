-module(podcast_rename).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(C_ACCEPTORS, 100).
%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    { ok, _ } = application:ensure_all_started(podcast_rename).

start(_StartType, _StartArgs) ->
    Routes    = routes(),
    Dispatch  = cowboy_router:compile(Routes),
    Port      = port(),
    TransOpts = [{port, Port}],
    ProtoOpts = [{env, [{dispatch, Dispatch}]}],
    {ok, _}   = cowboy:start_http(http, ?C_ACCEPTORS, TransOpts, ProtoOpts),
    podcast_rename_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================
routes() -> [{'_',[{'_',podcast_rename_handler,[]}]}].

port() ->
    case os:getenv("PORT") of
        false ->
            {ok,Port} = application:get_env(http_port),
            Port;
        Other ->
            list_to_integer(Other)
    end.

%% ===================================================================
%% Tests
%% ===================================================================
-ifdef(TEST).

get_url(URL) ->
    httpc:request(get,{URL,[]},[],[{socket_opts,[{keepalive,false}]}]).

start_app() ->
    {ok, _} = application:ensure_all_started(podcast_rename),
    ?assertNot(undefined == whereis(podcast_rename_sup)).

stop_app() ->
    % Stopping only podcast_rename does not work because httpc keeps
    % open connections and errors out randomly.
    ok = application:stop(inets),
    ok = application:stop(podcast_rename).

load_empty_page() ->
    {ok, {_,_,Body}} = get_url("http://localhost:8080"),
    {match, _} = re:run(Body,"This is the podcast renamer."),
    {match, _} = re:run(Body,"Path: */").

load_unhandled_page() ->
    {ok, {_,_,Body}} = get_url("http://localhost:8080/unhandled"),
    {match, _} = re:run(Body,"This is the podcast renamer."),
    {match, _} = re:run(Body,"Path: */unhandled").

suggested_donation() ->
    {ok, {_,_,Body}} = get_url("http://localhost:8080/rename?url=http://www.suggesteddonationpodcast.com/blog?format=rss&title=New%20Title"),
    nomatch    = re:run(Body,"podcast - Suggested Donation"),
    {match, _} = re:run(Body,"<title>New Title</title>").

unit_test_() ->
    [
        fun() -> start_app() end,
        fun() -> load_empty_page() end,
        fun() -> load_unhandled_page() end,
        fun() -> suggested_donation() end,
        fun() -> stop_app() end
    ].

-endif.
