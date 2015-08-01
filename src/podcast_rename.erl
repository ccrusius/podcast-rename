-module(podcast_rename).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1, handle/1, loop/1]).

-import(xmerl_scan,[string/1]).

-include_lib("xmerl/include/xmerl.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(C_ACCEPTORS, 100).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    spawn(?MODULE,loop,[start]),
    podcast_rename_sup:start_link().

start(_StartType, _StartArgs) ->
    start().

stop(_State) ->
    ok.

%% ===================================================================
%% Simple HTTP server
%% Similar ones all over the web. Spawns a process per request.
%% ===================================================================
loop(start) ->
    Port = port(),
    {ok,ListenSocket} = gen_tcp:listen(Port,[list,{active,false},{packet,http}]),
    loop(ListenSocket);
loop(ListenSocket) ->
    {ok,Socket} = gen_tcp:accept(ListenSocket),
    spawn(?MODULE,handle,[Socket]),
    loop(ListenSocket).

handle(Socket) ->
    {ok,{http_request,Method,Path,_Version}} = gen_tcp:recv(Socket,0),
    case Method of
        'GET' -> {abs_path,Req} = Path,
                 handle_get(Socket,Req);
        _     -> send_unsupported_error(Socket)
    end.

send_reply(Sock,Headers,Body) ->
    gen_tcp:send(Sock,[
        Headers,
        "\r\n\r\n",
        Body ]),
    gen_tcp:close(Sock).

send_unsupported_error(Sock) ->
    gen_tcp:send(Sock,
        "HTTP/1.1 405 Method Not Allowed\r\n"
        ++ "Connection: close\r\n"
        ++ "Allow: GET\r\n"
        ++ "Content-Type: text/html; charset=UTF-8\r\n"
        ++ "Cache-Control: no-cache\r\n\r\n"),
    gen_tcp:close(Sock).

port() ->
    case os:getenv("PORT") of
        false -> 8080;
        Other -> list_to_integer(Other)
    end.

%------------------------------------------------------------------------------
%
% GET processing - either output info, or rename podcast
%
%------------------------------------------------------------------------------
handle_get(Socket,Req) ->
    {Path,Query} = parse_request(Req),
    {Headers,Body} = get_reply(Path,Query),
    send_reply(Socket,Headers,Body).

get_reply("/rename",Query) ->
    {_,URL}       = proplists:lookup("url",Query),
    {_,Title}     = proplists:lookup("title",Query),
    {Header,Body} = wget(URL),
    NewBody       = process(binary_to_list(Body),Title),
    NewHeader     = remove_content_length(Header),
    {NewHeader,NewBody};

get_reply(Path,Query) ->
    {
        ["HTTP/1.1 200 OK\r\n", "Connection: close"],
        io_lib:format(
            "This is the podcast renamer.~n"
            ++ "Unhandled request.~n~n"
            ++ "Path:   ~s~n"
            ++ "Query:  ~p~n"
            ++ "Pid:    ~p~n"
            ++ "Erlang: ~s~n",
            [
                Path,
                Query,
                self(),
                erlang:system_info(otp_release)
            ])
    }.

remove_content_length(Header) ->
    [PreHdr|[Rest|_]]=binary:split(Header,[<<"\r\nContent-Length:">>]),
    [_|[PostHdr|_]]  =binary:split(Rest,[<<"\r\n">>]),
    [PreHdr,<<"\r\n">>,PostHdr].

%------------------------------------------------------------------------------
%
% Replace the title.
%
% Overkill. Parse the XML document, recurse through it, replacing the right
% <title> element with the new podcast title, translate XML back to text.
%
% SAX would be a better way to do this. Later.
%
%------------------------------------------------------------------------------
process(Body,Title) ->
    {ParsedBody,_} = xmerl_scan:string(Body),
    NewBody        = replaceTitle(ParsedBody,Title),
    ExportedBody   = xmerl:export([NewBody],xmerl_xml),
    unicode:characters_to_binary(lists:flatten(ExportedBody)).

replaceTitle(#xmlText{parents=[{title,_},{channel,_},{rss,_}]}=Element,Title) ->
    Element#xmlText{value=Title};
replaceTitle(#xmlElement{content=Content}=Element,Title) ->
    Fun = fun(E) -> replaceTitle(E,Title) end,
    Element#xmlElement{content = lists:map(Fun,Content)};
replaceTitle(Other,_Title) -> Other.

%------------------------------------------------------------------------------
%
% Utility functions
%
%------------------------------------------------------------------------------
parse_request(Req) ->
    {RelPath, Rest} = lists:splitwith(fun(C) -> C =/= $? end,Req),
    {RelPath, parse_query(Rest)}.

parse_query([ $? | QueryString ]) -> httpd:parse_query(QueryString);
parse_query(_)                    -> [].

wget(URL) ->
    {ok,{Scheme,UserInfo,Host,Port,Path,Query}}=http_uri:parse(URL,[{fragment,false}]),

    Req = lists:flatten(io_lib:format(
        "GET ~s~s HTTP/1.0\r\nUser-Agent: curl/7.37.1\r\nHost: ~s\r\nAccept: */*\r\n\r\n",
        [Path,Query,Host])),

    {ok,Socket} = gen_tcp:connect(Host,Port,[binary,{packet,raw}]),
    ok = gen_tcp:send(Socket,Req),
    {ok,Data} = receive_data(Socket,[]),
    gen_tcp:close(Socket),

    assert_ok_response(Data),
    split_response(Data).

receive_data(Socket,Acc) ->
    receive
        {tcp,Socket,Binary} -> receive_data(Socket,[Binary|Acc]);
        {tcp_closed,Socket} -> {ok, list_to_binary(lists:reverse(Acc))}
    end.

assert_ok_response(Data) ->
    {ok,{http_response,_,200,"OK"},_}=erlang:decode_packet(http,Data,[]).

split_response(Data) ->
    [Hdr|[Body|_]]=binary:split(Data,[<<"\r\n\r\n">>]),
    {Hdr,Body}.

%% ===================================================================
%% Tests
%% ===================================================================
-ifdef(TEST).

get_url(URL) ->
    httpc:request(URL).

start_app() ->
    ok = application:start(inets),
    ok = application:start(podcast_rename),
    ?assertNot(undefined == whereis(podcast_rename_sup)).

stop_app() ->
    % Stopping only podcast_rename does not work because httpc keeps
    % open connections and errors out randomly.
    %ok = application:stop(inets),
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
    {ok, {_,_,Body}} = get_url("http://localhost:8080/rename?url=http://www.suggesteddonationpodcast.com/blog?format%3Drss&title=New%20Title"),
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
