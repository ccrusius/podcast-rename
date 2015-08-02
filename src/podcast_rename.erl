-module(podcast_rename).

-behaviour(application).

-export([start/0, start/2, stop/1, handle/1, loop/1]).

-import(xmerl_scan,[string/1]).

-include_lib("xmerl/include/xmerl.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(C_ACCEPTORS, 100).

%% =============================================================================
%% Application callbacks
%% =============================================================================

start() ->
    spawn(?MODULE,loop,[start]),
    podcast_rename_sup:start_link().

start(_StartType, _StartArgs) ->
    start().

stop(_State) ->
    ok.

%% =============================================================================
%% Simple HTTP server
%% Similar ones all over the web. Spawns a process per request.
%% =============================================================================
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
    send_reply(Sock,
               ["HTTP/1.1 405 Method Not Allowed\r\n"
                ++ "Connection: close\r\n"
                ++ "Allow: GET\r\n"
                ++ "Content-Type: text/html; charset=UTF-8\r\n"
                ++ "Cache-Control: no-cache"],
               []).

port() ->
    case os:getenv("PORT") of
        false -> 8080;
        Other -> list_to_integer(Other)
    end.

%% =============================================================================
%% GET processing - either output info, or rename podcast
%% =============================================================================
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
      [
       "This is the podcast renamer.\n",
       "Unhandled request.\n",
       "\nPath:   ",Path,
       "\nQuery:  ",io_lib:format("~p",[Query]),
       "\nPid:    ",io_lib:format("~p",[self()]),
       "\nErlang: ",erlang:system_info(otp_release)
      ]
    }.

remove_content_length(Header) ->
    case binary:match(Header,<<"\r\nContent-Length:">>) of
        nomatch -> Header;
        _ -> [PreHdr,Rest]=binary:split(Header,[<<"\r\nContent-Length:">>]),
             [_,PostHdr]  =binary:split(Rest,[<<"\r\n">>]),
             [PreHdr,<<"\r\n">>,PostHdr]
    end.

%% =============================================================================
%% Replace the title.
%%
%% Overkill. Parse the XML document, recurse through it, replacing the right
%% <title> element with the new podcast title, translate XML back to text.
%%
%% SAX would be a better way to do this. Later.
%% =============================================================================
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

%% =============================================================================
%% Utility functions
%% =============================================================================
parse_request(Req) ->
    {RelPath, Rest} = lists:splitwith(fun(C) -> C =/= $? end,Req),
    {RelPath, parse_query(Rest)}.

parse_query([ $? | QueryString ]) -> httpd:parse_query(QueryString);
parse_query(_)                    -> [].

wget(URL) ->
    {ok,{_Scheme,_UserInfo,Host,Port,Path,Query}}=http_uri:parse(URL,[{fragment,false}]),

    Req = [
           "GET ",Path,Query," HTTP/1.0\r\n",
           "User-Agent: curl/7.37.1\r\n",
           "Host: ",Host,"\r\n",
           "Accept: */*\r\n\r\n"
          ],
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
    [Hdr,Body]=binary:split(Data,[<<"\r\n\r\n">>]),
    {Hdr,Body}.

%% =============================================================================
%% Tests
%% =============================================================================
-ifdef(TEST).

start_app() ->
    ok = application:start(podcast_rename),
    ?assertNot(undefined == whereis(podcast_rename_sup)).

stop_app() ->
    ok = application:stop(podcast_rename).

load_empty_page() ->
    {_Header,Body} = wget("http://localhost:8080"),
    ?assertNot(nomatch == binary:match(Body,<<"This is the podcast renamer.">>)),
    ?assertNot(nomatch == binary:match(Body,<<"Path:   /">>)).

load_unhandled_page() ->
    {_Header,Body} = wget("http://localhost:8080/unhandled"),
    ?assertNot(nomatch == binary:match(Body,<<"This is the podcast renamer.">>)),
    ?assertNot(nomatch == binary:match(Body,<<"Path:   /unhandled">>)).

test_url(URL,Title) ->
    {_Header,Body} = wget("http://localhost:8080/rename"
                          ++"?url="++http_uri:encode(URL)
                          ++"&title="++http_uri:encode(Title)),
    {Doc,_}  = xmerl_scan:string(binary_to_list(Body)),
    [#xmlElement{content=[#xmlText{value=Title}]}] = xmerl_xpath:string("/rss/channel/title",Doc).

unit_test_() ->
    [
     fun() -> start_app() end,
     fun() -> load_empty_page() end,
     fun() -> load_unhandled_page() end,
     fun() -> test_url(
                "http://www.suggesteddonationpodcast.com/blog?format=rss",
                "New Title") end,
     fun() -> test_url(
                "http://feeds.feedburner.com/dancarlin/history?format=xml",
                "New Title") end,
     fun() -> stop_app() end
    ].

-endif.
