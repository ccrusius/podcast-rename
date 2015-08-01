-module(podcast_rename_handler).

-export([init/3,handle/2,terminate/3]).

-import(xmerl_scan,[string/1]).

-include_lib("xmerl/include/xmerl.hrl").

init(_Transport, Req, []) ->
    {ok,Req,undefined}.

handle(Req,State) ->
    {Path,  Req2} = cowboy_req:path(Req),
    {Query, Req3} = cowboy_req:qs_vals(Req2),
    {ok, Msg}     = handle_path(Path,Query),
    {ok, Req4}    = cowboy_req:reply(200, [], erlang:iolist_to_binary(Msg), Req3),
    {ok, Req4, State}.

terminate(_Reason,_Req,_State) -> ok.

%------------------------------------------------------------------------------
%
% handlers
%
%------------------------------------------------------------------------------
handle_path(<<"/rename">>,Query) ->
    {_,URL}    = proplists:lookup(<<"url">>,Query),
    {_,Title}  = proplists:lookup(<<"title">>,Query),
    Body       = wget(binary_to_list(URL)),
    NewBody    = process(Body,Title),
    {ok, NewBody};

handle_path(Path,Query) ->
    Info = io_lib:format(
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
        ]),
    {ok,Info}.

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
% Get the podcast feed contents.
%
%------------------------------------------------------------------------------
wget(URL) ->
    UserAgent = "curl/7.37.1",
    Headers   = [{"User-Agent",UserAgent}],
    {ok,{{_,200,"OK"},_,Body}} = httpc:request(get,{URL,Headers},[],[]),
    Body.

