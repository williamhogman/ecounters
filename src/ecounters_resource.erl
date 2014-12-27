-module(ecounters_resource).
-export([
    init/1,
    to_html/2,
    to_json/2,
    content_types_provided/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok, term()}.
init([]) ->
    {ok, undefined}.

links() -> [
	 {counters, "/counters"}
].

content_types_provided(ReqData, State) ->
    Types = [
	     {"text/html", to_html},
	     {"application/json", to_json}
    ],
    {Types, ReqData, State}.

-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.

html_link(URL, Text)->
    "<a href='" ++ URL ++ "'>" ++ Text ++ "</a>".

html_li(Wrapped)->
    "<li>" ++ Wrapped ++ "</li>".

li_list([{Text, URL}|T]) ->
    Line = html_li(html_link(URL, atom_to_list(Text))),
    [Line|li_list(T)];
li_list([]) -> [].

to_html(ReqData, State) ->
    List = li_list(links()),
    Prefix = "<html><body><ul>",
    Suffix = "</ul></body></html>",
    Data = Prefix ++ lists:flatten(List) ++ Suffix,
    {Data, ReqData, State}.



json_body(QS) -> mochijson:encode({struct, QS}).

to_json(ReqData, State) ->
    json_body(links()).
    


