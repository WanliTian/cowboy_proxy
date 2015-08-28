-module(proxy_middleware).
-behaviour(cowboy_middleware).

-export([
        compile/1,
        execute/2
]).

compile(List) when is_list(List) ->
    compile_inner(List, []).

execute(Req, Env) ->
    case lists:keyfind(proxy, 1, Env) of 
        fase ->
            {ok, Req, Env};
        {_, ProxyRules} ->
            Path = cowboy_req:path(Req),
            case match(ProxyRules, Path) of 
                false ->
                    {ok, Req, Env};
                Destination ->
                    connect(Req, Env, Destination),
                    {stop, set_done_state(Req)}
            end
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Inner Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(stproxy, {
        listener :: ranch:ref(),
        socket :: inet:socket(),
        transport :: module(),
        handler :: {module(), any()} | {module(), any(), any()},
        timeout :: timeout(),
        buffer = <<>> :: binary(),
        remote :: any(),
        remote_socket :: inet:socket(),
        remote_transport :: module()
    }).

connect(Req, Env, Dest) ->
    {_, Listener}       = lists:keyfind(listener, 1, Env),
    %%ok                  = ranch:remove_connection(Listener),
    [Socket, Transport] = cowboy_req:get([socket, transport], Req),
    Buffer              = get_raw_body(Req),
    wait_request(#stproxy{
        socket    = Socket,
        transport = Transport,
        listener  = Listener,
        timeout   = 5000,
        buffer    = Buffer,
        remote    = Dest
    }).

wait_request(State=#stproxy{socket=Socket, transport=Transport, timeout=T,
        remote=Remote, buffer=Buffer}) ->
    start_proxy_loop(State#stproxy{buffer=Buffer, remote=Remote}).
%%wait_request(State=#stproxy{socket=Socket, transport=Transport, timeout=T,
%%        remote=Remote, buffer=Buffer}) ->
%%    case Transport:recv(Socket, 0, T) of
%%        {ok, Data} ->
%%            Buffer1 = << Buffer/binary, Data/binary >>,
%%            start_proxy_loop(State#stproxy{buffer=Buffer1, remote=Remote});
%%        {error, _Reason} ->
%%            terminate(State);
%%        Other ->
%%            start_proxy_loop(State#stproxy{buffer=Buffer, remote=Remote})
%%    end.

start_proxy_loop(State=#stproxy{remote=Remote, buffer=Buffer}) ->
    case remote_connect(Remote) of
        {Transport, {ok, Socket}} ->
            Transport:send(Socket, Buffer),
            proxy_loop(State#stproxy{remote_socket=Socket,
                    remote_transport=Transport, buffer= <<>> });
        {error, _Error} ->
            terminate(State)
    end.

proxy_loop(State=#stproxy{socket=From, transport=TFrom,
        remote_socket=To, remote_transport=TTo}) ->
    TFrom:setopts(From, [{packet, 0}, {active, once}]),
    TTo:setopts(To, [{packet, 0}, {active, once}]),

    receive
        {_, From, Data} ->
            TTo:send(To, Data),
            proxy_loop(State);
        {_, To, Data} ->
            TFrom:send(From, Data),
            proxy_loop(State);
        {tcp_closed, To} ->
            terminate(State);
        {tcp_closed, From} ->
            remote_terminate(State);
        _ ->
            terminate_all(State)
    end.

remote_connect({Ip, Port}) ->
    {ranch_tcp, gen_tcp:connect(Ip, Port, [binary,
                {packet, 0}, {delay_send, true}])}.

remote_terminate(#stproxy{remote_socket=Socket,
        remote_transport=Transport}) ->
    Transport:close(Socket),
    ok.

terminate(_) ->
    ok.

terminate_all(State) ->
    remote_terminate(State),
    terminate(State).

compile_inner([], Result) ->
    lists:reverse(Result);
compile_inner([{Paths, Destination} | Tail], Result) ->
    PathCompiled = compile_paths(Paths, []),
    compile_inner(Tail, [{PathCompiled, Destination} | Result] ).

compile_paths([], Acc) ->
    lists:reverse(Acc);
compile_paths([PathMatch|Tail], Acc) when is_binary(PathMatch); is_list(PathMatch)->
    compile_paths([{PathMatch, []}|Tail], Acc);
compile_paths([{PathMatch, Fields}|Tail], Acc) when is_list(PathMatch) ->
    compile_paths([{iolist_to_binary(PathMatch),Fields}|Tail], Acc);
compile_paths([{'_', Fields }|Tail], Acc) ->
    compile_paths(Tail, [{'_', Fields}] ++ Acc);
compile_paths([{<< $/, PathMatch/bits >>, Fields}|Tail], Acc) ->
    PathRules = compile_rules(PathMatch, $/, [], [], <<>>),
    Paths = [{lists:reverse(R), Fields} || R <- PathRules],
    compile_paths(Tail, Paths ++ Acc);
compile_paths([{PathMatch, _}|_], _) ->
    error({badarg, "The following route MUST begin with a slash: "
        ++ binary_to_list(PathMatch)}).


compile_rules(<<>>, _, Segments, Rules, <<>>) ->
    [Segments|Rules];
compile_rules(<<>>, _, Segments, Rules, Acc) ->
    [[Acc|Segments]|Rules];
compile_rules(<< S, Rest/bits >>, S, Segments, Rules, <<>>) ->
    compile_rules(Rest, S, Segments, Rules, <<>>);
compile_rules(<< S, Rest/bits >>, S, Segments, Rules, Acc) ->
    compile_rules(Rest, S, [Acc|Segments], Rules, <<>>);
compile_rules(<< $:, Rest/bits >>, S, Segments, Rules, <<>>) ->
    {NameBin, Rest2} = compile_binding(Rest, S, <<>>),
    Name = binary_to_atom(NameBin, utf8),
    compile_rules(Rest2, S, Segments, Rules, Name);
compile_rules(<< $:, _/bits >>, _, _, _, _) ->
    error(badarg);
compile_rules(<< $[, $., $., $., $], Rest/bits >>, S, Segments, Rules, Acc)
        when Acc =:= <<>> ->
    compile_rules(Rest, S, ['...'|Segments], Rules, Acc);
compile_rules(<< $[, $., $., $., $], Rest/bits >>, S, Segments, Rules, Acc) ->
    compile_rules(Rest, S, ['...', Acc|Segments], Rules, Acc);
compile_rules(<< $[, S, Rest/bits >>, S, Segments, Rules, Acc) ->
    compile_brackets(Rest, S, [Acc|Segments], Rules);
compile_rules(<< $[, Rest/bits >>, S, Segments, Rules, <<>>) ->
    compile_brackets(Rest, S, Segments, Rules);
%% Open bracket in the middle of a segment.
compile_rules(<< $[, _/bits >>, _, _, _, _) ->
    error(badarg);
%% Missing an open bracket.
compile_rules(<< $], _/bits >>, _, _, _, _) ->
    error(badarg);
compile_rules(<< C, Rest/bits >>, S, Segments, Rules, Acc) ->
    compile_rules(Rest, S, Segments, Rules, << Acc/binary, C >>).

%% Everything past $: until the segment separator ($. for hosts,
%% $/ for paths) or $[ or $] or end of binary is the binding name.
compile_binding(<<>>, _, <<>>) ->
    error(badarg);
compile_binding(Rest = <<>>, _, Acc) ->
    {Acc, Rest};
compile_binding(Rest = << C, _/bits >>, S, Acc)
        when C =:= S; C =:= $[; C =:= $] ->
    {Acc, Rest};
compile_binding(<< C, Rest/bits >>, S, Acc) ->
    compile_binding(Rest, S, << Acc/binary, C >>).

compile_brackets(Rest, S, Segments, Rules) ->
    {Bracket, Rest2} = compile_brackets_split(Rest, <<>>, 0),
    Rules1 = compile_rules(Rest2, S, Segments, [], <<>>),
    Rules2 = compile_rules(<< Bracket/binary, Rest2/binary >>,
        S, Segments, [], <<>>),
    Rules ++ Rules2 ++ Rules1.

%% Missing a close bracket.
compile_brackets_split(<<>>, _, _) ->
    error(badarg);
%% Make sure we don't confuse the closing bracket we're looking for.
compile_brackets_split(<< C, Rest/bits >>, Acc, N) when C =:= $[ ->
    compile_brackets_split(Rest, << Acc/binary, C >>, N + 1);
compile_brackets_split(<< C, Rest/bits >>, Acc, N) when C =:= $], N > 0 ->
    compile_brackets_split(Rest, << Acc/binary, C >>, N - 1);
%% That's the right one.
compile_brackets_split(<< $], Rest/bits >>, Acc, 0) ->
    {Acc, Rest};
compile_brackets_split(<< C, Rest/bits >>, Acc, N) ->
    compile_brackets_split(Rest, << Acc/binary, C >>, N).

match([], Path) ->
    false;
match([{ProxyRule, Destination} | Tail], Path) ->
    case match_path(ProxyRule, Path) of
        true ->
            Destination;
        false ->
            match(Tail, Path)
    end.

match_path([], _) ->
    false;
%% If the path is '_' then there can be no constraints.
match_path([{'_', []}|_Tail], _) ->
    true;
match_path([{<<"*">>, _}|_Tail], <<"*">>) ->
    true;
match_path([{PathMatch, _Fields}|Tail], Tokens) when is_list(Tokens) ->
    case list_match(Tokens, PathMatch, []) of
        false ->
            match_path(Tail, Tokens);
        {true, _, _} ->
            true
    end;
match_path(_Dispatch, badrequest) ->
    {error, badrequest, path};
match_path(Dispatch, Path) ->
    match_path(Dispatch, split_path(Path)).

split_path(<< $/, Path/bits >>) ->
    split_path(Path, []);
split_path(_) ->
    badrequest.

split_path(Path, Acc) ->
    try
        case binary:match(Path, <<"/">>) of
            nomatch when Path =:= <<>> ->
                lists:reverse([cow_qs:urldecode(S) || S <- Acc]);
            nomatch ->
                lists:reverse([cow_qs:urldecode(S) || S <- [Path|Acc]]);
            {Pos, _} ->
                << Segment:Pos/binary, _:8, Rest/bits >> = Path,
                split_path(Rest, [Segment|Acc])
        end
    catch
        error:badarg ->
            badrequest
    end.

%% Atom '...' matches any trailing path, stop right now.
list_match(List, ['...'], Binds) ->
    {true, Binds, List};
%% Atom '_' matches anything, continue.
list_match([_E|Tail], ['_'|TailMatch], Binds) ->
    list_match(Tail, TailMatch, Binds);
%% Both values match, continue.
list_match([E|Tail], [E|TailMatch], Binds) ->
    list_match(Tail, TailMatch, Binds);
%% Bind E to the variable name V and continue,
%% unless V was already defined and E isn't identical to the previous value.
list_match([E|Tail], [V|TailMatch], Binds) when is_atom(V) ->
    case lists:keyfind(V, 1, Binds) of
        {_, E} ->
            list_match(Tail, TailMatch, Binds);
        {_, _} ->
            false;
        false ->
            list_match(Tail, TailMatch, [{V, E}|Binds])
    end;
%% Match complete.
list_match([], [], Binds) ->
    {true, Binds, undefined};
%% Values don't match, stop.
list_match(_List, _Match, _Binds) ->
    false.

get_raw_body(Req) ->
    Method = cowboy_req:method(Req),
    {ok, Body, _} = cowboy_req:body(Req),
    Path = cowboy_req:path(Req),
    Version = cowboy_req:version(Req),
    Headers = cowboy_req:headers(Req),

    ReqHeader = list_to_binary(cow_http:request(Method, Path, Version, Headers)),
    <<ReqHeader/binary, Body/binary >>.

set_done_state(Req) ->
    cowboy_req:set([{resp_state, done}, {connection, close}], Req).
