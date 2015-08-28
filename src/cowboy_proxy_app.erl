-module(cowboy_proxy_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
            {'_', [
                    {"/hello", hello_handler, []} 
                ]}  
    ]), 

    Proxy = proxy_middleware:compile([
            {[
                "/test"
            ], {{127,0,0,1}, 8888}}
    ]), 

    %% start webserver
    {ok, _} = cowboy:start_http(proxy, 100, 
        [{max_connections, infinity}, {port, 9999}], 
        [{env, [{dispatch, Dispatch}, {proxy, Proxy}]},
        {middlewares, [proxy_middleware,cowboy_router, cowboy_handler]}]
    ),

    cowboy_proxy_sup:start_link().

stop(_State) ->
    ok.
