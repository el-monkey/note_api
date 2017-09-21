%%%-------------------------------------------------------------------
%% @doc note_api public API
%% @end
%%%-------------------------------------------------------------------

-module(note_api_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {
         '_',  % any host
         [
          %% root 
          {
           "/",          % PathMatch
           root_handler, % Handler
           []            % Initial State
          },
          %% note collection 
          { 
           "/notes",                 % PathMatch
            note_collection_handler, % Handler
            []                       % Initial state  
          },
          %% note
          {
           "/notes/:uuid", % PathMatch
           note_handler,   % Handler
           []              % Initial State
          } 
         ]
        }
        %% Routes   = [Host]
        %% Host     = {HostMath, PathList}
        %% PathList = [Path]
        %% Path     = {PathMatch, Hanlder, InitialState}  
        %% {'_', [{"/", note_handler, []}]},
        
    ]),
    {ok, _} = cowboy:start_clear(http, 
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    note_api_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
