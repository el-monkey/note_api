%% @doc Cowboy Rest handler for note
%% @end
-module(note_handler).
-export([init/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).
-export([note_to_json/2]).

%% @doc init
%% @end 
init(Req, State) -> 
    {cowboy_rest, Req, State}.

%% @doc allowed_methods
%% @end
allowed_methods(Req, State) -> 
    {[<<"GET">>], Req, State}.

%% @doc content_types_provided
%% @end
content_types_provided(Req, State) -> 
    {[{<<"application/json">>, note_to_json}], Req, State}.

%% @doc resource_exists
%% @end
resource_exists(Req, State) -> 
    case cowboy_req:binding(uuid, Req) of
        undefined -> 
            {true, Req, index}; % what does this do?
        Uuid      ->
            case note:find_note_using_uuid(Uuid) of
                {ok, Note}      -> {true, Req, State};
                {error, Reason} -> {false, Req, State}
            end
    end.  

%% @doc note_to_json
%% @end
note_to_json(Req, State) -> 
    Method = cowboy_req:method(Req),
    Uuid   = cowboy_req:binding(uuid, Req), 
    Req1   = note_to_json(Method, Uuid,  Req, State),
    {ok, Req1, State}.

%% 
%% PRIVATE API
%%

%% @doc note_to_json
%% @end
note_to_json(<<"GET">>, Uuid, Req, State) ->
    case note:find_note_using_uuid(Uuid) of
        {ok, Note}   ->
            Self  = erlang:iolist_to_binary(cowboy_req:uri(Req)),  
            StateMap = #{
                <<"uuid">>          => note:uuid(Note), 
                <<"created_date">>  => iso8601:format(note:created_date(Note)),
                <<"modified_date">> => iso8601:format(note:modified_date(Note)),
                <<"text">>          => note:text(Note) 
            },
            Hal        = hal:create_hal(Self, StateMap),
            StatusCode = 200, 
            Headers    = #{<<"content-type">> => <<"application/hal+json">>},
            Body       = jiffy:encode(hal:hal_to_map(Hal)), 
            cowboy_req:reply(StatusCode, Headers, Body, Req);
        {error, Msg} -> 
            % problem api
            cowboy_req:reply(404, #{}, erlang:list_to_binary(Msg), Req)
    end.

