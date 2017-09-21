%% @doc Cowboy Rest handler for collections of notes
%% @end
-module(note_collection_handler).
-export([init/2]).
-export([content_types_provided/2]).
-export([note_collection_to_json/2]).
-export([create_note/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).

%% @doc init
%% @end
init(Req, State) -> 
    {cowboy_rest, Req, State}.

%% @doc allowed_methods
%% @end
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

%% @doc content_types_provided 
%% @end
content_types_provided(Req, State) -> 
    {[{<<"application/json">>, note_collection_to_json}], Req, State}.

%% @doc content_types_accepted
%% @end
content_types_accepted(Req, State) -> 
    {[ 
      {{<<"application">>, <<"x-www-form-urlencoded">>, []}, create_note}  
     ], Req, State}.


%% @doc note_collection_to_json
%% @end
note_collection_to_json(Req, State) -> 
    Method = cowboy_req:method(Req),
    _HasBody = cowboy_req:has_body(Req),
    Req1   = note_collection_to_json(Method, Req, State),
    {ok, Req1, State}.

%%
%% PRIVATE API
%%

%% @doc GET a collection of note records
%% @doc Return a hal collection 
note_collection_to_json(<<"GET">>, Req, _State) -> 
    case note:fetch_notes() of
        {ok, Notes} -> 
            NumberOfNotes = length(Notes),
            Self          = erlang:iolist_to_binary(cowboy_req:uri(Req)),
            StateMap = #{
                <<"number_of_pages">> => erlang:integer_to_binary(1),
                <<"total">>           => erlang:integer_to_binary(NumberOfNotes)
            },  
            Hal = hal:create_hal(Self, StateMap),

            %% loop through each Note, create a Hal record for it and
            %% add it to the collection Hal record
            Fun = fun(Note) ->
                S = erlang:iolist_to_binary(cowboy_req:uri(
                    Req, 
                    #{path => erlang:list_to_binary([<<"/notes/">>, note:uuid(Note)])}
                )), 
                hal:create_hal(
                    S,
                    #{
                        <<"uuid">>          => note:uuid(Note),
                        <<"text">>          => note:text(Note), 
                        <<"created_date">>  => iso8601:format(note:created_date(Note)),             
                        <<"modified_date">> => iso8601:format(note:modified_date(Note))             
                    }
                ) 
            end,
            EmbeddedNoteHals = lists:map(Fun, Notes),             
            Hal2 = hal:add_embedded(Hal, <<"note">>, EmbeddedNoteHals), 
            StatusCode = 200,
            Headers = #{<<"content-type">> => <<"application/hal+json">>},
            Body = jiffy:encode(hal:hal_to_map(Hal2)),
            cowboy_req:reply(StatusCode, Headers, Body, Req);
        {error, Msg} -> 
            cowboy_req:reply(404, #{}, erlang:list_to_binary(Msg), Req)
    end.

%% @doc POST a note to the collection
%% @link https://ninenines.eu/docs/en/cowboy/2.0/guide/req_body/
%% @end
create_note(Req, _State) -> 
    HasBody = cowboy_req:has_body(Req),
    if HasBody =:= false -> 
            % no body
            cowboy_req:reply(400, #{}, <<"Missing Body">>, Req);
        true ->
            {ok, [{<<"text">>, Text}], Req1} = cowboy_req:read_urlencoded_body(Req),
            {ok, Note}      = note:create_note(Text),
            {ok, SavedNote} = note:save_note(Note),
            cowboy_req:reply(200, #{<<"content-type">> => <<"application/text">>}, Text, Req1)
    end.
