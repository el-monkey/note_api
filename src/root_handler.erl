-module(root_handler).

-export([init/2]).



init(Req, State) -> 
    %% request propertes
    Method   = cowboy_req:method(Req),
    Version  = cowboy_req:version(Req),
    Scheme   = cowboy_req:scheme(Req),
    Host     = cowboy_req:host(Req),
    Port     = cowboy_req:port(Req),
    Path     = cowboy_req:path(Req),
    Qs       = cowboy_req:qs(Req),
    Uri      = cowboy_req:uri(Req),
    HostInfo = cowboy_req:host_info(Req),
    PathInfo = cowboy_req:path_info(Req),
    Peer     = cowboy_req:peer(Req),
    ReqUri   = cowboy_req:uri(Req),

    Req0 = cowboy_req:reply(
        200, 
        #{<<"content-type">> => <<"text/plain">>},
        ["method:   ", Method, "\r\n", 
         "version:  ", erlang:atom_to_binary(Version, utf8), "\r\n",
         "scheme:   ", Scheme, "\r\n",
         "host:     ", Host, "\r\n",
         "port:     ", erlang:integer_to_binary(Port), "\r\n",
         "path:     ", Path, "\r\n",
         "qs:       ", Qs, "\r\n" 
         "uri:      ", Uri, "\r\n",
         % "host_info:", HostInfo, "\r\n",
         % "path_info:", PathInfo, "\r\n",
         % "Peer:     ", Peer, "\r\n",
         "ReqUri:   ", ReqUri, "\r\n" 
        ], Req
    ),
    {ok, Req0, State}.


