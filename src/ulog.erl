-module(ulog).
-export([debug/1, debug/2]).
-export([info/1, info/2]).
-export([warning/1, warning/2]).

out(Level, Format, Args) ->
    Record = io_lib:format(Format, Args),
    io:format("[~w] ~s~n", [Level, Record]).

debug(Record) ->
    out(dbg, "~p", [Record]).

debug(Format, Args) ->
    out(dbg, Format, Args).

info(Record) ->
    out(inf, "~p", [Record]).

info(Format, Args) ->
    out(inf, Format, Args).

warning(Record) ->
    out(wrn, "~p", [Record]).

warning(Format, Args) ->
    out(wrn, Format, Args).


