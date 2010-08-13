-module(config).

%% http://spawnlink.com/articles/managing-application-configuration/
%% -export([read/1, get/2]).
-export([init/1, get/1, set/2, install_overlay/0, save_overlay/0]).

get(Id) ->
    %%ets:i(),
    %%io:format("V-PID: ~p:::~p~n", [self(), ets:tab2list(app_config)]),
    [{_, Value}] = ets:lookup(app_config, Id),
    Value.

set(Id, Value) ->
    ulog:debug("Inserting into config: ~s = ~p", [Id, Value]),
    ets:insert(app_config, {Id, Value}).

init(FileName) ->
    ets:new(app_config, [named_table]),
    install(FileName).

read_and_store(FileName) ->
    {ok, Config} = read(FileName),
    store(Config).

install(FileName) ->
    FirstEntry = ets:first(app_config),
    if
        FirstEntry == '$end_of_table' ->
            read_and_store(FileName),
            ulog:info("Static config installed:~n  ~p", [ets:tab2list(app_config)]);
        true ->
            ulog:warn("Config is already installed, installing overlay"),
            install_overlay()
    end.

install_overlay() ->
    ulog:info("Installing overlay..."),
    OverlayPath = config:get(dynamic_config_path),
    read_and_store(OverlayPath).

append_termlist_to_string([], String) ->
    String;
append_termlist_to_string([H|T], String) ->
    Term = io_lib:fwrite("~p.\n",[H]),
    NewString = string:concat(String,Term),
    append_termlist_to_string(T, NewString).

save_overlay() ->
    OverlayPath = config:get(dynamic_config_path),
    ulog:info("Saving config overlay to ~s", [OverlayPath]),
    Data = ets:tab2list(app_config),
    Result = append_termlist_to_string(Data, ""),
    file:write_file(OverlayPath,Result).

store([]) ->
    done;
store([H|T]) ->
    {Id, Value} = H,
    set(Id, Value),
    store(T).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read(FileName) ->
    {ok, Terms} = file:consult(FileName),
    read_includes(Terms).

read_includes(Terms) ->
    read_includes(Terms, []).

read_includes([{include_config, File} | Terms], Acc) ->
    case file:consult(File) of
        {ok, NewTerms} ->
            read_includes(Terms ++ NewTerms, Acc);
        {error,enoent} ->
            {error, {bad_include, File}};
        Other ->
            {error, Other}
    end;
read_includes([Other | Terms], Acc) ->
    read_includes(Terms, [Other | Acc]);
read_includes([], Result) ->
    {ok, Result}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% get(_Key, []) ->
%%     {error, not_found};
%% get(Key, [{Key, Value} | _Config]) ->
%%     {ok, Value};
%% get(Key, [{_Other, _Value} | Config]) ->
%%     get(Key, Config).
