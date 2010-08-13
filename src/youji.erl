%% @author Mickael Remond <mickael.remond@process-one.net>

%% @doc
%% The module <strong>{@module}</strong> implements a simple XMPP echo client.
%%
%% <p>
%% This is a example use of the exmpp framework.
%% </p>
%%
%% <p>
%% Usage:
%% </p>
%% <pre>{ok, session} = echo_client:start().
%% echo_client:stop(Session).</pre>

%% TODO:
%% %% %% 3) Flexible reconnection policies - Timeouts with progression for now
%% %% %% 6) Pings for conferences with reconnections - sending presence every N seconds for now

%% %% 1) Multiple JIDs support
%% %% 2) Reconnect on error
%% %% 4) Protected Conferences participation
%% %%  ) dynamic config
%% %%  ) Mute state for jids and rooms

%%-- ) JID info helpers (such as resourceByJidInfo)
%%-- 11) Flexible config with updates

%% 5) Total logging (conferences && privates)
%% 7) Internal logging system
%% ) Add all admins to roster
%% 8) Message sending, queues
%% 9) Custom commands
%% 10) Human-like resources && statuses

%% FUTURE:
%% 12) JID registration and storing registration data
%% ------------------------------------------------------

-module(youji).

-export([start/0, stop/1]).
-export([xmpp_supervisor/0, start_worker/3]).

-include("xmpp.hrl").

-record(xworker, {
          server_info,
          jid_info,
          reconnection_timeout
         }).

start_worker(ServerInfo, JidInfo, Timeout) ->
    ulog:debug("Initiating session on server:~n  ~p~nfor jid:~n  ~p) with supervisor ~p", [ServerInfo, JidInfo, self()]),
    Pid = spawn_link(worker, connect, [ServerInfo, JidInfo]),
    WorkerInfo = #xworker{server_info = ServerInfo,
                          jid_info = JidInfo,
                          reconnection_timeout = lists:max([50, Timeout])
                         },
    WorkerRecord = {Pid, WorkerInfo},
    ulog:debug("Worker with pid ~p initiated and has record~n   ~p", [Pid, WorkerRecord]),
    ets:insert(xmpp_workers, WorkerRecord).

%% ets:tab2file(xmpp_workers, "ololo"),
%%ulog:debug("~p~n", [ets:tab2list(xmpp_workers)]),

make_srv_record(SrvProps) ->
    #server_info{
                 addr = proplists:get_value(addr, SrvProps),
                 port = proplists:get_value(port, SrvProps, 5222)
                }.

make_jid_record(JidProps) ->
    #jid_info{
               internal_id = proplists:get_value(id, JidProps),
               jid = proplists:get_value(jid, JidProps),
               password = proplists:get_value(password, JidProps),
               status_text = proplists:get_value(status_text, JidProps, ""),
               resource = proplists:get_value(resource, JidProps, random),
               mute = proplists:get_value(mute, JidProps, false),
               modules = proplists:get_value(modules, JidProps, [])
             }.

xmpp_supervisor() ->
    process_flag(trap_exit, true),
    config:init("bot.cfg"),
    %% config:install_overlay(),
    config:save_overlay(),
    application:start(exmpp),
    ulog:debug("xmpp_supervisor has PID ~p", [self()]),
    ulog:debug("Working applications:  ~p", [application:which_applications()]),
    ets:new(xmpp_workers, [named_table, set]), %% bag]),
    muc_state:init(),
    Servers = config:get(servers),
    Jids = config:get(jids),
    lists:foreach(fun(JidEntry) ->
                          JidInfo = make_jid_record(JidEntry),
                          ServerNameAtom = proplists:get_value(server, JidEntry),
                          ServerInfo = make_srv_record(proplists:get_value(ServerNameAtom, Servers)),
                          start_worker(ServerInfo, JidInfo, config:get(min_reconnection_timeout))
                  end,
                  Jids
                 ),
    xmpp_supervisor_proc().

worker_by_pid(Pid) ->
    [{_, Worker}] = ets:lookup(xmpp_workers, Pid),
    Worker.

enter_rooms(InternalId, WorkerPid) ->
    AllRooms = config:get(rooms),
    Rooms = proplists:get_value(InternalId, AllRooms, []),
    lists:foreach(fun(Room) ->
                          RoomJid = proplists:get_value(jid, Room),
                          Nick = proplists:get_value(nick, Room),
                          Password = proplists:get_value(password, Room, undefined),
                          case Password of
                              undefined ->
                                  WorkerPid ! {join, RoomJid, Nick};
                              _ ->
                                  WorkerPid ! {join, RoomJid, Nick, Password}
                          end
                  end,
                  Rooms
                 ).

print_state() ->
    ulog:debug("Current MUC table:~n ~p", [ets:tab2list(muc_state)]),
    ulog:debug("Current workers table:~n ~p", [ets:tab2list(xmpp_workers)]).

remove_saved_states(Pid) ->
    ets:delete(xmpp_workers, Pid),
    muc_state:remove_state_for_pid(Pid),
    print_state().

xmpp_supervisor_proc() ->
    receive
        {'EXIT', From, Reason} ->
            Worker = worker_by_pid(From),
            MaxTimeout = config:get(max_reconnection_timeout),
            ulog:debug("Process ~p exited~n  Reason    : ~p~n  Descriptor: ~p", [From, Reason, Worker]),
            NextTimeout = Worker#xworker.reconnection_timeout * config:get(reconnection_factor),
            Timeout = lists:min([NextTimeout, MaxTimeout]),
            remove_saved_states(From),
            ulog:debug("Worker will be respawned after: ~pms", [Timeout]),
            timer:send_after(Timeout, {respawn,
                                       Worker#xworker.server_info,
                                       Worker#xworker.jid_info,
                                       Timeout
                                      }
                            );
        {connected, From} ->
            Worker = worker_by_pid(From),
            UpdatedWorker = Worker#xworker{reconnection_timeout =
                                               config:get(min_reconnection_timeout)
                                          },
            ets:insert(xmpp_workers, {From, UpdatedWorker}),
            JidInfo = UpdatedWorker#xworker.jid_info,
            InternalId = JidInfo#jid_info.internal_id,
            enter_rooms(InternalId, From),
            From ! muc_keepalive,
            ulog:debug("Connected to JID with internal_id: ~w", [InternalId]),
            ulog:debug("Worker ~p, successfully connected", [From]);
        {respawn, ServerInfo, JidInfo, Timeout} ->
            ulog:debug("Respawning worker for jid~n  ~p on server~n  ~p", [JidInfo, ServerInfo]),
            start_worker(ServerInfo, JidInfo, Timeout);
        Any ->
            ulog:debug("UNKNOWN_MESSASGE: ~p", [Any])
    end,
    xmpp_supervisor_proc().

start() ->
    %% xmpp_supervisor().
    %% spawn(bot1, xmpp_supervisor, []).
    ulog:debug("PID of main process: ~p", [self()]),
    %%spawn(?MODULE, xmpp_supervisor, []).
    register(xmpp_supervising_process, spawn(?MODULE, xmpp_supervisor, [])).

stop(EchoClientPid) ->
    EchoClientPid ! stop.

