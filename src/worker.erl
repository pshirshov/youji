-module(worker).

-include_lib("exmpp/include/exmpp_client.hrl").
-include("xmpp.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public
-export([connect/2]).

connect(ServerInfo, JidInfo) ->
    %%    link(xmpp_supervisor_pid),
    %% Start XMPP session: Needed to start service (Like
    %% exmpp_stringprep):
    XmppSession = exmpp_session:start(),
    %% Create XMPP ID (Session Key):
    JidParts = split_jid(JidInfo#jid_info.jid),
    XmppJid = exmpp_jid:make(lists:nth(1, JidParts),
                             lists:nth(2, JidParts),
                             JidInfo#jid_info.resource),
    %% Create a new session with basic (digest) authentication:
    exmpp_session:auth_basic_digest(XmppSession, XmppJid,
                                    JidInfo#jid_info.password),
    %% Connect in standard TCP:
    {ok, _StreamId} = exmpp_session:connect_TCP(XmppSession,
                                                ServerInfo#server_info.addr,
                                                ServerInfo#server_info.port),
    session(XmppSession, XmppJid, ServerInfo, JidInfo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
split_jid(Jid) ->
    string:tokens(Jid, "@").

%% We are connected. We now log in (and try registering if authentication fails)
session(XmppSession, _XmppJid, ServerInfo, JidInfo) ->
    %% Login with defined JID / Authentication:
    try exmpp_session:login(XmppSession)
    catch
        throw:{auth_error, 'not-authorized'} ->
            %% Try creating a new user:
            ulog:debug("Register"),
            %% In a real life client, we should trap error case here
            %% and print the correct message.
            exmpp_session:register_account(XmppSession, "password"),
            %% After registration, retry to login:
            exmpp_session:login(XmppSession)
    end,
    %% We explicitely send presence:
    exmpp_session:send_packet(XmppSession,
                              exmpp_presence:set_status(
                                exmpp_presence:available(), JidInfo#jid_info.status_text
                               )
                             ),
    xmpp_supervising_process ! {connected, self()},
    loop(XmppSession, ServerInfo, JidInfo).

join_groupchat(XmppSession, Room, Nick) ->
    ulog:info("Joining ~s as ~s", [Room, Nick]),
    Presence = muc_tools:create_open_room_presence(Room, Nick),
    muc_state:remember_conference_status(Room, Nick, entering),
    exmpp_session:send_packet(XmppSession, Presence).

join_groupchat(XmppSession, Room, Nick, Password) ->
    ulog:info("Joining ~s as ~s with pw ~s", [Room, Nick, Password]),
    Presence = muc_tools:create_protected_room_presence(Room, Nick, Password),
    muc_state:remember_conference_status(Room, Nick, entering),
    exmpp_session:send_packet(XmppSession, Presence).

process_muc_keepalive(XmppSession, JidInfo) ->
    ulog:debug("Processing muc keepalive in ~p", [self()]),
    %% TODO: rejoin all
    CurrentMucs = muc_state:current_states(),
    lists:foreach(fun(Muc) ->
                          {{_, Jid}, Props} = Muc,
                          Nick = proplists:get_value(nick, Props),
                          RoomInfo = muc_state:roominfo_by_jid(JidInfo, Jid),
                          muc_tools:rejoin_using_info(RoomInfo, Nick, 0)
                  end,
                  CurrentMucs),
    ulog:debug("MUCs: ~p", [CurrentMucs]),
    ulog:debug("Current MUC state:~n ~p", [ets:tab2list(muc_state)]).

%% Process exmpp packet:
loop(XmppSession, ServerInfo, JidInfo) ->
    receive
        stop ->
            exmpp_session:stop(XmppSession),
            exit(normal);
        muc_keepalive ->
            process_muc_keepalive(XmppSession, JidInfo),
            MucKeepalivePause = config:get(muc_keepalive_rejoin_timeout),
            timer:send_after(MucKeepalivePause, muc_keepalive);
        %% TODO: save join data into temporary config
        {join, Room, Nick} ->
            join_groupchat(XmppSession, Room, Nick);
        %%    timer:send_after(MucKeepalivePause, {join, Room, Nick});
        {join, Room, Nick, Password} ->
            join_groupchat(XmppSession, Room, Nick, Password);
        {send_packet, Packet}
          when ?IS_MESSAGE(Packet) ->
            ulog:debug(Packet),
            JidMuteState = JidInfo#jid_info.mute,
            if
                JidMuteState /= true ->
                    MessageType = exmpp_message:get_type(Packet),
                    if
                        MessageType == groupchat ->
                            From = exmpp_xml:get_attribute(Packet, from, <<"unknown">>),
                            To = exmpp_xml:get_attribute(Packet, to, <<"unknown">>),
                            {RoomInfo, _} = muc_state:room_info_by_from_value(JidInfo, To, From),
                            RoomMuteState = proplists:get_value(mute, RoomInfo, false),
                            if
                                RoomMuteState /= true ->
                                    exmpp_session:send_packet(XmppSession, Packet);
                                true ->
                                    ulog:debug("MUC Packet not sent: MUTE")
                            end;
                        true ->
                            exmpp_session:send_packet(XmppSession, Packet)
                    end;
                true ->
                    ulog:debug("Packet not sent: MUTE")
            end;
        %%    timer:send_after(MucKeepalivePause, {join, Room, Nick, Password});
        %% If we receive a message, we reply with the same message
                                                %Record = #received_packet{packet_type=message, raw_packet=Packet} ->
                                                %    %ulog:debug(Record),
                                                %    echo_packet(XmppSession, Packet),
                                                %    loop(XmppSession);
        Record = #received_packet{packet_type=presence, raw_packet=Packet}
          when ?IS_PRESENCE(Packet) ->
            PacketType = exmpp_presence:get_type(Packet),
            presences:process(PacketType, JidInfo, XmppSession, Packet);
        Record = #received_packet{packet_type=message, raw_packet=Packet}
          when ?IS_MESSAGE(Packet) ->
            PacketType = exmpp_message:get_type(Packet),
            messages:process(PacketType, JidInfo, XmppSession, Packet);
        Record ->
            %% ulog:debug(Record),
            skip
    end,
    loop(XmppSession, ServerInfo, JidInfo).

%% Send the same packet back for each message received
echo_packet(XmppSession, Packet) ->
    From = exmpp_xml:get_attribute(Packet, from, <<"unknown">>),
    To = exmpp_xml:get_attribute(Packet, to, <<"unknown">>),
    TmpPacket = exmpp_xml:set_attribute(Packet, from, To),
    TmpPacket2 = exmpp_xml:set_attribute(TmpPacket, to, From),
    NewPacket = exmpp_xml:remove_attribute(TmpPacket2, id),
    exmpp_session:send_packet(XmppSession, NewPacket).
