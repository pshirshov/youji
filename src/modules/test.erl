-module(test).

-behaviour(packet_handler).

-export([handle_chat/4,
         handle_groupchat/4,
         handle_presence_available/4,
         handle_presence_unavailable/4,
         handle_room_presence_available/4,
         handle_room_presence_unavailable/4
        ]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include("xmpp.hrl").

%% Return values: ok, stop

handle_chat(JidInfo, XmppSession, Message, WorkerPID) ->
    ok.

handlez_groupchat(JidInfo, XmppSession, Message, WorkerPID) ->
    ulog:debug(JidInfo),
    ok.
handle_groupchat(JidInfo, XmppSession, Message, WorkerPID) ->
    %% Packet = Message,
    %% NewPacket = exmpp_xml:remove_attribute(TmpPacket2, id),
    OldBody = exmpp_message:get_body(Message),
    OldFrom = exmpp_xml:get_attribute(Message, from, <<"unknown">>),
    Body = "BLABLABLA",%%tools:format_str("Dummy message from worker ~w in reply to '~s' from ~s",
    %%                 [self(), OldBody, OldFrom]),
    Packet1 = exmpp_message:make_groupchat(?NS_JABBER_CLIENT, Body),
    From = muc_tools:roomJidByJidInfo(JidInfo), %%exmpp_xml:get_attribute(Packet, from, <<"unknown">>),
    To = <<"sibotaku@conference.neko.im">>, %%exmpp_xml:get_attribute(Packet, to, <<"unknown">>),
    TmpPacket = exmpp_xml:set_attribute(Packet1, from, From),
    TmpPacket2 = exmpp_xml:set_attribute(TmpPacket, to, To),
    NewPacket = TmpPacket2,
    %% exmpp_session:send_packet(XmppSession, NewPacket),
    WorkerPID ! {send_packet, NewPacket},
    ok.

handle_presence_available(JidInfo, XmppSession, Message, WorkerPID) ->
    ulog:debug("PRESENCE AVAIL"),
    ok.

handle_presence_unavailable(JidInfo, XmppSession, Message, WorkerPID) ->
    ulog:debug("PRESENCE UNAVAIL"),
    ok.

handle_room_presence_available(JidInfo, XmppSession, Message, WorkerPID) ->
    ulog:debug("ROOM PRESENCE AVAIL"),
    ok.

handle_room_presence_unavailable(JidInfo, XmppSession, Message, WorkerPID) ->
    ulog:debug("ROOM PRESENCE UNAVAIL"),
    ok.
