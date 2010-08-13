-module(messages).

-include_lib("exmpp/include/exmpp_client.hrl").
-include("xmpp.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public
-export([process/4]).

process(chat, JidInfo, XmppSession, Packet) ->
    process_chat(JidInfo, XmppSession, Packet);
process(groupchat, JidInfo, XmppSession, Packet) ->
    process_groupchat(JidInfo, XmppSession, Packet);
process(PacketType, _JidInfo, _XmppSession, Packet) ->
    ulog:warning("Unknown message type `~p`:~n  ~p", [PacketType, Packet]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private

%% run_chat_handlers(Modules, JidInfo, XmppSession, Packet) ->
%%     tools:run_handlers(Modules, ok, handle_chat, JidInfo, XmppSession, Packet).

%% run_groupchat_handlers(Modules, JidInfo, XmppSession, Packet) ->
%%     tools:run_handlers(Modules, ok, handle_groupchat, JidInfo, XmppSession, Packet).

process_chat(JidInfo, XmppSession, Packet) ->
    %% ulog:info("MESSAGE ~p", [Packet]),
    %%Handlers = JidInfo#jid_info.modules,
    %%run_chat_handlers(Handlers, JidInfo, XmppSession, Packet).
    tools:run_handlers_by_jid_info(handle_chat, JidInfo, XmppSession, Packet).

process_groupchat(JidInfo, XmppSession, Packet) ->
    tools:run_handlers_by_room_info(handle_groupchat,JidInfo, XmppSession, Packet).

    %% ulog:info("MUC MESSAGE ~p", [Packet]),
%% TestAtom = test,
%% %% Load JID list
%% %% Load MUC list
%% TestAtom:handle_groupchat(JidInfo, XmppSession, Packet, self()).
