-module(tools).

-export([format_str/2]).
-export([run_handlers/6, run_handlers_by_jid_info/4,
         run_handlers_by_room_info/4, run_handlers_by_room_info_record/5]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include("xmpp.hrl").

format_str(Format, Data) ->
    erlang:binary_to_list(erlang:iolist_to_binary(io_lib:format(Format, Data))).

run_handlers(_, stop, HandlerAtom, _JidInfo, _XmppSession, _Packet) ->
    stop;
run_handlers([], ok,_HandlerAtom, _JidInfo, _XmppSession, _Packet) ->
    ok;
run_handlers([H|T], ok, HandlerAtom, JidInfo, XmppSession, Packet) ->
    Ret = H:HandlerAtom(JidInfo, XmppSession, Packet, self()),
    run_handlers(T, Ret, HandlerAtom, JidInfo, XmppSession, Packet).

run_handlers_by_jid_info(HandlerAtom, JidInfo, XmppSession, Packet) ->
    Modules = JidInfo#jid_info.modules,
    run_handlers(Modules, ok, HandlerAtom, JidInfo, XmppSession, Packet).

run_handlers_by_room_info(HandlerAtom, JidInfo, XmppSession, Packet) ->
    From = exmpp_xml:get_attribute(Packet, from, undefined),
    To = exmpp_xml:get_attribute(Packet, to, undefined),
    %% TODO: Strict check?
    {RoomInfo, _OldNick}
        = muc_state:room_info_by_from_value(JidInfo, From, To),
    run_handlers_by_room_info_record(HandlerAtom, JidInfo, RoomInfo, XmppSession, Packet).
    %% RoomHandlers = proplists:get_value(modules, RoomInfo, []),
    %% JidHandlers = JidInfo#jid_info.modules,
    %% Modules = JidHandlers ++ RoomHandlers,
    %% run_handlers(Modules, ok, HandlerAtom, JidInfo, XmppSession, Packet).

run_handlers_by_room_info_record(HandlerAtom, JidInfo, RoomInfo, XmppSession, Packet) ->
    RoomHandlers = proplists:get_value(modules, RoomInfo, []),
    JidHandlers = JidInfo#jid_info.modules,
    Modules = JidHandlers ++ RoomHandlers,
    run_handlers(Modules, ok, HandlerAtom, JidInfo, XmppSession, Packet).

