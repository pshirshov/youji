-module(presences).

-include_lib("exmpp/include/exmpp_client.hrl").
-include("xmpp.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public
-export([process/4]).

process(error, JidInfo, XmppSession, Packet) ->
    process_presence_error(JidInfo, XmppSession, Packet);
process(available, JidInfo, XmppSession, Packet) ->
    process_presence_available(JidInfo, XmppSession, Packet);
process(unavailable, JidInfo, XmppSession, Packet) ->
    process_presence_unavailable(JidInfo, XmppSession, Packet);
process(PacketType, JidInfo, XmppSession, Packet) ->
    ulog:warning("Unknown presence type `~p`:~n  ~p", [PacketType, Packet]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
process_nick_conflict(RoomInfo, OldNick) ->
    Nick = tools:format_str("~s_", [OldNick]),
    Pause = config:get(muc_nick_conflict_rejoin_timeout),
    ulog:info("Rejoining to ~p as ~s due to nick conflict", [RoomInfo, Nick]),
    muc_tools:rejoin_using_info(RoomInfo, Nick, Pause).

process_access_denied(RoomInfo, OldNick) ->
    Pause = config:get(muc_access_denied_rejoin_timeout),
    ulog:info("Probably not-a-member. Access denied at ~p as ~s", [RoomInfo, OldNick]),
    muc_tools:rejoin_using_info(RoomInfo, OldNick, Pause).

process_authorization_required(RoomInfo, OldNick) ->
    ulog:info("Bad password. Access denied at ~p as ~s. Giving up.", [RoomInfo, OldNick]).

process_presence_error(JidInfo, XmppSession, Presence) ->
    %%    ulog:debug("Error Presence ~p", [exmpp_xml:node_to_binary(Presence, a, [b])]),
    %%    ulog:debug("Error Presence ~p", [Presence]),
    ErrorInfo = exmpp_xml:get_element(Presence, error),
    From = exmpp_xml:get_attribute(Presence, from, undefined),
    To = exmpp_xml:get_attribute(Presence, to, undefined),
    ulog:debug("Error presence catched:~n From: ~p~n To  : ~p~n ~p", [From, To, ErrorInfo]),
    case ErrorInfo of
        undefined ->
            ulog:warning("Error presence doesn't contain error section:~n  ~p", [Presence]);
        _ ->
            ErrorCode = exmpp_xml:get_attribute(ErrorInfo, code, undefined),
            ulog:debug("Presence error code: ~p", [ErrorCode]),
            %% Room joining errors, rejoin if possible
            %% TODO: strict checking for room errors
            %% TODO: ? check From, To for undefined value
            try
                {RoomInfo, OldNick}
                    = muc_state:room_info_by_from_value(JidInfo, From, To),
                muc_state:remember_conference_status(proplists:get_value(jid, RoomInfo, undefined), OldNick, error),
                case ErrorCode of
                    <<"409">> -> %%when From /= undefined, To /= undefined ->
                        process_nick_conflict(RoomInfo, OldNick);
                    <<"403">> ->
                        process_access_denied(RoomInfo, OldNick);
                    <<"401">> ->
                        process_authorization_required(RoomInfo, OldNick);
                    _ ->
                        ulog:warning("Unknown presence error code:~n  ~p", [Presence])
                end
            catch
                {jid, parse, Reason, {jid, String}} ->
                    ulog:warning("JID parsing error (~p, ~s). Giving up. No more rejoins. Presence:~n  ~p",
                                 [Reason, String, Presence]);
                {bad, What} ->
                    ulog:warning("Bad argument in server answer: ~p. Giving up. No more rejoins. presence:~n  ~p", [What, Presence])
            end
    end.

process_presence_statuses(Statuses, RoomInfo, OldNick) ->
    lists:foreach(fun(Status) ->
                          StatusCode = exmpp_xml:get_attribute(Status, code, undefined),
                          %% ulog:debug(StatusCode),
                          case StatusCode of
                              <<"110">> ->
                                  ulog:info("Successfully entered to room ~p as ~s", [RoomInfo, OldNick]),
                                  RoomJid = proplists:get_value(jid, RoomInfo),
                                  muc_state:remember_conference_status(RoomJid, OldNick, entered);
                              <<"210">> ->
                                  ulog:info("Nick was overwritten by server: ~s (~p)", [OldNick, RoomInfo]);
                              _ ->
                                  skip
                          end
                  end,
                  Statuses).

process_presence(JidInfo, XmppSession, Presence, HandlerAtom, XInfoLambda) ->
    From = exmpp_xml:get_attribute(Presence, from, undefined),
    To = exmpp_xml:get_attribute(Presence, to, undefined),
    XInfo = exmpp_xml:get_element(Presence, ?NS_MUC_USER, x),
    %%ulog:debug("Available presence catched:~n From: ~p~n To  : ~p~n ~p", [From, To, XInfo]),
    case XInfo of
        undefined -> % Not a muc presence
            tools:run_handlers_by_jid_info(HandlerAtom, JidInfo, XmppSession, Presence);
        _ ->
            try
                {RoomInfo, OldNick}
                    = muc_state:room_info_by_from_value(JidInfo, From, To),
                XInfoLambda(XInfo, RoomInfo, OldNick),
                tools:run_handlers_by_room_info_record(handle_room_presence_available, JidInfo, RoomInfo, XmppSession, Presence)
            catch
                {jid, parse, Reason, {jid, String}} ->
                    ulog:warning("JID parsing error (~p, ~s). Skipping presence:~n  ~p",
                                 [Reason, String, Presence]);
                {bad, What} ->
                    ulog:warning("Bad argument in server answer: ~p. Skipping presence:~n  ~p", [What, Presence])
            end
    end.

process_presence_available(JidInfo, XmppSession, Presence) ->
    %%ulog:debug(Presence),
    XInfoLambda = fun(XInfo, RoomInfo, OldNick) ->
                          Statuses = exmpp_xml:get_elements(XInfo, status),
                          process_presence_statuses(Statuses, RoomInfo, OldNick)
                  end,
    process_presence(JidInfo, XmppSession, Presence, handle_presence_available, XInfoLambda).

process_presence_unavailable(JidInfo, XmppSession, Presence) ->
    XInfoLambda = fun(_XInfo, _RoomInfo, _OldNick) ->
                          ok
                  end,
    process_presence(JidInfo, XmppSession, Presence, handle_presence_unavailable, XInfoLambda).
