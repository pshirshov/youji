-module(muc_state).

-include("xmpp.hrl").
-export([remember_conference_status/3, remove_state_for_pid/1, current_states/0, init/0]).
-export([roominfo_by_jid/2, room_info_by_from_value/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public
init() ->
    ets:new(muc_state, [named_table, set, public]).

current_states() ->
    ets:match_object(muc_state, {{self(), '_'}, '_'}).

remember_conference_status(Jid, Nick, Status) ->
    ConferenceRecord = {{self(), Jid}, [{nick, Nick}, {status, Status}]},
    ets:insert(muc_state, ConferenceRecord),
    ulog:debug("Current MUC state:~n ~p", [ets:tab2list(muc_state)]).

remove_state_for_pid(Pid) ->
    ets:match_delete(muc_state, {{Pid , '_'}, '_'}).

roominfo_by_jid(JidInfo, RoomJid) ->
    AllRooms = config:get(rooms),
    Rooms = proplists:get_value(JidInfo#jid_info.internal_id, AllRooms),
    roominfo_loop(RoomJid, Rooms).

room_info_by_from_value(JidInfo, From, _To) ->
    %% ToParsed = exmpp_jid:parse(To),
    FromParsed = exmpp_jid:parse(From),
    ulog:debug("Parsed From JID: ~p", [FromParsed]),

    DomainBin = exmpp_jid:prep_domain(FromParsed),
    ConferenceBin = exmpp_jid:prep_node(FromParsed),
    OldNickBin = exmpp_jid:prep_resource(FromParsed),

    if
        DomainBin == undefined ->
            throw ({bad, domain});
        ConferenceBin == undefined ->
            throw ({bad, conference});
        OldNickBin == undefined ->
            ulog:warning("Undefined Resource: ~p", [FromParsed]);
        %%throw ({bad, old_nick});
        true ->
            false
    end,

    Domain = binary_to_list(DomainBin),
    ConferenceName = binary_to_list(ConferenceBin),

    Conference = tools:format_str("~s@~s", [ConferenceName, Domain]),
    RoomInfo = muc_state:roominfo_by_jid(JidInfo, Conference),
    if
        OldNickBin == undefined ->
            {RoomInfo, undefined};
        true ->
            OldNick = binary_to_list(OldNickBin),
            {RoomInfo, OldNick}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
roominfo_loop(Jid, []) ->
    %% ulog:debug("ENDENDEND---------------------------------------"),
    throw({room_info_not_found_in_config, Jid});
roominfo_loop(Jid, [H|T]) ->
    %% ulog:debug(Jid),
    CurrentJid = proplists:get_value(jid, H, undefined),
    %% ulog:debug("Search: '~p' '~p' '~p'", [Jid, CurrentJid, H]),
    %% ulog:debug(string:equal(Jid, CurrentJid)),
    case CurrentJid of
        Jid ->
            H;
        undefined ->
            throw({bad_room_info_in_config, H});
        _ ->
            roominfo_loop(Jid, T)
    end.
