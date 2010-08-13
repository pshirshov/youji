-module(muc_tools).

-include("xmpp.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public
-export([rejoin_using_info/3]).
-export([create_open_room_presence/2, create_protected_room_presence/3]).
-export([roomJidByJidInfo/1]).

roomJidByJidInfo(JidInfo) ->
    tools:format_str("~s/~s", [JidInfo#jid_info.jid, JidInfo#jid_info.resource]).

rejoin_using_info(RoomInfo, Nick, Pause) ->
    Conference =  proplists:get_value(jid, RoomInfo, undefined),
    Password = proplists:get_value(password, RoomInfo, undefined),
    case Password of
        undefined ->
            timer:send_after(Pause, {join, Conference, Nick});
        _ ->
            timer:send_after(Pause, {join, Conference, Nick, Password})
    end.

create_open_room_presence(Jid, Username) ->
    BasePresence = exmpp_xml:set_attribute(?EMPTY_PRESENCE, to, list_to_binary(Jid++"/"++Username)),
    exmpp_xml:append_child(BasePresence,
                           #xmlel{name=x, attrs=[#xmlattr{name=xmlns, value=?NS_MUC_b}]
                                 }
                          ).

create_protected_room_presence(Jid, Username, Password) ->
    BasePresence = exmpp_xml:set_attribute(?EMPTY_PRESENCE, to, list_to_binary(Jid++"/"++Username)),
    exmpp_xml:append_child(BasePresence,
                           #xmlel{name=x, attrs=[#xmlattr{name=xmlns, value=?NS_MUC_b}],
                                  children = [
                                              #xmlel{name=password,
                                                     children=[#xmlcdata{cdata=Password}]
                                                    }
                                             ]

                                 }
                          ).

%% #xmlel {name=presence, attrs=[#xmlattr{name=to, value=list_to_binary(Jid++"/"++Username)}],
%%         children=[
%%                   #xmlel{name=x, attrs=[#xmlattr{name=xmlns, value=?NS_MUC_b}],
%%                          children = [
%%                                      #xmlel{name=password,
%%                                             children=[#xmlcdata{cdata=Password}]
%%                                            }
%%                                     ]

%%                         }
%%                  ]
%%        }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
