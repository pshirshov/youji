-module(packet_handler).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
     {handle_groupchat, 4},
     {handle_chat, 4},
     {handle_presence_available, 4},
     {handle_presence_unavailable, 4},
     {handle_room_presence_available, 4},
     {handle_room_presence_unavailable, 4}
    ];
behaviour_info(_Other) ->
    undefined.
