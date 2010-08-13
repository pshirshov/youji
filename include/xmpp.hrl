-record(server_info,
        {port = 5222,
         addr
        }).

-record(jid_info,
        {internal_id,
         jid,
         password,
         status_text,
         resource = random,
         mute = false,
         modules = []
        }).

-include_lib("exmpp/include/exmpp.hrl").

-define (NS_MUC_b, list_to_binary(?NS_MUC_s)).
%%<<"http://jabber.org/protocol/muc">>).

%% copied from src/core/exmpp_presence.erl
-define(EMPTY_PRESENCE, #xmlel{name = 'presence'}).
