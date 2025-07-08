import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15
import QtQuick.Window 2.15
import QtMultimedia 5.15

import Components 1.0
import Futr 1.0
import Profile 1.0

TextEdit {
    property string value

    readOnly: true
    selectByMouse: true
    wrapMode: Text.Wrap
    textFormat: Text.RichText
    color: Material.foreground
    text: value

    onLinkActivated: function(link) {
        if (!link) return;

        try {
            if (link.startsWith("profile://")) {
                let profileId = link.substring(10);

                if (profileId.startsWith("nprofile")) {
                    profileId = convertNprofileToNpub(profileId);
                    if (!profileId) {
                        console.error("Failed to convert nprofile to npub");
                        return;
                    }
                }

                personalFeed.npub = profileId
            } else if (link.startsWith("note://")) {
                console.log("Note clicked:", link.substring(7));
            } else {
                Qt.openUrlExternally(link);
            }
        } catch (e) {
            console.error("Error handling link activation:", e);
        }
    }
}
