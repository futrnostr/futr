import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Profile 1.0

RowLayout {
    id: personalFeed
    anchors.fill: parent
    spacing: 10

    required property var npub

    Component.onCompleted: {
        setCurrentProfile(npub) // @todo setCurrentProfile and openChat should be one call maybe?
        openChat(npub)          //       investigate further
        chat.profileData = currentProfile
        profile.profileData = currentProfile
    }

    Rectangle {
        Layout.preferredWidth: parent.width * 0.4
        Layout.fillHeight: true
        Layout.fillWidth: true
        Layout.preferredHeight: parent.height
        color: Material.backgroundColor

        Chat {
            id: chat
            anchors.fill: parent
            anchors.rightMargin: 10
            anchors.bottomMargin: 5

            npub: personalFeed.npub
        }
    }

    Rectangle {
        Layout.preferredWidth: parent.width * 0.3
        Layout.fillHeight: true
        Layout.fillWidth: true
        Layout.preferredHeight: parent.height
        color: Material.backgroundColor

        Profile {
            id: profile
            anchors.fill: parent

            npub: personalFeed.npub
        }
    }
}
