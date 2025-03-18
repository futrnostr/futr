import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Profile 1.0

RowLayout {
    id: personalFeed
    anchors.fill: parent

    required property var npub

    property var editMode: false

    Component.onCompleted: {
        setCurrentProfile(npub) // @todo setCurrentProfile and openChat should be one call maybe?
        openChat(npub)          //       investigate further
        chat.profileData = currentProfile
        profile.profileData = currentProfile
        profileEdit.profileData = currentProfile
    }

    Rectangle {
        Layout.preferredWidth: parent.width * 0.6
        Layout.fillHeight: true
        Layout.fillWidth: true
        Layout.preferredHeight: parent.height
        color: Material.backgroundColor

        Chat {
            id: chat
            anchors.fill: parent

            npub: personalFeed.npub
        }
    }

    Rectangle {
        Layout.preferredWidth: parent.width * 0.4
        Layout.fillHeight: true
        Layout.fillWidth: true
        Layout.preferredHeight: parent.height
        color: Material.backgroundColor

        Profile {
            id: profile
            anchors.fill: parent
            visible: !personalFeed.editMode

            npub: personalFeed.npub
        }

        EditProfile {
            id: profileEdit
            anchors.fill: parent
            visible: personalFeed.editMode

            npub: personalFeed.npub
        }
    }
}
