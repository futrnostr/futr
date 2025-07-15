import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Profile 1.0

RowLayout {
    id: personalFeed

    required property var npub
    required property var currentUser
    required property var currentUserPicture
    property var editMode: false

    Rectangle {
        Layout.preferredWidth: 60
        Layout.fillHeight: true
        Layout.fillWidth: true
        Layout.preferredHeight: parent.height
        color: Material.backgroundColor

        Chat {
            id: chat
            anchors.fill: parent

            npub: personalFeed.npub
            currentUser: personalFeed.currentUser
            currentUserPicture: personalFeed.currentUserPicture
        }
    }

    Rectangle {
        Layout.preferredWidth: 40
        Layout.fillHeight: true
        Layout.fillWidth: true
        Layout.preferredHeight: parent.height
        color: Material.backgroundColor

        Profile {
            id: profile
            anchors.fill: parent
            visible: !personalFeed.editMode

            npub: personalFeed.npub
            currentUser: personalFeed.currentUser
        }

        EditProfile {
            id: profileEdit
            anchors.fill: parent
            visible: personalFeed.editMode

            npub: personalFeed.npub
        }
    }
}
