import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Futr 1.0

Item {
    id: topBar

    RoundButton {
        id: profileButton
        anchors.right: parent.right
        anchors.rightMargin: 100
        anchors.verticalCenter: parent.verticalCenter
        width: 75
        height: 75
        flat: true

        icon.width: 65
        icon.height: 65
        icon.color: "transparent"
        icon.source: Util.getProfilePicture(mypicture, mynpub)

        Material.elevation: 6

        onClicked: profileMenu.open()

        background: Rectangle {
            color: "transparent"
        }

        states: [
            State {
                name: "pressed"
                when: profileButton.pressed
                PropertyChanges { target: profileButton; opacity: 0.8 }
            }
        ]

        Menu {
            id: profileMenu
            y: profileButton.height

            MenuItem {
                text: qsTr("My Profile")
                onTriggered: {
                    setCurrentProfile(mynpub)
                    profileLoader.setSource(
                        "Profile/Profile.ui.qml",
                        { "profileData": currentProfile, "npub": mynpub }
                    )
                    profileMenu.close()
                }
            }

            MenuItem {
                text: qsTr("Relay Management")
                onTriggered: {
                    profileMenu.close()
                    relayMgmtDialog.open()
                }
            }

            MenuItem {
                text: qsTr("Settings")
                onTriggered: {
                    // Dummy for now, does nothing
                    console.log("Settings clicked")
                    profileMenu.close()
                }
            }

            MenuItem {
                text: qsTr("Logout")
                onTriggered: {
                    logout()
                }
            }
        }
    }

    // Search row
    RowLayout {
        anchors.centerIn: parent
        spacing: 10

        TextField {
            id: searchInput
            placeholderText: qsTr("Enter npub or nprofile")
            Layout.preferredWidth: 300
            onAccepted: searchButton.clicked()
        }

        Button {
            id: searchButton
            text: qsTr("Search")
            onClicked: {
                var input = searchInput.text.trim()
                var result = JSON.parse(search(input))
                if (result && result.npub) {
                    setCurrentProfile(result.npub)
                    profileLoader.setSource("Profile/Profile.ui.qml", {
                        "profileData": currentProfile,
                        "npub": result.npub
                    })
                    searchInput.text = ""
                }
            }
        }
    }
}
