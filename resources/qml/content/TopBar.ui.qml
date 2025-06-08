import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Futr 1.0

Item {
    id: topBar
    required property string currentUserPicture
    required property string currentUser
    
    // Set explicit bounds to prevent unnecessary rendering area
    implicitHeight: 80
    clip: true

    RowLayout {
        id: controlsLayout
        anchors.right: parent.right
        anchors.verticalCenter: parent.verticalCenter
        anchors.rightMargin: 10
        spacing: 10
        
        // Set explicit size to prevent overdraw
        width: implicitWidth
        height: 60

        Rectangle {
            id: profilePictureContainer
            Layout.preferredWidth: 60
            Layout.preferredHeight: 60
            radius: 30
            // Remove default background
            color: "transparent"
            clip: true
            
            Image {
                id: profilePicture
                anchors.fill: parent
                source: Util.getProfilePicture(currentUserPicture, currentUser)
                fillMode: Image.PreserveAspectCrop
                cache: false
                // Ensure no background
                smooth: true
            }
        }

        Button {
            id: themeToggle
            Layout.preferredWidth: 40
            Layout.preferredHeight: 40
            icon.source: isDarkTheme ? "qrc:/icons/light_mode.svg" : "qrc:/icons/dark_mode.svg"
            icon.color: Material.foreground
            onClicked: isDarkTheme = !isDarkTheme
            flat: true
            // Explicitly remove background
            background: Item {}

            ToolTip.visible: hovered
            ToolTip.text: qsTr("Switch to " + (isDarkTheme ? "Light" : "Dark") + " Mode")
        }

        Button {
            id: menuButton
            Layout.preferredWidth: 40
            Layout.preferredHeight: 40
            flat: true
            // Explicitly remove background
            background: Item {}

            icon.source: "qrc:/icons/menu.svg"
            icon.width: 24
            icon.height: 24

            onClicked: profileMenu.open()
        }
    }

    Menu {
        id: profileMenu
        y: menuButton.y + menuButton.height + 10
        x: parent.width - width - 10
        // Optimize menu rendering
        clip: true

        MenuItem {
            text: qsTr("Relay Management")
            onTriggered: {
                profileMenu.close()
                relayMgmtDialog.open()
            }
        }

        MenuItem {
            text: qsTr("My Keys")
            onTriggered: {
                profileMenu.close()
                showKeysDialog.open()
            }
        }

        MenuItem {
            text: qsTr("Logout")
            onTriggered: {
                logout()
            }
        }
    }

    // Search row
    RowLayout {
        anchors.centerIn: parent
        spacing: 10
        // Set explicit size to prevent overdraw
        width: implicitWidth
        height: implicitHeight

        TextField {
            id: searchInput
            placeholderText: qsTr("Enter npub, nprofile, or NIP-05 (name@domain.com)")
            Layout.preferredWidth: 400
            onAccepted: searchButton.clicked()
            // Optimize text field rendering
            selectByMouse: true
        }

        Button {
            id: searchButton
            text: qsTr("Search")
            // Remove unnecessary background if flat
            // background: Item {}
            onClicked: {
                var input = searchInput.text.trim()
                var result = JSON.parse(search(input))

                if (result && result.npub) {
                    searchInput.text = ""
                    personalFeed.npub = result.npub
                }
            }
        }
    }
}
