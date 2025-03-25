import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15
import QtGraphicalEffects 1.15

import Futr 1.0

Item {
    id: topBar

    RowLayout {
        id: controlsLayout
        anchors.right: parent.right
        anchors.verticalCenter: parent.verticalCenter
        anchors.rightMargin: 10
        spacing: 10

        Image {
            id: profilePicture
            Layout.preferredWidth: 60
            Layout.preferredHeight: 60
            source: Util.getProfilePicture(mypicture, mynpub)
            fillMode: Image.PreserveAspectCrop

            layer.enabled: true
            layer.effect: OpacityMask {
                maskSource: Rectangle {
                    width: profilePicture.width
                    height: profilePicture.height
                    radius: width / 2
                }
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

            ToolTip.visible: hovered
            ToolTip.text: qsTr("Switch to " + (isDarkTheme ? "Light" : "Dark") + " Mode")
        }

        Button {
            id: menuButton
            Layout.preferredWidth: 40
            Layout.preferredHeight: 40
            flat: true

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
                console.log("Search result npub:", result.npub)
                if (result && result.npub) {
                    searchInput.text = ""
                    stackView.replace(personalFeedComponent, {"npub": result.npub})
                    console.log("Navigated to PersonalFeed.ui.qml with npub:", result.npub)
                }
            }
        }
    }
}
