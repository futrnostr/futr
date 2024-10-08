import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import HsQML.Model 1.0
import Futr 1.0
import Profile 1.0

Item {
    id: homeScreen
    width: parent.width
    height: parent.height

    ColumnLayout {
        anchors.fill: parent
        spacing: 10

        // Top row with profile button
        Item {
            Layout.fillWidth: true
            height: 80

            RoundButton {
                id: profileButton
                anchors.right: parent.right
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

                    onClosed: {
                        if (!profileLoader.item || !profileLoader.item.visible) {
                            profileCard.visible = false
                            profileLoader.source = ""
                        }
                    }

                    MenuItem {
                        text: qsTr("My Profile")
                        onTriggered: {
                            var profile = JSON.parse(getProfile(mynpub))
                            profileLoader.setSource(
                                "Profile/MyProfile.ui.qml",
                                { "profileData": profile }
                            )
                            profileCard.visible = true
                            profileMenu.close()
                        }
                    }

                    MenuItem {
                        text: qsTr("Settings")
                        onTriggered: {
                            // Dummy for now, does nothing
                            console.log("Settings clicked")
                            profileCard.visible = false
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
        }

        // Search row
        RowLayout {
            Layout.alignment: Qt.AlignHCenter
            spacing: 10

            TextField {
                id: searchInput
                placeholderText: qsTr("Enter npub to search")
                Layout.preferredWidth: 300
            }

            Button {
                text: qsTr("Search")
                onClicked: {
                    var npub = searchInput.text.trim()
                    if (npub.length > 0) {
                        profileLoader.setSource(
                            "Profile/ViewProfile.ui.qml",
                            { "npub": npub }
                        )
                    }
                }
            }

            Button {
                text: qsTr("Follow")
                onClicked: {
                    var npub = searchInput.text.trim()
                    if (npub.length > 0) {
                        follow(npub)
                        searchInput.text = ""
                    }
                }
            }
        }

        Item {
            Layout.fillWidth: true
            Layout.fillHeight: true

            Row {
                anchors.fill: parent
                spacing: 10

                // Left column: Follows list
                Rectangle {
                    width: parent.width * 0.3 - (parent.spacing * 2 / 3)
                    height: parent.height
                    color: Material.backgroundColor

                    ColumnLayout {
                        anchors.fill: parent
                        spacing: 10

                        // Filter input
                        Rectangle {
                            Layout.fillWidth: true
                            height: 40
                            radius: 20
                            color: Material.background
                            border.color: Material.accentColor
                            border.width: filterInput.activeFocus ? 2 : 1

                            RowLayout {
                                anchors.fill: parent
                                spacing: 10

                                Image {
                                    Layout.leftMargin: 10
                                    source: "qrc:/icons/search.svg"
                                    sourceSize.width: 20
                                    sourceSize.height: 20
                                    Layout.alignment: Qt.AlignVCenter
                                }

                                TextField {
                                    id: filterInput
                                    Layout.fillWidth: true
                                    Layout.fillHeight: true
                                    placeholderText: qsTr("Filter follows...")
                                    background: Item {}
                                    color: Material.foreground
                                    font.pixelSize: 14
                                    verticalAlignment: TextInput.AlignVCenter
                                    leftPadding: 0
                                    rightPadding: 0
                                    topPadding: 0
                                    bottomPadding: 0
                                }

                                Image {
                                    source: "qrc:/icons/close.svg"
                                    sourceSize.width: 16
                                    sourceSize.height: 16
                                    Layout.alignment: Qt.AlignVCenter
                                    Layout.rightMargin: 10
                                    visible: filterInput.text.length > 0
                                    opacity: clearMouseArea.containsMouse ? 0.7 : 1.0

                                    MouseArea {
                                        id: clearMouseArea
                                        anchors.fill: parent
                                        hoverEnabled: true
                                        cursorShape: Qt.PointingHandCursor
                                        onClicked: filterInput.text = ""
                                    }
                                }
                            }
                        }

                        // Follows list
                        ListView {
                            id: followsView
                            Layout.fillWidth: true
                            Layout.fillHeight: true
                            clip: true
                            spacing: 5

                            model: AutoListModel {
                                id: followsModel
                                source: follows
                            }

                            delegate: Rectangle {
                                id: followItem
                                property bool mouseHover: false
                                height: visible ? 80 : 0
                                width: parent ? parent.width : 200
                                visible: {
                                    if (filterInput.text === "") return true;
                                    var searchText = filterInput.text.toLowerCase();
                                    return modelData.pubkey.toLowerCase().includes(searchText) ||
                                           (modelData.displayName && modelData.displayName.toLowerCase().includes(searchText));
                                }
                                color: mouseHover ? Material.accentColor : Material.backgroundColor
                                border.color: Material.dividerColor
                                radius: 5

                                RowLayout {
                                    anchors.fill: parent
                                    anchors.margins: 10

                                    Image {
                                        source: Util.getProfilePicture(modelData.picture, modelData.pubkey)
                                        Layout.preferredWidth: 50
                                        Layout.preferredHeight: 50
                                        Layout.alignment: Qt.AlignVCenter
                                        smooth: true
                                        fillMode: Image.PreserveAspectCrop
                                    }

                                    ColumnLayout {
                                        Layout.fillWidth: true
                                        spacing: 5

                                        Text {
                                            text: modelData.displayName !== "" ? modelData.displayName : modelData.pubkey
                                            font: Constants.font
                                            color: Material.primaryTextColor
                                            elide: Text.ElideRight
                                            Layout.fillWidth: true
                                        }

                                        Text {
                                            text: modelData.pubkey
                                            elide: Text.ElideRight
                                            Layout.fillWidth: true
                                            font: Constants.smallFont
                                            color: Material.secondaryTextColor
                                            visible: modelData.displayName !== ""
                                        }
                                    }
                                }

                                MouseArea {
                                    anchors.fill: parent
                                    hoverEnabled: true
                                    onEntered: followItem.mouseHover = true
                                    onExited: followItem.mouseHover = false
                                    onClicked: {
                                        chatLoader.setSource("Chat/ChatWindow.ui.qml", { "pubkey": modelData.pubkey })
                                        rightProfileLoader.setSource("Profile/ViewProfile.ui.qml", { "npub": modelData.pubkey })
                                    }
                                }
                            }
                        }
                    }
                }

                // Center column: Chat window
                Rectangle {
                    width: parent.width * 0.4 - (parent.spacing * 2 / 3)
                    height: parent.height
                    color: Material.backgroundColor

                    Loader {
                        id: chatLoader
                        anchors.fill: parent
                    }
                }

                // Right column: Profile view
                Rectangle {
                    width: parent.width * 0.3 - (parent.spacing * 2 / 3)
                    height: parent.height
                    color: Material.backgroundColor

                    Loader {
                        id: rightProfileLoader
                        anchors.fill: parent
                    }
                }
            }
        }
    }

    // Profile card
    Pane {
        id: profileCard
        width: 400
        padding: 0
        visible: false
        Material.elevation: 6

        anchors {
            right: parent.right
            top: parent.top
            margins: 20
        }

        Loader {
            id: profileLoader
            onLoaded: {
                if (item && typeof item.closeRequested === "function") {
                    item.closeRequested.connect(function() {
                        profileCard.visible = false
                        profileLoader.source = ""
                    })
                }
            }
        }

        Behavior on opacity {
            NumberAnimation { duration: 150 }
        }

        onVisibleChanged: {
            opacity = visible ? 1 : 0
        }
    }
}
