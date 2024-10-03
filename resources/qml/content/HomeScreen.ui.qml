import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import HsQML.Model 1.0
import Futr 1.0
import Profile 1.0

Rectangle {
    id: homeScreen
    width: parent.width
    height: parent.height
    color: Material.backgroundColor

    ColumnLayout {
        anchors.fill: parent
        spacing: 10

        RowLayout {
            Layout.alignment: Qt.AlignRight
            Layout.fillWidth: true

            RoundButton {
                width: 75
                height: 75
                flat: true

                icon.width: 65
                icon.height: 65
                icon.color: "transparent"

                Component.onCompleted: {
                    icon.source = Util.getProfilePicture(mypicture, mynpub)
                }

                Material.elevation: 10

                ToolTip.visible: hovered
                ToolTip.delay: 500
                ToolTip.timeout: 5000
                ToolTip.text: qsTr("My Profile")

                onClicked: {
                    var profile = JSON.parse(getProfile(mynpub))
                    profileLoader.setSource(
                        "Profile/MyProfile.ui.qml",
                        { "profileData": profile }
                    )
                    profileCard.visible = true
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
                width: 300
            }

            Button {
                text: qsTr("Search")
                onClicked: {
                    var npub = searchInput.text.trim()
                    if (npub.length > 0) {
                        // Implement your search logic here
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

        // Three-column layout
        RowLayout {
            Layout.fillWidth: true
            Layout.fillHeight: true
            spacing: 10

            // Left column: Follows list
            ColumnLayout {
                Layout.preferredWidth: parent.width * 0.3
                Layout.fillHeight: true

                ScrollView {
                    Layout.fillWidth: true
                    Layout.fillHeight: true

                    ListView {
                        id: followsView
                        clip: true
                        spacing: 5

                        model: AutoListModel {
                            source: follows
                        }

                        delegate: Rectangle {
                            id: followItem
                            property bool mouseHover: false
                            height: 80
                            width: parent ? parent.width : 200
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
                                        text: modelData.relay
                                        elide: Text.ElideRight
                                        Layout.fillWidth: true
                                        font: Constants.smallFont
                                        color: Material.secondaryTextColor
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
            ColumnLayout {
                Layout.preferredWidth: parent.width * 0.4
                Layout.fillHeight: true

                Loader {
                    id: chatLoader
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                }
            }

            // Right column: Profile view
            ColumnLayout {
                Layout.preferredWidth: parent.width * 0.3
                Layout.fillHeight: true

                Loader {
                    id: rightProfileLoader
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                }
            }
        }
    }

    // Add this Rectangle for the profile card
    Rectangle {
        id: profileCard
        width: 400
        anchors {
            right: parent.right
            top: parent.top
            margins: 20
        }
        visible: false
        Material.elevation: 6
        radius: 10

        Loader {
            id: profileLoader
        }

        Behavior on opacity {
            NumberAnimation { duration: 150 }
        }

        onVisibleChanged: {
            opacity = visible ? 1 : 0
        }
    }
}
