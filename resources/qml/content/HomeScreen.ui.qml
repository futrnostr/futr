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

    ColumnLayout {
        anchors.fill: parent
        spacing: 10

        // Profile button
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
                }
            }
        }

        // Profile loader
        Loader {
            id: profileLoader
            Layout.alignment: Qt.AlignRight
            Layout.rightMargin: 10
            Layout.topMargin: -100
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

        // Follows list
        ListView {
            id: followsView
            Layout.fillWidth: true
            Layout.fillHeight: true
            clip: true
            spacing: 5

            model: AutoListModel {
                source: follows
                mode: AutoListModel.ByKey
            }

            delegate: Rectangle {
                id: followItem
                property bool mouseHover: false
                height: 80
                width: parent ? parent.width : 200
                color: mouseHover ? "lightsteelblue" : "#f0f0f0"
                border.color: "gray"
                radius: 5

                RowLayout {
                    anchors.fill: parent
                    anchors.margins: 10

                    Image {
                        source: Util.getProfilePicture(modelData.picture, modelData.pubkey)
                        width: 50
                        height: 50
                        clip: true
                        Layout.preferredWidth: 50
                        Layout.preferredHeight: 50
                        smooth: true
                        fillMode: Image.PreserveAspectCrop
                    }

                    RowLayout {
                        spacing: 10

                        ColumnLayout {
                            Layout.fillWidth: true
                            spacing: 5

                            Text {
                                text: modelData.displayName !== "" ? modelData.displayName : modelData.pubkey
                                font: Constants.largeFont
                                elide: Text.ElideRight
                                wrapMode: Text.NoWrap
                                clip: true
                                Layout.alignment: Qt.AlignVCenter
                            }

                            Text {
                                text: modelData.relay
                                elide: Text.ElideRight
                                wrapMode: Text.NoWrap
                                clip: true
                                Layout.alignment: Qt.AlignVCenter
                            }
                        }

                        Button {
                            text: qsTr("Chat Now")
                            onClicked: {
                                openChat(modelData.pubkey)
                            }
                        }

                        Button {
                            text: qsTr("Unfollow")
                            onClicked: {
                                unfollow(modelData.pubkey)
                            }
                        }
                    }
                }

                MouseArea {
                    anchors.fill: parent
                    hoverEnabled: true
                    onEntered: followItem.mouseHover = true
                    onExited: followItem.mouseHover = false
                }
            }
        }
    }
}
