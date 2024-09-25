import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Buttons 1.0
import Futr 1.0

Rectangle {
    id: root
    border.color: "#e0e0e0"
    radius: 5
    width: 400
    implicitHeight: content.implicitHeight + 20
    property var profileData: {}

    ColumnLayout {
        id: content
        anchors.fill: parent
        anchors.margins: 10
        spacing: 10

        RowLayout {
            Layout.fillWidth: true
            Layout.alignment: Qt.AlignCenter
            Layout.rightMargin: 2
            Layout.topMargin: 2

            BackButton {
                id: backButton

                onClicked: {
                    editMyProfile.visible = false;
                    myProfile.visible = true;
                }
            }

            Item {
                Layout.fillWidth: true
            }

            Text {
                text: "Edit Profile"
                font: Constants.largeFont
            }

            Item {
                Layout.fillWidth: true
            }

            CloseButton {
                id: closeButton
                target: root
            }
        }

        ColumnLayout {
            spacing: 10
            width: parent.width

            TextField {
                id: displayNameField
                placeholderText: "Display Name"
                Layout.fillWidth: true
            }

            TextField {
                id: nameField
                placeholderText: "Name"
                Layout.fillWidth: true
            }

            TextArea {
                id: aboutMeField
                placeholderText: "About Me"
                Layout.fillWidth: true
                Layout.preferredHeight: 70
                wrapMode: TextEdit.Wrap
            }

            TextField {
                id: avatarUrlField
                placeholderText: "Avatar URL"
                Layout.fillWidth: true
            }

            TextField {
                id: bannerUrlField
                placeholderText: "Banner URL"
                Layout.fillWidth: true
            }

            TextField {
                id: nip05Field
                placeholderText: "NIP05 Address"
                Layout.fillWidth: true
            }

            RowLayout {
                width: parent.width
                spacing: 5

                MouseArea {
                    id: toggleArea
                    width: parent.width
                    height: parent.height

                    onClicked: proofFields.visible = ! proofFields.visible
                }

                RowLayout {
                    spacing: 5
                    Layout.fillWidth: true

                    Rectangle {
                        height: 1
                        Layout.fillWidth: true
                    }

                    Text {
                        text: "External Identities"
                        color: "darkgray"
                        padding: 10
                    }

                    Text {
                        id: arrowSymbol
                        text: proofFields.visible ? "\u25BC" : "\u25B6"
                        color: "darkgray"
                    }

                    Rectangle {
                        height: 1
                        Layout.fillWidth: true
                    }
                }

                RotationAnimation {
                    target: arrowSymbol
                    from: 0
                    to: 90
                    duration: 200
                    running: proofFields.visible
                }
            }

            ColumnLayout {
                id: proofFields
                visible: false

                TextField {
                    id: githubProofField
                    placeholderText: "GitHub Proof"
                    Layout.fillWidth: true
                }

                TextField {
                    id: twitterProofField
                    placeholderText: "Twitter Proof"
                    Layout.fillWidth: true
                }

                TextField {
                    id: telegramProofField
                    placeholderText: "Telegram Proof"
                    Layout.fillWidth: true
                }
            }
        }

        RowLayout {
            width: parent.width
            Layout.alignment: Qt.AlignRight

            Button {
                text: "Save"
                highlighted: true

                onClicked: {
                    // Handle save action here
                    console.log("Save profile clicked")
                }
            }
        }
    }
}
